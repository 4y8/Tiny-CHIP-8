(* First we declare the memory as arrays *)
(*   The RAM is a 4096 byte array (4K)   *)
let ram = Array.make 4096 0x00
(* There are 16 registers *)
let regs = Array.make 16 0
(* Plus some special registers *)
(* The program counter *)
let pc = ref 0
(*  The stack pointer  *)
let sp = ref 0
(* Plus two timers which get decreased every at a rate of 60Hz,
 * only if they are non-zero. They are used for timers and sounds *)
let sound_timer = ref 0
let delay_timer = ref 0
(* Then we declare the I register which is mainly used for 
   addresses. *)
let i = ref 0
(* Finally we declare the stack as an array of 16 values *)
let stack = Array.make 16 0
(* Magic numbers *)
let prog_start  = 0x200
let graph_start = 0xF00

(* Define error messages *)
exception Unknown_opcode
exception Internal_error

(* The standard ocaml ** function has type float -> float -> float
 * which causes problems because we need for the binary to int
 * conversion, so we declare a new one with type int -> int
 * -> int. *)

let rec ( *** ) a b =
  match b with
    0 -> 1
  | 1 -> a
  | _ -> a * (a *** (b-1))

(* Create a function like the "+=" of C to avoid code
 * redundancy. *)

let ( += ) a b =
  a := !a + b

(* Here we declare a function to get 4 bits of an integer which
 * coresponds to a hexadecimal digit and in order to decode
 * opcodes, we need to analyze each hex digit. *)

let rec get_4bits ?rel_pos:(rel_pos=0) position bits = 
  (* Here we substract 4 times the position to 12 because it 
     allows the position 0 to be the left digit and 3 the right *)
  let bit = (bits asr (12 - position * 4 + rel_pos)) land 1 in
  match rel_pos with 
    4 -> 0
  | _ ->
    bit * (2 *** rel_pos) + get_4bits position bits ~rel_pos:(rel_pos + 1)

(* Addresses in opcodes are always the 12 last bits, so this functions
 * get them and return a valid int representing the address *)
let get_addr bits =
  256 * get_4bits 1 bits + 16 * get_4bits 2 bits + get_4bits 3 bits

(* Some opcodes relie on the last byte to handle values, so this
 * function gets it and return its value. *)

let get_byte bits =
  16 * get_4bits 2 bits + get_4bits 3 bits

let graph_init =
  Graphics.open_graph " 640x320 ";
  Graphics.set_window_title "chip-8"

let rec draw_pixel ?rel_pos:(rel_pos=0) pixel x y =
  match rel_pos with
    8 -> ()
  | _ ->
    let pix = (pixel asr (7-rel_pos)) land 1 in
    begin
      match pix with
        0 ->
        Graphics.set_color Graphics.black;
      | 1 ->
        Graphics.set_color Graphics.white;
      | _ -> raise Internal_error
    end;
    Graphics.fill_rect ((x + rel_pos) * 10) (y * 10) 10 10;
    draw_pixel ~rel_pos:(rel_pos + 1) pixel x y

let rec draw ?rel_y:(rel_y=0) display =
  match rel_y with
    32 -> ()
  | _  ->
    let draw_relpix index pixel =
      draw_pixel pixel (index * 7) rel_y
    in
    Array.iteri draw_relpix (Array.sub display 0 8);
    draw ~rel_y: (rel_y + 1)
      (Array.sub display 8 (256 - 8 * (rel_y + 1)))

let rec draw_sprite x y bytes =
  match x mod 8 with
    0 ->
    let fx = x mod 64 in
    let place y2 byte =
      let place = graph_start + fx / 8 + 8 * y2 + y * 8 in
      ram.(place) <- byte lxor ram.(place) (* Tha sprite is "xored" on
                                            * the screen. *)
    in
    Array.iteri place bytes
  | m ->
    let shift a = a asr m               in
    let shift1  = Array.map shift bytes in
    draw_sprite (8 * (x / 8)) y shift1;
    let rec first_bits ?rel:(rel=0) byte =
      if m = rel then 0
      else
        (byte land 1) *** rel +
        first_bits (byte asr 1) ~rel:(rel + 1)
    in
    let shift_left byte =
      (first_bits byte) lsl (8 - m)
    in
    let shift2 = Array.map shift_left bytes in
    draw_sprite (8 * (x / 8) + 8) y shift2

(* Decode a 2-bytes opcode and execute its content. *)
let decode_opcode opcode =
  let digit0 = get_4bits 0 opcode in
  (* Find the instruction that have to be executed by using
   * the first 4 bits *)
  (* Gat all the different values that may be used by the opcodes *)
  let kk   = get_byte opcode    in
  let x    = get_4bits 1 opcode in
  let y    = get_4bits 2 opcode in
  let n    = get_4bits 3 opcode in
  let addr = get_addr opcode    in
  match digit0 with
    0x0 ->
    begin
      match opcode with
      (* 00E0 : Clean the screen by setting all the bytes reserved bytes
       * for the graphics to zero. *)
        0x00E0 ->
        Array.fill ram graph_start 256 0
      (* 00EE : Return from a subroutine by setting the program counter
       * to the value on the top of the stack and decrementing the
       * stack pointer. *)
      | 0x00EE ->
        pc := stack.(!sp);
        sp := !sp - 1     (* Decrements the stack pointer *)
      | _ -> raise Unknown_opcode
    end
  | 0x1 ->
    (* 1NNN : Jump to the address NNN, defined by the 12 last bits, by
     * setting the program counter to it. *)
    pc := addr
  | 0x2 ->
    (* 2NNN : Call the subroutine at the adress NNN, defined by the 12
     * last bits, by saving the program counter on top of the stack and
     *  setting the program counter to the adress.*)
    sp += 1;            (*         Increments the stack pointer         *)
    stack.(!sp) <- !pc; (* Save the program counter on top of the stack *)
    pc := addr          (*   Set the program counter to its new value   *)
  | 0x3 ->
    (* 3XKK : Skip the next instruction if the value in the register number X
     * is equal to KK by comparing them and adding 2 to them program counter
     * if needed *)
    if kk = regs.(x) then pc += 2
  | 0x4 ->
    (* 4XKK : Skip the next instruction if the value in the register number X
     * is not nequal to KK by comparing them and adding 2 to them program counter
     * if needed *)
    if kk <> regs.(x) then pc += 2
  | 0x5 ->
    begin
      match n with
        0 ->
        (* 5XY0 : Skip the next instruction if the values in the registers X and
         * Y are equal by comparing them and adding 2 to the program counter if
         * needed. *)
        if x = y then pc += 2
      | _ -> raise Unknown_opcode
    end
  | 0x6 ->
    (* 6XKK : puts the value KK into the register X. *)
    regs.(x) <- kk
  | 0x7 ->
    (* 7XKK : Increments the register X by the value KK *)
    regs.(x) <- kk + regs.(x) (* We can't use the function += because it is an
                               * array and not a mutable variable. *)
  | 0x8 ->
    begin
      match n with
      (* 8XY0 : stores the value of the register Y in the register X. *)
        0 -> regs.(x) <- regs.(y)
      (* 8XY1 : stores in the register X the bitwise or of X and Y. *)
      | 1 -> regs.(x) <- regs.(y) lor regs.(x)
      (* 8XY2 : stores in the register X the bitwise and of X and Y. *)
      | 2 -> regs.(x) <- regs.(y) land regs.(x)
      (* 8XY3 : stores in the register X the bitwise xor of X and Y. *)
      | 3 -> regs.(x) <- regs.(y) lxor regs.(x)
      (* 8XY4 : stores in the register X the sum of the registers X and
       * Y and set the register F to 1 if the sum overflows. *)
      | 4 ->
        let sum = regs.(x) + regs.(y) in
        if sum <= 255 then regs.(x) <- sum
        else
          begin
            regs.(0xF) <- 1;
            regs.(x)   <- sum - 256 (* Here we simulate an add overflow. *)
          end
      (* 8XY5 : stores in the register X the difference between the registers
       * X and Y and set the register F to 1 if there isn't a quarry. *)
      | 5 ->
        regs.(x) <- regs.(x) - regs.(y);
        if regs.(x) > regs.(y) then
            regs.(0xF) <- 1
        else regs.(x) <- regs.(x) + 256 (* We can't have a negative number
                                         *  in the register*)
      | 6 ->
        (* 8XY6 : set the last bit of X in the register F and perform a
         * bitwise right shift to the register X *)
        regs.(0xF) <- regs.(x) mod 2;
        regs.(x)   <- regs.(x) asr 1
      | 7 ->
        regs.(x) <- regs.(y) - regs.(x);
        if regs.(x) < regs.(y) then
          regs.(0xF) <- 1
        else regs.(x) <- regs.(x) + 256
      | 0xE ->
        regs.(0xF) <- (regs.(x) asr 7) land 1;
        regs.(x)   <- regs.(x) asr 1
      | _ -> raise Unknown_opcode
    end
  | 0x9 ->
    begin
      match n with
        0 ->
        (* 9XY0 : Skip the next instruction if the values in the registers X and
         * Y are not equal by comparing them and adding 2 to the program counter if
         * needed. *)
        if x <> y then pc += 2
      | _ -> raise Unknown_opcode
    end
  | 0xA ->
    (* ANNN : set the register I to the value NNN. *)
    i := addr
  | 0xB ->
    (* BNNN : jump to the address NNN plus the value of the 0th register by
     * setting the program counter to it. *)
    pc := addr + regs.(0)
  | 0xC ->
    (* CXKK : Generate a random number, bitwise and it and puts the result
     * in the X register. *)
    regs.(x) <- kk land (Random.int 256)
  | 0xD ->
    (* DXYN : display a sprite of n-byte at the memory location in the register
     * I at the coordinates X,Y. *)
    ()
  | 0xE -> ()
  | 0xF -> ()
  | _   -> raise Unknown_opcode

let () =
  (* An opcode needs a random number, so we initiate the generator. *)
  Random.self_init();
  graph_init;
  pc := prog_start;
  draw_sprite 1 1 [|0xFF|];
  draw_sprite 0 0 [|0xFF|];
  draw (Array.sub ram graph_start 256);
  while true do print_string "" done
