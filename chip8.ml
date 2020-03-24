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
(* Here we define the keyboard layout *)
let layout = "v\"'(ertdfgcb-yhn"
let key_pressed = ref 0
(* The interpreter has to have a simple font built into its
 * ram *)
let font =
  [|
    0xF0; 0x90; 0x90; 0x90; 0xF0; (* 0 *)
    0x20; 0x60; 0x20; 0x20; 0x70; (* 1 *)
    0xF0; 0x10; 0xF0; 0x80; 0xF0; (* 2 *)
    0xF0; 0x10; 0xF0; 0x10; 0xF0; (* 3 *)
    0x90; 0x90; 0xF0; 0x10; 0x10; (* 4 *)
    0xF0; 0x80; 0xF0; 0x10; 0xF0; (* 5 *)
    0xF0; 0x80; 0xF0; 0x90; 0xF0; (* 6 *)
    0xF0; 0x10; 0x20; 0x40; 0x40; (* 7 *)
    0xF0; 0x90; 0xF0; 0x90; 0xF0; (* 8 *)
    0xF0; 0x90; 0xF0; 0x10; 0xF0; (* 9 *)
    0xF0; 0x90; 0xF0; 0x90; 0x90; (* A *)
    0xE0; 0x90; 0xE0; 0x90; 0xE0; (* B *)
    0xF0; 0x80; 0x80; 0x80; 0xF0; (* C *)
    0xE0; 0x90; 0x90; 0x90; 0xE0; (* D *)
    0xF0; 0x80; 0xF0; 0x80; 0xF0; (* E *)
    0xF0; 0x80; 0xF0; 0x80; 0x80  (* F *)
  |]

let last_time = ref 0.

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

let ( += ) a b = a := !a + b

(* Some opcodes relie on the last byte to handle values, so this
 * function gets it and return its value. *)

let int_of_bool bool =
  match bool with
    true  -> 1
  | false -> 0

let init () =
  (* Creates the window *)
  Graphics.open_graph " 640x320 ";
  Graphics.set_window_title "chip-8";
  (* Puts the font in its position. The font is on the beginning of the
   * ram and 80 is the number of elemnts in the font list. *)
  Array.blit font 0 ram 0 80;
  pc := prog_start;
  (* An opcode needs a random number, so we initiate the generator. *)
  Random.self_init();
  let ic = open_in Sys.argv.(1) in
  let rec read_chan chan pos =
    try
      ram.(prog_start + pos) <- Char.code (input_char ic);
      read_chan chan (pos + 1)
    with
      End_of_file -> close_in chan
  in
  read_chan ic 0;
  last_time := Unix.gettimeofday()

let rec draw_pixel ?rel_pos:(rel_pos=0) pixel x y =
  match rel_pos with
    8 -> ()
  | _ ->
    let pix = (pixel asr (7 - rel_pos)) land 1 in
    begin
      match pix with
        0 -> Graphics.set_color Graphics.black;
      | 1 -> Graphics.set_color Graphics.white;
      | _ -> raise Internal_error
    end;
    Graphics.fill_rect ((x + rel_pos) * 10) (310 - y * 10) 10 10;
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
  let fx = x mod 64 + ((int_of_bool (x < 0)) * 32) in
  let fy = y mod 32 + ((int_of_bool (y < 0)) * 32) in
  match x mod 8 with
    0 ->
    regs.(0xF) <- 0;
    let place y2 byte =
      (* Calculate the memory emplacement of the sprite. *)
      let place = graph_start + fx / 8 + ((fy + y2) mod 32) * 8 in
      let save  = ram.(place) in
      ram.(place) <- byte lxor ram.(place); (* The sprite is "xored" on
                                             * the screen. *)
      if regs.(0xF) = 0 then
        regs.(0xF) <- int_of_bool(save > (save land ram.(place)))
    in
    Array.iteri place bytes
  | m ->
    let shift a = a asr (m - int_of_bool(m >= 4) * 1) in
    let shift1  = Array.map shift bytes in
    draw_sprite (8 * (fx / 8)) fy shift1;
    let sv = regs.(0xF) in
    let rec first_bits ?rel:(rel=0) byte w =
      if m = rel then 0
      else
        (byte land 1) * 2 *** rel + first_bits (byte asr 1) w ~rel:(rel + 1)
    in
    let shift_left byte = (first_bits byte m) lsl (8 - m) in
    let shift2 = Array.map shift_left bytes in
    draw_sprite (8 * (fx / 8) + 8) fy shift2;
    regs.(0xF) <- sv lor regs.(0xF)

(* Decode a 2-bytes opcode and execute its content. *)
let decode_opcode opcode =
  (* Find the instruction that have to be executed by using
   * the first 4 bits *)
  let op   = (opcode land 0xF000) asr 12 in
  (* Gat all the different values that may be used by the opcodes *)
  let kk   = opcode land 0x00FF in
  let x    = (opcode land 0x0F00) asr 8 in
  let y    = (opcode land 0x00F0) asr 4 in
  let n    = opcode land 0x000F in
  let addr = opcode land 0x0FFF in
  match op with
    0x0 ->
    begin
      match opcode with
      (* 00E0 : Clean the screen by setting all the bytes reserved bytes
       * for the graphics to zero. *)
        0x00E0 ->
        Array.fill ram graph_start 256 0;
        draw (Array.sub ram graph_start 256 )
      (* 00EE : Return from a subroutine by setting the program counter
       * to the value on the top of the stack and decrementing the
       * stack pointer. *)
      | 0x00EE ->
        pc := stack.(!sp) - 2;
        sp := !sp - 1
      | _ -> raise Unknown_opcode
    end
  (* 1NNN : Jump to the address NNN, defined by the 12 last bits, by
   * setting the program counter to it. *)
  | 0x1 -> pc := addr - 2
  (* 2NNN : Call the subroutine at the adress NNN, defined by the 12
   * last bits, by saving the program counter on top of the stack and
   *  setting the program counter to the adress.*)
  | 0x2 ->
    sp += 1;
    stack.(!sp) <- !pc + 2;
    pc := addr - 2
  (* 3XKK : Skip the next instruction if the value in the register number X
   * is equal to KK by comparing them and adding 2 to them program counter
   * if needed *)
  | 0x3 -> pc += int_of_bool(kk = regs.(x)) * 2
  (* 4XKK : Skip the next instruction if the value in the register number X
   * is not nequal to KK by comparing them and adding 2 to them program counter
   * if needed *)
  | 0x4 -> pc += int_of_bool(kk <> regs.(x)) * 2
  | 0x5 ->
    begin
      match n with
      (* 5XY0 : Skip the next instruction if the values in the registers X and
       * Y are equal by comparing them and adding 2 to the program counter if
       * needed. *)
        0 -> pc += 2 * int_of_bool (regs.(x) = regs.(y))
      | _ -> raise Unknown_opcode
    end
    (* 6XKK : puts the value KK into the register X. *)
  | 0x6 -> regs.(x) <- kk
    (* 7XKK : Increments the register X by the value KK *)
  | 0x7 -> regs.(x) <- (kk + regs.(x)) mod 256;
  | 0x8 ->
    begin
      match n with
      (* 8XY0 : stores the value of the register Y in the register X. *)
        0 -> regs.(x) <- regs.(y)
      (* 8XY1 : stores in the register X the bitwise or of X and Y. *)
      | 1 -> regs.(x) <- regs.(x) lor regs.(y)
      (* 8XY2 : stores in the register X the bitwise and of X and Y. *)
      | 2 -> regs.(x) <- regs.(y) land regs.(x)
      (* 8XY3 : stores in the register X the bitwise xor of X and Y. *)
      | 3 -> regs.(x) <- regs.(y) lxor regs.(x)
      (* 8XY4 : stores in the register X the sum of the registers X and
       * Y and set the register F to 1 if the sum overflows. *)
      | 4 ->
        let sum = regs.(x) + regs.(y) in
        regs.(0xF) <- int_of_bool(sum > 255);
        regs.(x)   <- sum mod 256; (* Here we simulate an add overflow. *)
      (* 8XY5 : stores in the register X the difference between the registers
       * X and Y and set the register F to 1 if there isn't a quarry. *)
      | 5 ->
        regs.(0xF) <- int_of_bool (regs.(x) >= regs.(y));
        regs.(x) <- regs.(x) - regs.(y) - 256 * (regs.(0xF) - 1);
       (* We can't have a negative number in the register*)
      | 6 ->
        (* 8XY6 : set the last bit of X in the register F and perform a
         * bitwise right shift to the register X *)
        regs.(0xF) <- regs.(x) land 1; (* Get the last significant bit. *)
        regs.(x)   <- regs.(x) asr 1
      | 7 ->
        regs.(0xF) <- int_of_bool (regs.(x) <= regs.(y));
        regs.(x)   <- regs.(y) - regs.(x) - 256 * (regs.(0xF) - 1);
      | 0xE ->
        regs.(0xF) <- (regs.(x) asr 7) land 1; (* Get the most significant bit. *)
        regs.(x)   <- (regs.(x) lsl 1) land 0xFF
      | _ -> raise Unknown_opcode
    end
  | 0x9 ->
    begin
      match n with
        0 ->
        (* 9XY0 : Skip the next instruction if the values in the registers X and
         * Y are not equal by comparing them and adding 2 to the program counter if
         * needed. *)
        pc += int_of_bool(regs.(y) <> regs.(x)) * 2
      | _ -> raise Unknown_opcode
    end
  (* ANNN : set the register I to the value NNN. *)
  | 0xA -> i := addr
  (* BNNN : jump to the address NNN plus the value of the 0th register by
   * setting the program counter to it. *)
  | 0xB -> pc := addr + regs.(0) - 2
  (* CXKK : Generate a random number, bitwise and it and puts the result
   * in the X register. *)
  | 0xC -> regs.(x) <- kk land (Random.int 256)
  (* DXYN : display a sprite of n-byte at the memory location in the register
   * I at the coordinates in the registers X and Y. *)
  | 0xD ->
    draw_sprite regs.(x) regs.(y) (Array.sub ram !i n);
    draw (Array.sub ram graph_start 256)
  | 0xE ->
    begin
      match kk with
      (* EX9E : skips the next instruction if the key pressed has the
       * same value as the one in the register X. *)
        0x9E -> pc += 2 * int_of_bool (!key_pressed = regs.(x))
      (* EXA1 : skips the next instruction if the key pressed has not
       * the same value as the one in the register X. *)
      | 0xA1 -> pc += 2 * int_of_bool (!key_pressed <> regs.(x))
      | _ -> raise Unknown_opcode
    end
  | 0xF ->
    begin
      match kk with
      (* FX07 : set the value of the delay timer in the register X. *)
        0x07 -> regs.(x) <- !delay_timer
      (* FX0A : wait for a keypress and put its result in the X register. *)
      | 0x0A ->
        let rec wait_for_key () =
          (* Check if the key pressed is in the layout. *)
          match String.index_opt layout (Graphics.read_key()) with
            None     -> wait_for_key () (* If it's not try another time. *)
          | Some key -> key             (* Else return the key. *)
        in
        regs.(x) <- wait_for_key()
      (* FX15 : Set the value of the delay timer to the value of the
       * register X. *)
      | 0x15 -> delay_timer := regs.(x)
      (* FX18 : Set the value of the sound timer to the value of the
       * register X. *)
      | 0x18 -> sound_timer := regs.(x);
      (* FX1E : increment the value of I by the one of the register X. *)
      | 0x1E -> i += regs.(x)
      (* FX29 : set the value of I to the location of the sprite corresponding
       * to the register X. *)
      | 0x29 -> i := regs.(x) * 5
      (* FX33 : put in the address i, i+1, i+2 the decimal digits of the values
       * in the register X. *)
      | 0x33 ->
        ram.(!i)     <- regs.(x) / 100;
        ram.(!i + 1) <- regs.(x) mod 100 / 10;
        ram.(!i + 2) <- regs.(x) mod 10
      (* FX55 : save the registers from 0 to X in the ram starting at the
       * address strored in i. *)
      | 0x55 ->
        let rec save_reg registers pos =
          match registers with
            []       -> ()
          | hd :: tl ->
            ram.(!i + pos) <- hd;
            save_reg tl (pos + 1)
        in
        save_reg (Array.to_list (Array.sub regs 0 (x + 1))) 0
      (* FX65 : read the registers from 0 to X from the ram starting at the
       * address strored in i. *)
      | 0x65 ->
        let rec read_reg registers pos =
          match registers with
            []       -> ()
          | hd :: tl ->
            regs.(pos) <- hd;
            read_reg tl (pos + 1)
        in
        read_reg (Array.to_list (Array.sub ram !i (x + 1))) 0
      | _ -> raise Unknown_opcode
    end
  | _   -> raise Unknown_opcode

let cycle() =
  if (Unix.gettimeofday() -. !last_time) >= 1./.60. then
    begin
      last_time   := Unix.gettimeofday();
      if !sound_timer > 0 then
        begin
          delay_timer += -1;
          Graphics.sound 523 10
        end;
      delay_timer += - int_of_bool (!delay_timer > 0);
      key_pressed :=
        match Graphics.key_pressed() with
          false -> -1
        | true  ->
            match String.index_opt layout (Graphics.read_key()) with
              None -> -1
            | Some x -> x
    end;
  decode_opcode ((ram.(!pc) lsl 8) lor ram.(!pc + 1));
  pc += 2

let rec game () =
  cycle();
  game()

let _ =
  init();
  game()
