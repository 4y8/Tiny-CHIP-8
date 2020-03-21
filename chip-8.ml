(* First we declare the memory as arrays *)
(*   The RAM is a 4096 byte array (4K)   *)
let ram = Array.make 4096 0xFF 
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
      | 2 -> regs.(x) <- regs.(y) land regs.(x)
      | 3 -> regs.(x) <- regs.(y) lxor regs.(x)
      | 4 ->
        let sum = regs.(x) + regs.(y) in
        if sum <= 255 then regs.(x) <- sum
        else
          begin
            regs.(0xF) <- 1;
            regs.(x)   <- 255
          end
      | 5 ->
        regs.(x) <- regs.(x) - regs.(y);
        if regs.(x) > regs.(y) then
            regs.(0xF) <- 1
      | 6 ->
        regs.(0xF) <- regs.(x) mod 2;
        regs.(x)   <- regs.(x) asr 1
    end
  | 0x9 ->
  | 0xA ->
  | 0xB ->
  | 0xC ->
  | 0xD ->
  | 0xE ->
  | 0xF ->

let () =
  pc := prog_start;
  let test = get_4bits 3 ram.(!pc) in 
  print_int test
