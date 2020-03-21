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

let get_addr bits =
  256 * get_4bits 1 bits + 16 * get_4bits 2 bits + get_4bits 3 bits

let decode_opcode opcode =
  let digit0 = get_4bits 0 opcode in
  (* Find the instruction that have to be executed by using
   * the first 4 bits *)
  match digit0 with
    0x0 ->
    begin
      match opcode with
      (* Clean the screen by setting all the bytes reserved bytes
       * for the graphics to zero. *)
        0x00E0 ->
        Array.fill ram graph_start 256 0
      (* Return from a subroutine by setting the program counter
       * to the value on the top of the stack and decrementing the
       * stack pointer. *)
      | 0x00EE ->
        pc := stack.(!sp);
        sp := !sp - 1     (* Decrements the stack pointer *)
      | _ -> raise Unknown_opcode
    end
  | 0x1 ->
    (* Jump to the address defined by the 12 last bits by setting
     * the program counter to it. *)
    pc := get_addr opcode
  | 0x2 ->
    sp := !sp + 1 (* Increments the stack pointer *)
  | 0x3
  | 0x4
  | 0x5
  | 0x6
  | 0x7
  | 0x8
  | 0x9
  | 0xA
  | 0xB
  | 0xC
  | 0xD
  | 0xE
  | 0xF

let () =
  pc := prog_start;
  let test = get_4bits 3 ram.(!pc) in 
  print_int test
