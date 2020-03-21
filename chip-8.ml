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
   only if they are non-zero. They are used for timers and sounds *)
let sound_timer = ref 0
let delay_timer = ref 0
(* Then we declare the I register which is mainly used for 
   addresses. *)
let i = ref 0
(* Finally we declare the stack as an array of 16 values *)
let stack = Array.make 16 0
(* Magic numbers *)
let prog_start = 0x200

(* The standard ocaml ** function has type float -> float -> float
   which causes problems because we need for the binary to int
   conversion, so we declare a new one with type int -> int 
   -> int. *)

let rec ( *** ) a b =
  match b with
    0 -> 1
  | 1 -> a
  | _ -> a * (a *** (b-1))

(* Here we declare a function to get 4 bits of an integer which
   coresponds to a hexadecimal digit and in order to decode
   opcodes, we need to analyze each hex digit. *)

let rec get_4bits ?rel_pos:(rel_pos=0) position bits = 
  (* Here we substract 4 times the position to 12 because it 
     allows the position 0 to be the left digit and 3 the right *)
  let bit = (bits asr (12 - position * 4 + rel_pos)) land 1 in
  match rel_pos with 
    4 -> 0
  | _ ->
    bit * (2 *** rel_pos) + get_4bits position bits ~rel_pos:(rel_pos + 1)
 
let () =
  pc := prog_start;
  let test = get_4bits 3 ram.(!pc) in 
  print_int test
