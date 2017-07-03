
open Printf
open Capstone
open X86
open X86_const

let print_string_hex comment str =
  printf "%s" comment;
  for i = 0 to (Array.length str - 1) do
    printf "0x%02x " str.(i)
  done;
  printf "\n"
;;

let print_op handle i op =
  ( match op.value with
  | X86_OP_INVALID _ -> ();	(* this would never happens *)
  | X86_OP_REG reg ->
     printf "\t\top[%d]: REG = %s\n" i (cs_reg_name handle reg);
  | X86_OP_IMM imm -> printf "\t\top[%d]: IMM = 0x%x\n" i imm;
  | X86_OP_FP fp -> printf "\t\top[%d]: FP = %f\n" i fp;
  | X86_OP_MEM mem ->
     ( printf "\t\top[%d]: MEM\n" i;
       if mem.base != 0 then
	 printf "\t\t\toperands[%u].mem.base: REG = %s\n" i (cs_reg_name handle  mem.base);
       if mem.index != 0 then
	 printf "\t\t\toperands[%u].mem.index: REG = %s\n" i (cs_reg_name handle mem.index);
       if mem.scale != 1 then
	 printf "\t\t\toperands[%u].mem.scale: %d\n" i mem.scale;
       if mem.disp != 0 then
	 printf "\t\t\toperands[%u].mem.disp: 0x%x\n" i mem.disp;
     );
  );
  ()
;;

let print_detail handle mode insn =
  match insn.arch with
  | CS_INFO_X86 x86 -> (
    print_string_hex "\tPrefix: " x86.prefix;

    (* print instruction's opcode *)
    print_string_hex "\tOpcode: " x86.opcode;

    (* print operand's size, address size, displacement size & immediate size *)
    printf "\taddr_size: %u\n" x86.addr_size;

    (* print modRM byte *)
    printf "\tmodrm: 0x%x\n" x86.modrm;

    (* print displacement value *)
    if x86.disp != 0 then
      printf "\tdisp: 0x%x\n" x86.disp;

    (* SIB is invalid in 16-bit mode *)
    if not (List.mem CS_MODE_16 mode) then (
      (* print SIB byte *)
      printf "\tsib: 0x%x\n" x86.sib;

      (* print sib index/scale/base (if applicable) *)
      if x86.sib_index != _X86_REG_INVALID then
	printf "\tsib_index: %s, sib_scale: %u, sib_base: %s\n"
	  (cs_reg_name handle x86.sib_index)
	  x86.sib_scale
	  (cs_reg_name handle x86.sib_base);
    );

    (* print all operands info (type & value) *)
    if (Array.length x86.operands) > 0 then (
      printf "\top_count: %d\n" (Array.length x86.operands);
      Array.iteri (print_op handle) x86.operands;
    );
    printf "\n";
  );
  | _ -> ();
;;

let print_insn handle mode insn =
  printf "0x%x\t%s\t%s\n" insn.address insn.mnemonic insn.op_str;
  (*print_detail handle mode insn*)
;;

let print_arch arch mode code syntax =
  let handle = cs_open arch mode in (
    if syntax != 0L then (
      let err = cs_option handle CS_OPT_SYNTAX syntax in
      match err with
      | _ -> ();
    );
    let err = cs_option handle CS_OPT_DETAIL _CS_OPT_ON in
    match err with
    | _ -> ();
      let insns = cs_disasm handle code 0x1000L 0L in (
	printf "*************\n";
	List.iter (print_insn handle mode) insns;
      );
      match cs_close handle with
      | 0 -> ();
      | _ -> printf "Failed to close handle";
  )
;;

let decode modes str =
  let handle = Capstone.cs_open Capstone.CS_ARCH_X86 modes in
  let _=
    Capstone.cs_option handle Capstone.CS_OPT_DETAIL Capstone._CS_OPT_ON in
  let instr = Capstone.cs_disasm handle str 0x1000L 0L in
  (match Capstone.cs_close handle with
  | 0 -> ()
  | _ -> failwith "Failed to close handle");
  instr
;;
