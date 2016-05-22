
open Printf
open Capstone
open Arm
open Arm_const

let print_string_hex comment str =
  printf "%s" comment;
  for i = 0 to (Array.length str - 1) do
    printf "0x%02x " str.(i)
  done;
  printf "\n"
;;

let print_op handle i op =
  ( match op.value with
  | ARM_OP_INVALID _ -> ();	(* this would never happens *)
  | ARM_OP_REG reg ->
     printf "\t\top[%d]: REG = %s\n" i (cs_reg_name handle reg);
  | ARM_OP_CIMM imm -> printf "\t\top[%d]: C-IMM = %u\n" i imm;
  | ARM_OP_PIMM imm -> printf "\t\top[%d]: P-IMM = %u\n" i imm;
  | ARM_OP_IMM imm -> printf "\t\top[%d]: IMM = 0x%x\n" i imm;
  | ARM_OP_FP fp -> printf "\t\top[%d]: FP = %f\n" i fp;
  | ARM_OP_MEM mem ->
     ( printf "\t\top[%d]: MEM\n" i;
       if mem.base != 0 then
	 printf "\t\t\toperands[%u].mem.base: REG = %s\n" i (cs_reg_name handle mem.base);
       if mem.index != 0 then
	 printf "\t\t\toperands[%u].mem.index: REG = %s\n" i (cs_reg_name handle mem.index);
       if mem.scale != 1 then
	 printf "\t\t\toperands[%u].mem.scale: %d\n" i mem.scale;
       if mem.disp != 0 then
	 printf "\t\t\toperands[%u].mem.disp: 0x%x\n" i mem.disp;
     );
  | ARM_OP_SETEND sd -> printf "\t\top[%d]: SETEND = %u\n" i sd;
  );
  if op.shift.shift_type != _ARM_SFT_INVALID && op.shift.shift_value > 0 then
    printf "\t\t\tShift: type = %u, value = %u\n"
      op.shift.shift_type op.shift.shift_value;
  ()
;;

let print_detail handle insn =
  match insn.arch with
  | CS_INFO_ARM arm -> (
    if arm.cc != _ARM_CC_AL && arm.cc != _ARM_CC_INVALID then
      printf "\tCode condition: %u\n" arm.cc;

    if arm.update_flags then
      printf "\tUpdate-flags: True\n";

    if arm.writeback then
      printf "\tWriteback: True\n";

      (* print all operands info (type & value) *)
    if (Array.length arm.operands) > 0 then (
      printf "\top_count: %d\n" (Array.length arm.operands);
      Array.iteri (print_op handle) arm.operands;
    );
    printf "\n";
  );
  | _ -> ();
;;

let print_insn handle insn =
  printf "0x%x\t%s\t%s\n" insn.address insn.mnemonic insn.op_str;
  (*print_detail handle insn*)
;;

let print_arch arch mode code =
  let handle = cs_open arch mode in
  let err = cs_option handle CS_OPT_DETAIL _CS_OPT_ON in
  match err with
  | _ -> ();
    let insns = cs_disasm handle code 0x1000L 0L in
    printf "*************\n";
    List.iter (print_insn handle) insns;
    match cs_close handle with
    | 0 -> ();
    | _ -> printf "Failed to close handle";
;;
