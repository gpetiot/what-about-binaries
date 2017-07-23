
type etype = TNone | Relocatable | Executable | Shared | Core | Loproc | Hiproc;;

type eclass = C32 | C64;;

type eclass_conf = {
  eclass : eclass;
  addr_size : int;
  header_size : int;
  modes : Capstone.mode list;
  (* program header field sizes *)
  type_size : int;
  offset_size : int;
  vaddr_size : int;
  paddr_size : int;
  filesz_size : int;
  memsz_size : int;
  flags_size : int;
  align_size : int;
  (* program header field offsets *)
  type_offset : int;
  offset_offset : int;
  vaddr_offset : int;
  paddr_offset : int;
  filesz_offset : int;
  memsz_offset : int;
  flags_offset : int;
  align_offset : int;
  (* symbol field sizes *)
  name_size : int;
  value_size : int;
  size_size : int;
  info_size : int;
  other_size : int;
  shndex_size : int;
  (* symbol field offsets *)
  name_offset : int;
  info_offset : int;
  other_offset : int;
  shndex_offset : int;
  value_offset : int;
  size_offset : int;
};;

type emachine =
  | MNone
  | SPARC
  | X86
  | MIPS
  | PowerPC
  | ARM
  | SuperH
  | IA64
  | X86_64
  | AArch64
;;

type capstone_instr = Capstone.cs_insn0;;

type endianness = LittleEndian | BigEndian;;

type sym_visibility = Default | Hidden
type sym_ndx = Abs | Und | Int of int
type sym_binding = Local | Global | Weak
type sym_type = Notype | Object | Func | Section | File

type ph_type =
  | Null
  | Load
  | Dynamic
  | Interp
  | Note
  | Shlib
  | Phdr
  | Tls
  | Loos (* or Amdgpu_hsa_load_global_program *)
  | Amdgpu_hsa_load_global_agent
  | Amdgpu_hsa_load_readonly_agent
  | Amdgpu_hsa_load_code_agent
  | Sunw_unwind
  | Gnu_eh_frame (* or Sunw_eh_frame *)
  | Gnu_stack
  | Gnu_relro
  | Hios
  | Loproc (* or Arm_archext or Mips_reginfo *)
  | Arm_exidx (* or Arm_unwind or Mips_rtproc *)
  | Mips_options
  | Mips_abiflags
  | Hiproc
;;
  
type sh_type =
  | Null
  | Progbits
  | Symtab
  | Strtab
  | Rela
  | Hash
  | Dynamic
  | Note
  | Nobits
  | Rel
  | Shlib
  | Dynsym
  | Init_array
  | Fini_array
  | Preinit_array
  | Group
  | Symtab_shndx
  | Loos
  | Gnu_attributes
  | Gnu_hash
  | Gnu_verdef
  | Gnu_verneed
  | Gnu_versym (* or Hios *)
  | Loproc (* or Hex_ordered *)
  | Arm_exidx (* or X86_64_unwind *)
  | Arm_preemptmap
  | Arm_attributes
  | Arm_debugoverlay
  | Arm_overlaysection
  | Mips_reginfo
  | Mips_options
  | Mips_abiflags
  | Hiproc
  | Louser
  | Hiuser
;;

type elf_header = {
  ei_class : eclass;
  ei_endian : endianness;
  ei_version : int;
  ei_osabi : int;
  ei_abiversion : int;
  ei_type : etype;
  ei_machine : emachine;
  ei_entry : int;
  ei_phoff : int;
  ei_shoff : int;
  ei_flags : int;
  ei_ehsize : int;
  ei_phentsize : int;
  ei_phnum : int;
  ei_shentsize : int;
  ei_shnum : int;
  ei_shstrndx : int;
};;

type ph_entry = {
  ph_type : ph_type;
  ph_off : int;
  ph_vaddr : int;
  ph_addr : int;
  ph_filesz : int;
  ph_memsz : int;
  ph_flags : int;
  ph_align : int;
};;

type sh_entry = {
  sh_name : string;
  sh_type : sh_type;
  sh_flags : int;
  sh_addr : int;
  sh_off : int;
  sh_size : int;
  sh_link : int;
  sh_info : int;
  sh_addralign : int;
  sh_entsize : int;
};;

type symtbl_entry = {
  value : int;
  size : int;
  symtype : sym_type;
  bind : sym_binding;
  vis : sym_visibility;
  ndx : sym_ndx;
  name : string;
};;

type funct = symtbl_entry * capstone_instr list;;

type op =
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Xor
;;

type reg =
  | Eax
  | Edx
  | Rbp (* base pointer, which points to the base of the current stack frame *)
  | Rsp (* stack pointer, which points to the top of the current stack frame *)
  | Rip (* instruction pointer, which points to the next instruction *)

type mov_value =
  | Reg of reg
  | RegOff of reg * int
  | Addr of int
  | Value of int
;;

type instr =
  | Nop      of int (* @ *)
  | Push     of int (* @ *) * reg
  | Pop      of int (* @ *) * reg
  | Call     of int (* @ *) * int (* @ callee *)
  | Op       of int (* @ *) * op * reg * reg
  | Mov      of int (* @ *) * reg * mov_value
  | Ret      of int (* @ *)
;;
