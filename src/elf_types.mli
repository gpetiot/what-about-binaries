
type etype = TNone | Relocatable | Executable | Shared | Core | Loproc | Hiproc;;

type eclass = C32 | C64;;

module type Addr =
sig
  val eclass : eclass
  val size : int
  val header_size : int
  val modes : Capstone.mode list
  (* program header field sizes *)
  val type_size : int
  val offset_size : int
  val vaddr_size : int
  val paddr_size : int
  val filesz_size : int
  val memsz_size : int
  val flags_size : int
  val align_size : int
  (* program header field offsets *)
  val type_offset : int
  val offset_offset : int
  val vaddr_offset : int
  val paddr_offset : int
  val filesz_offset : int
  val memsz_offset : int
  val flags_offset : int
  val align_offset : int
  (* symbol field sizes *)
  val name_size : int
  val value_size : int
  val size_size : int
  val info_size : int
  val other_size : int
  val shndex_size : int
  (* symbol field offsets *)
  val name_offset : int
  val info_offset : int
  val other_offset : int
  val shndex_offset : int
  val value_offset : int
  val size_offset : int
end;;

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

type instr = Capstone.cs_insn0;;

type endianness = LittleEndian | BigEndian;;

module type Endianness =
sig
  val edata : endianness
  val order : 'a list -> 'a list
end;;

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
