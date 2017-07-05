
let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = Elf_header.parse_class_endianness filename in
	let eclass_conf = match eclass with
	  | Elf_types.C32 -> Archi.eclass_32_conf
	  | Elf_types.C64 -> Archi.eclass_64_conf
	in

	(* file header *)
	let fh = Elf_header.parse_elf_header eclass_conf edata filename in
	Format.printf "ELF Header:\n%a" Print.elf_header fh;

	(* section header *)
	let shl = Elf_header.parse_section_header edata eclass_conf fh filename in
	Format.printf "\nSection Headers:\n";
	Format.printf
	  "  [Nr] Name              Type            Addr     Off    Size   ES \
Flg Lk Inf Al\n";
	List.iteri (fun i x -> Format.printf "  [%2i] %a" i Print.sh_entry x) shl;
	Format.printf "\
Key to Flags:\n\
\  W (write), A (alloc), X (execute), M (merge), S (strings)\n\
\  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)\n\
\  O (extra OS processing required) o (OS specific), p (processor specific)\n";

	(* program header *)
	let phl = Elf_header.parse_program_header edata eclass_conf fh filename in
	Format.printf "\nProgram Headers:\n";
	Format.printf "  Type           Offset   VirtAddr   PhysAddr   FileSiz \
MemSiz  Flg Align\n";
	List.iter (fun x -> Format.printf "%a" Print.ph_entry x) phl;
	
	(* dynamic symbol table *)
	begin
	  try
	    let dstrtab =
	      Elf_header.strtab ~filename ~tablename:".dynstr" shl edata in
	    let dsymtab =
	      Elf_header.parse_symtbl
		~filename ~tablename:".dynsym" ~strtab:dstrtab
		shl edata eclass_conf
	    in
	    Format.printf "\nSymbol table '.dynsym' contains %i entries:\n"
	      (List.length dsymtab);
	    Format.printf
	      "   Num:    Value  Size Type    Bind   Vis      Ndx Name\n";
	    List.iteri
	      (fun i x -> Format.printf "%6i: %a" i Print.symtbl_entry x)
	      dsymtab
	  with Not_found -> ()
	end;

	(* symbol table *)
	let strtab =
	  Elf_header.strtab ~filename ~tablename:".strtab" shl edata in
	let symtab =
	  Elf_header.parse_symtbl ~filename ~tablename:".symtab" ~strtab
	    shl edata eclass_conf in
	Format.printf "\nSymbol table '.symtab' contains %i entries:\n"
	  (List.length symtab);
	Format.printf
	  "   Num:    Value  Size Type    Bind   Vis      Ndx Name\n";
	List.iteri
	  (fun i x -> Format.printf "%6i: %a" i Print.symtbl_entry x)
	  symtab;

	(* instructions *)
	begin
	  try
            let instrs =
	      Elf_header.decode_instr
		~filename ~secname:".text" fh eclass_conf shl symtab
	    in
	    if instrs <> [] then
	      begin
		Format.printf "\nInstructions:\n";
		List.iter
		  (fun i -> Format.printf "%a" Print.instr i) instrs
	      end
	  with Not_found -> ()
	end
      with
	Elf_header.Invalid_Elf -> Format.printf "invalid ELF file !\n"
    else
      Format.printf "%s does not exist !\n" filename
  else
    Format.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
