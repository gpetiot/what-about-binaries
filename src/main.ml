
let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = Elf_header.parse_class_endianness filename in
	let eclass_conf = match eclass with
	  | Elf_types.C32 -> Eclass_conf.conf_32
	  | Elf_types.C64 -> Eclass_conf.conf_64
	in

	let fh = Elf_header.parse_elf_header eclass_conf edata filename in
	Format.printf "ELF Header:\n%a" Print.elf_header fh;

	let shl =
	  Elf_header.parse_section_header edata eclass_conf fh filename in
        Format.printf "%a" Print.sh shl;
	
	Format.printf "\
Key to Flags:\n\
\  W (write), A (alloc), X (execute), M (merge), S (strings)\n\
\  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)\n\
\  O (extra OS processing required) o (OS specific), p (processor specific)\n";

	let phl =
	  Elf_header.parse_program_header edata eclass_conf fh filename in
	Format.printf "%a" Print.ph phl;
        
	begin
	  try
	    let dstrtab =
	      Elf_header.strtab ~filename ~tablename:".dynstr" shl edata in
	    let dsymtab =
	      Elf_header.parse_symtbl
		~filename ~tablename:".dynsym" ~strtab:dstrtab
		shl edata eclass_conf
	    in
	    Format.printf "%a" (Print.symtbl ".dynsym") dsymtab
	  with Not_found -> ()
	end;

	let strtab =
	  Elf_header.strtab ~filename ~tablename:".strtab" shl edata in
	let symtab =
	  Elf_header.parse_symtbl ~filename ~tablename:".symtab" ~strtab
	    shl edata eclass_conf in
	Format.printf "%a" (Print.symtbl ".symtab") symtab;

	begin
	  try
            let instrs =
	      Elf_header.decode_instr
		~filename ~secname:".text" fh eclass_conf shl symtab
	    in
	    if instrs <> [] then
	      begin
		Format.printf "\nInstructions:\n";
		List.iter (fun i -> Format.printf "%a" Print.instr i) instrs
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
