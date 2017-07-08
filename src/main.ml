
let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = Parse.class_endianness filename in
	let eclass_conf = match eclass with
	  | Elf_types.C32 -> Eclass_conf.conf_32
	  | Elf_types.C64 -> Eclass_conf.conf_64
	in

	let fh = Parse.elf_header eclass_conf edata filename in
	Format.printf "ELF Header:\n%a" Print.elf_header fh;

	let shl = Parse.section_header edata eclass_conf fh filename in
        Format.printf "%a" Print.sh shl;
	
	Format.printf "\
Key to Flags:\n\
\  W (write), A (alloc), X (execute), M (merge), S (strings)\n\
\  I (info), L (link order), G (group), T (TLS), E (exclude), x (unknown)\n\
\  O (extra OS processing required) o (OS specific), p (processor specific)\n";

	let phl = Parse.program_header edata eclass_conf fh filename in
	Format.printf "%a" Print.ph phl;
        
	begin
	  try
	    let dsymtab =
	      Parse.symtbl
		~filename ~symtbl_name:".dynsym" ~strtab_name:".dynstr"
		shl edata eclass_conf
	    in
	    Format.printf "%a" (Print.symtbl ".dynsym") dsymtab
	  with Not_found -> ()
	end;

	let symtab =
	  Parse.symtbl ~filename ~symtbl_name:".symtab" ~strtab_name:".strtab"
	    shl edata eclass_conf in
	Format.printf "%a" (Print.symtbl ".symtab") symtab;

	begin
	  try
            let instrs =
	      Parse.instrs ~filename ~secname:".text" fh eclass_conf shl symtab
	    in
	    if instrs <> [] then
	      begin
		Format.printf "\nInstructions:\n";
		List.iter (fun i -> Format.printf "%a" Print.instr i) instrs
	      end
	  with Not_found -> ()
	end
      with
      | Failure msg -> Format.printf "[wab] error: %s\n" msg
      | exn -> Format.printf "[wab] error: %s\n" (Printexc.to_string exn)
    else
      Format.printf "%s does not exist !\n" filename
  else
    Format.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
