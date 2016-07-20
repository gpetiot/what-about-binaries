
let () =
  let argc = Array.length Sys.argv in
  if argc > 1 then
    let filename = Sys.argv.(1) in
    if Sys.file_exists filename then
      try
	let eclass, edata = Elf_header.parse_class_endianness filename in
	let module A = (val (Archi.addr eclass)) in
	let module E = (val (Endian.endianness edata)) in
	let module FH = Elf_header.Make(A)(E) in

	(* file header *)
	let fh = FH.parse filename in
	Format.printf "file header:\n%a" FH.pretty fh;

	(* section header *)
	let shl = FH.Sh.parse fh filename in
	Format.printf "\nsection header:\n";
	List.iter (fun x -> Format.printf "%a" FH.Sh.pretty x) shl;

	(* program header *)
	let phl = FH.Ph.parse fh filename in
	Format.printf "\nprogram header:\n";
	List.iter (fun x -> Format.printf "%a" FH.Ph.pretty x) phl;

	(* symbol table *)
	let strtab = FH.Sh.strtab ~filename ~tablename:".strtab" shl in
	let symtab =
	  FH.Symtbl.parse ~filename ~tablename:".symtab" ~strtab shl in
	Format.printf "\nsymbol table:\n";
	List.iter (fun x -> Format.printf "%a" FH.Symtbl.pretty x) symtab;
	
	(* dynamic symbol table *)
	begin
	  try
	    let dstrtab = FH.Sh.strtab ~filename ~tablename:".dynstr" shl in
	    let dsymtab =
	      FH.Symtbl.parse ~filename ~tablename:".dynsym" ~strtab:dstrtab shl
	    in
	    Format.printf "\ndynamic symbol table:\n";
	    List.iter (fun x -> Format.printf "%a" FH.Symtbl.pretty x) dsymtab
	  with Not_found -> ()
	end;

	(* instructions *)
	begin
	  try
            FH.Decode.start ~filename ~secname:".text" shl
	  with Not_found -> ()
	end
      with
	Elf_header.Invalid_Elf -> Format.printf "invalid ELF file !\n"
    else
      Format.printf "%s does not exist !\n" filename
  else
    Format.printf "Usage: %s binary_file\n" Sys.argv.(0)
;;
