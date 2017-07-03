
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
	let fh = FH.parse eclass edata filename in
	Format.printf "ELF Header:\n%a" Print.elf_header fh;

	(* section header *)
	let shl = FH.Sh.parse fh filename in
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
	let phl = FH.Ph.parse fh filename in
	Format.printf "\nProgram Headers:\n";
	Format.printf "  Type           Offset   VirtAddr   PhysAddr   FileSiz \
MemSiz  Flg Align\n";
	List.iter (fun x -> Format.printf "%a" Print.ph_entry x) phl;
	
	(* dynamic symbol table *)
	begin
	  try
	    let dstrtab = FH.Sh.strtab ~filename ~tablename:".dynstr" shl in
	    let dsymtab =
	      FH.Symtbl.parse ~filename ~tablename:".dynsym" ~strtab:dstrtab shl
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
	let strtab = FH.Sh.strtab ~filename ~tablename:".strtab" shl in
	let symtab =
	  FH.Symtbl.parse ~filename ~tablename:".symtab" ~strtab shl in
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
	      FH.Decode.decode ~filename ~secname:".text" fh shl symtab in
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
