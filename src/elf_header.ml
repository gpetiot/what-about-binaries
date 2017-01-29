
exception Invalid_Elf

type etype = None | Relocatable | Executable | Shared | Core | Loproc | Hiproc;;

let int_to_etype = function
  | 0 -> None
  | 1 -> Relocatable
  | 2 -> Executable
  | 3 -> Shared
  | 4 -> Core
  | 65280 -> Loproc
  | 65535 -> Hiproc
  | x -> failwith (Format.sprintf "Elf_header.int_to_etype %i" x)
;;

let etype_to_string = function
  | None -> "NONE"
  | Relocatable -> "RELOCATABLE"
  | Executable -> "EXEC (Executable file)"
  | Shared -> "SHARED"
  | Core -> "CORE"
  | Loproc -> "LOPROC"
  | Hiproc -> "HIPROC"
;;

let parse_class_endianness filename =
  let chan = open_in_bin filename in
  let buf = Buffer.create 6 in
  try
    Buffer.add_channel buf chan 6;
    assert (Buffer.nth buf 0 = '\x7F');
    assert (Buffer.nth buf 1 = '\x45');
    assert (Buffer.nth buf 2 = '\x4c');
    assert (Buffer.nth buf 3 = '\x46');
    Archi.of_int (int_of_char (Buffer.nth buf 4)),
    Endian.of_int (int_of_char (Buffer.nth buf 5))
  with exn ->
    close_in chan;
    Format.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;

module Make (A : Archi.Addr) (E : Endian.T) = struct
  type t = {
    ei_version : int;
    ei_osabi : int;
    ei_abiversion : int;
    ei_type : etype;
    ei_machine : Machine.t;
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

  let machine x = x.ei_machine;;

  let pretty fmt e =
    Format.fprintf
      fmt
      "\
\  Class:                             ELF%a\n\
\  Data:                              %a\n\
\  Version:                           %i\n\
\  OS/ABI:                            %i\n\
\  ABI Version:                       %i\n\
\  Type:                              %s\n\
\  Machine:                           %a\n\
\  Entry point address:               0x%x\n\
\  Start of program headers:          %i (bytes into file)\n\
\  Start of section headers:          %i (bytes into file)\n\
\  Flags:                             0x%x\n\
\  Size of this header:               %i (bytes)\n\
\  Size of program headers:           %i (bytes)\n\
\  Number of program headers:         %i\n\
\  Size of section headers:           %i (bytes)\n\
\  Number of section headers:         %i\n\
\  Section header string table index: %i\n"
      Archi.pretty A.eclass
      Endian.pretty E.edata
      e.ei_version
      e.ei_osabi
      e.ei_abiversion
      (etype_to_string e.ei_type)
      Machine.pretty e.ei_machine
      e.ei_entry
      e.ei_phoff
      e.ei_shoff
      e.ei_flags
      e.ei_ehsize
      e.ei_phentsize
      e.ei_phnum
      e.ei_shentsize
      e.ei_shnum
      e.ei_shstrndx
  ;;

  let multi_bytes buf off nb =
    let rec aux ret = function
      | x when x = nb -> ret
      | x ->
	 try aux ((Buffer.nth buf (off+x))::ret) (x+1)
	 with Invalid_argument _ ->
	   failwith (Printf.sprintf "Elf_header.multi_bytes %i %i" off nb)
    in
    let l = List.rev_map int_of_char (aux [] 0) in
    E.order l
  ;;

  let multi_bytes_int buf off nb =
    let rec aux ret i = function
      | [] -> ret
      | h :: t ->
	 aux (ret lor (h lsl (8*(nb-i-1)))) (i+1) t
    in
    aux 0 0 (multi_bytes buf off nb)
  ;;
  
  let parse filename =
    let chan = open_in_bin filename in
    let buf = Buffer.create A.header_size in
    try
      Buffer.add_channel buf chan A.header_size;
      let ei_version = int_of_char (Buffer.nth buf 6) in
      let ei_osabi = int_of_char (Buffer.nth buf 7) in
      let ei_abiversion = int_of_char (Buffer.nth buf 8) in
      let ei_type = int_to_etype (multi_bytes_int buf 16 2) in
      let ei_machine = Machine.of_int (multi_bytes_int buf 18 2) in
      let version' = multi_bytes_int buf 20 4 in
      assert (ei_version = version');
      let ei_entry = multi_bytes_int buf 24 A.size in
      let ei_phoff = multi_bytes_int buf (24+A.size) A.size in
      let ei_shoff = multi_bytes_int buf (24+A.size*2) A.size in
      let ei_flags = multi_bytes_int buf (24+A.size*3) 4 in
      let ei_ehsize = multi_bytes_int buf (28+A.size*3) 2 in
      assert (ei_ehsize = A.header_size);
      let ei_phentsize = multi_bytes_int buf (30+A.size*3) 2 in
      let ei_phnum = multi_bytes_int buf (32+A.size*3) 2 in
      let ei_shentsize = multi_bytes_int buf (34+A.size*3) 2 in
      let ei_shnum = multi_bytes_int buf (36+A.size*3) 2 in
      let ei_shstrndx = multi_bytes_int buf (38+A.size*3) 2 in
      close_in chan;
      { ei_version; ei_osabi; ei_abiversion; ei_type; ei_machine; ei_entry;
	ei_phoff; ei_shoff; ei_flags; ei_ehsize; ei_phentsize; ei_phnum;
	ei_shentsize; ei_shnum; ei_shstrndx }
    with exn ->
      close_in chan;
      Format.printf "%s" (Printexc.to_string exn);
      raise Invalid_Elf
  ;;
  
  module Strtab = struct
    let parse filename ~off ~size =
      let chan = open_in_bin filename in
      let rec aux chan i =
	if i < off then
	  let _ = input_byte chan in
	  aux chan (i+1)
	else
	  let buf = Buffer.create size in
	  Buffer.add_channel buf chan size;
	  Buffer.contents buf
      in
      try
	let r = aux chan 0 in
	close_in chan;
	r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;

    let get snames index =
      let end_index = String.index_from snames index (char_of_int 0) in
      String.sub snames index (end_index - index)
    ;;
  end;;

  module Ph = struct
    type entry = {
      ph_type : Program_header_type.t;
      ph_off : int;
      ph_vaddr : int;
      ph_addr : int;
      ph_filesz : int;
      ph_memsz : int;
      ph_flags : int;
      ph_align : int;
    };;

    let flag_read f = (f land 4) <> 0;;
    let flag_write f = (f land 2) <> 0;;
    let flag_exec f = (f land 1) <> 0;;

    let pretty fmt e =
      Format.fprintf
	fmt
	"  %a 0x%016x 0x%016x 0x%016x 0x%x 0x%x %c%c%c %x\n"
	Program_header_type.pretty e.ph_type
        e.ph_off
        e.ph_vaddr
	e.ph_addr
        e.ph_filesz
        e.ph_memsz
	(if flag_read e.ph_flags then 'R' else ' ')
	(if flag_write e.ph_flags then 'W' else ' ')
	(if flag_exec e.ph_flags then 'E' else ' ')
        e.ph_align
    ;;

    let parse header filename =
      let chan = open_in_bin filename in
      let rec aux chan i ret =
	let pbeg = header.ei_phoff in
	let pend = header.ei_phoff + header.ei_phentsize * header.ei_phnum in
	if i < pbeg then
	  let _ = input_byte chan in
	  aux chan (i+1) ret
	else
	  if i < pend then
	    begin
	      let buf = Buffer.create header.ei_phentsize in
	      Buffer.add_channel buf chan header.ei_phentsize;
	      let o1,o2,o3,o4,o5,o6,o7,o8 = A.ph_offsets in
	      let s1,s2,s3,s4,s5,s6,s7,s8 = A.ph_sizes in
	      let ph_type =
		Program_header_type.of_int (multi_bytes_int buf o1 s1) in
	      let ph_off = multi_bytes_int buf o2 s2 in
	      let ph_vaddr = multi_bytes_int buf o3 s3 in
	      let ph_addr = multi_bytes_int buf o4 s4 in
	      let ph_filesz = multi_bytes_int buf o5 s5 in
	      let ph_memsz = multi_bytes_int buf o6 s6 in
	      let ph_flags = multi_bytes_int buf o7 s7 in
	      let ph_align = multi_bytes_int buf o8 s8 in
	      let ph = { ph_type; ph_off; ph_vaddr; ph_addr; ph_filesz;
			 ph_memsz; ph_flags; ph_align } in
	      aux chan (i+header.ei_phentsize) (ph::ret)
	    end
	  else
	    ret
      in
      try
	let r = List.rev (aux chan 0 []) in
	close_in chan;
	r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;
  end;;

  module Sh = struct
    type entry = {
      sh_name : string;
      sh_type : Section_header_type.t;
      sh_flags : int;
      sh_addr : int;
      sh_off : int;
      sh_size : int;
      sh_link : int;
      sh_info : int;
      sh_addralign : int;
      sh_entsize : int;
    };;

    let pretty fmt e =
      Format.fprintf
	fmt
	"%-17s %a %08x %08x %08x %02x %3i %2i %3i %2i\n"
	e.sh_name
	Section_header_type.pretty e.sh_type
	e.sh_addr
	e.sh_off
	e.sh_size
	e.sh_entsize
	e.sh_flags
	e.sh_link
	e.sh_info
	e.sh_addralign
    ;;

    let get name = List.find (fun x -> x.sh_name = name);;
    let offset s = s.sh_off;;
    let size s = s.sh_size;;
    let entry_size s = s.sh_entsize;;
    let addr s = s.sh_addr;;
    
    let strtab ~filename ~tablename sections =
      let strtab = get tablename sections in
      let off = offset strtab in
      let size = size strtab in
      Strtab.parse filename ~off ~size
    ;;

    let parse header filename =
      let chan = open_in_bin filename in
      let rec aux chan i ret =
	let sbeg = header.ei_shoff in
	let send = header.ei_shoff + header.ei_shentsize * header.ei_shnum in
	if i < sbeg then
	  let _ = input_byte chan in
	  aux chan (i+1) ret
	else
	  if i < send then
	    begin
	      let buf = Buffer.create header.ei_shentsize in
	      Buffer.add_channel buf chan header.ei_shentsize;
	      let sh_name = multi_bytes_int buf 0 4 in
	      let sh_type =
		Section_header_type.of_int (multi_bytes_int buf 4 4) in
	      let sh_flags = multi_bytes_int buf 8 A.size in
	      let sh_addr = multi_bytes_int buf (8+A.size) A.size in
	      let sh_off = multi_bytes_int buf (8+A.size*2) A.size in
	      let sh_size = multi_bytes_int buf (8+A.size*3) A.size in
	      let sh_link = multi_bytes_int buf (8+A.size*4) 4 in
	      let sh_info = multi_bytes_int buf (12+A.size*4) 4 in
	      let sh_addralign = multi_bytes_int buf (16+A.size*4) A.size in
	      let sh_entsize = multi_bytes_int buf (16+A.size*5) A.size in
	      let sh = sh_name, sh_type, sh_flags, sh_addr, sh_off, sh_size,
		       sh_link, sh_info, sh_addralign, sh_entsize in
	      aux chan (i+header.ei_shentsize) (sh::ret)
	    end
	  else
	    ret
      in
      try
	let r = List.rev (aux chan 0 []) in
	close_in chan;
	let _,_,_,_,off,size,_,_,_,_ = List.nth r header.ei_shstrndx in
	let shnames = Strtab.parse filename ~off ~size in
	let name_section (id,sh_type,sh_flags,sh_addr,sh_off,sh_size,sh_link,
			  sh_info,sh_addralign,sh_entsize) =
	  let sh_name = Strtab.get shnames id in
	  { sh_name; sh_type; sh_flags; sh_addr; sh_off; sh_size; sh_link;
	    sh_info; sh_addralign; sh_entsize }
	in
	List.map name_section r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;
  end;;
    
  module Symtbl = struct
    type entry = {
      value : int;
      size : int;
      symtype : Symbol.Type.t;
      bind : Symbol.Binding.t;
      vis : Symbol.Visibility.t;
      ndx : Symbol.Ndx.t;
      name : string;
    };;
      
    let pretty fmt x =
      Format.fprintf
	fmt "%08x  %4i %a %a %a %a %s\n"
        x.value
	x.size
	Symbol.Type.pretty x.symtype
	Symbol.Binding.pretty x.bind
	Symbol.Visibility.pretty x.vis
	Symbol.Ndx.pretty x.ndx
	x.name
    ;;
      
    let parse ~filename ~tablename ~strtab sections =
      let section = Sh.get tablename sections in
      let offset = Sh.offset section in
      let size = Sh.size section in
      let entry_size = Sh.entry_size section in
      let chan = open_in_bin filename in
      let rec aux chan i ret =
	let sbeg = offset in
	let send = offset+size in
	if i < sbeg then
	  let _ = input_byte chan in
	  aux chan (i+1) ret
	else
	  if i < send then
	    begin
	      let buf = Buffer.create entry_size in
	      Buffer.add_channel buf chan entry_size;
	      let o1,o2,o3,o4,o5,o6 = A.sym_offsets in
	      let s1,s2,s3,s4,s5,s6 = A.sym_sizes in
	      let name_id = multi_bytes_int buf o1 s1 in
	      let value = multi_bytes_int buf o2 s2 in
	      let size = multi_bytes_int buf o3 s3 in
	      let info = multi_bytes_int buf o4 s4 in
	      let other = multi_bytes_int buf o5 s5 in
	      let shndex = multi_bytes_int buf o6 s6 in
	      let name = Strtab.get strtab name_id in
	      let bind = Symbol.Binding.of_int (info lsr 4) in
	      let symtype = Symbol.Type.of_int (info land 15) in
	      let vis = Symbol.Visibility.of_int other in
	      let ndx = Symbol.Ndx.of_int shndex in
	      let symbol = {value; size; symtype; bind; vis; ndx; name} in
	      aux chan (i+entry_size) (symbol::ret)
	    end
	  else
	    ret
      in
      try
	let r = List.rev (aux chan 0 []) in
	close_in chan;
	r
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;

    let get s = List.find (fun x -> x.name = s);;
    let value s = s.value;;
    let size s = s.size;;
  end;;

  module Decode = struct
    let decode ~filename ~secname ei_machine sections symbols =
      let section = Sh.get secname sections in
      let offset = Sh.offset section in
      let _size = Sh.size section in
      let start_addr = Sh.addr section in
      let main = Symtbl.get "main" symbols in
      let main_value = Symtbl.value main in
      let main_size = Symtbl.size main in
      let chan = open_in_bin filename in
      let rec aux chan i =
	let sbeg = main_value - start_addr + offset in
	let send = sbeg + main_size in
	if i < sbeg then
	  let _ = input_byte chan in
	  aux chan (i+1)
	else
	  if i < send then
	    begin
	      let buf = Buffer.create main_size in
	      Buffer.add_channel buf chan main_size;
	      let buf_str = Buffer.contents buf in
	      let module I = (val (Machine.instr ei_machine)) in
	      I.decode A.modes buf_str
	    end
	  else
	    assert false; (* unreachable *)
      in
      try
        let instrs = aux chan 0 in
	close_in chan;
	instrs
      with exn ->
	close_in chan;
	Format.printf "%s" (Printexc.to_string exn);
	raise Invalid_Elf
    ;;
  end;;
end;;
