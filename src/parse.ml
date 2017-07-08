
open Elf_types


let class_endianness filename =
  let chan = open_in_bin filename in
  let buf = Buffer.create 6 in
  try
    Buffer.add_channel buf chan 6;
    assert (Buffer.nth buf 0 = '\x7F');
    assert (Buffer.nth buf 1 = '\x45');
    assert (Buffer.nth buf 2 = '\x4c');
    assert (Buffer.nth buf 3 = '\x46');
    Decode.eclass (int_of_char (Buffer.nth buf 4)),
    Decode.endianness (int_of_char (Buffer.nth buf 5))
  with exn ->
    close_in chan;
    raise exn
;;

let bytes endianness buf off nb =
  let order = match endianness with BigEndian -> (fun x -> x) | _ -> List.rev in
  let rec aux ret = function
    | x when x = nb -> ret
    | x ->
       try aux ((Buffer.nth buf (off+x))::ret) (x+1)
       with Invalid_argument _ ->
	 failwith (Printf.sprintf "Parse.bytes %i %i is invalid" off nb)
  in
  let l = List.rev_map int_of_char (aux [] 0) in
  order l
;;

let int_bytes endianness buf off nb =
  let rec aux ret i = function
    | [] -> ret
    | h :: t ->
       aux (ret lor (h lsl (8*(nb-i-1)))) (i+1) t
  in
  aux 0 0 (bytes endianness buf off nb)
;;

let elf_header eclass_conf ei_endian filename =
  let int_bytes = int_bytes ei_endian in
  let chan = open_in_bin filename in
  let buf = Buffer.create eclass_conf.header_size in
  try
    Buffer.add_channel buf chan eclass_conf.header_size;
    let ei_class = eclass_conf.eclass in
    let ei_version = int_of_char (Buffer.nth buf 6) in
    let ei_osabi = int_of_char (Buffer.nth buf 7) in
    let ei_abiversion = int_of_char (Buffer.nth buf 8) in
    let ei_type = Decode.etype (int_bytes buf 16 2) in
    let ei_machine = Decode.emachine (int_bytes buf 18 2) in
    let version' = int_bytes buf 20 4 in
    assert (ei_version = version');
    let ei_entry = int_bytes buf 24 eclass_conf.addr_size in
    let ei_phoff =
      int_bytes buf (24+eclass_conf.addr_size) eclass_conf.addr_size in
    let ei_shoff =
      int_bytes buf (24+eclass_conf.addr_size*2) eclass_conf.addr_size in
    let ei_flags = int_bytes buf (24+eclass_conf.addr_size*3) 4 in
    let ei_ehsize = int_bytes buf (28+eclass_conf.addr_size*3) 2 in
    assert (ei_ehsize = eclass_conf.header_size);
    let ei_phentsize = int_bytes buf (30+eclass_conf.addr_size*3) 2 in
    let ei_phnum = int_bytes buf (32+eclass_conf.addr_size*3) 2 in
    let ei_shentsize = int_bytes buf (34+eclass_conf.addr_size*3) 2 in
    let ei_shnum = int_bytes buf (36+eclass_conf.addr_size*3) 2 in
    let ei_shstrndx = int_bytes buf (38+eclass_conf.addr_size*3) 2 in
    close_in chan;
    { ei_class; ei_endian; ei_version; ei_osabi; ei_abiversion; ei_type;
      ei_machine; ei_entry; ei_phoff; ei_shoff; ei_flags; ei_ehsize;
      ei_phentsize; ei_phnum;	ei_shentsize; ei_shnum; ei_shstrndx }
  with exn ->
    close_in chan;
    raise exn
;;

let get_strtab_content filename ~off ~size endianness =
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
    raise exn
;;

let program_header endianness conf header filename =
  let int_bytes = int_bytes endianness in
  let chan = open_in_bin filename in
  let rec aux chan i ret =
    if i < header.ei_phoff then
      let _ = input_byte chan in
      aux chan (i+1) ret
    else
      if i < header.ei_phoff + header.ei_phentsize * header.ei_phnum then
	begin
	  let buf = Buffer.create header.ei_phentsize in
	  Buffer.add_channel buf chan header.ei_phentsize;
	  let ph_type_int = int_bytes buf conf.type_offset conf.type_size in
	  let ph = {
	    ph_type = Decode.ph_type ph_type_int;
	    ph_off = int_bytes buf conf.offset_offset conf.offset_size;
	    ph_vaddr = int_bytes buf conf.vaddr_offset conf.vaddr_size;
	    ph_addr = int_bytes buf conf.paddr_offset conf.paddr_size;
	    ph_filesz = int_bytes buf conf.filesz_offset conf.filesz_size;
	    ph_memsz = int_bytes buf conf.memsz_offset conf.memsz_size;
	    ph_flags = int_bytes buf conf.flags_offset conf.flags_size;
	    ph_align = int_bytes buf conf.align_offset conf.align_size } in
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
    raise exn
;;

let section_header endianness conf header filename =
  let int_bytes = int_bytes endianness in
  let chan = open_in_bin filename in
  let rec aux chan i ret =
    if i < header.ei_shoff then
      let _ = input_byte chan in
      aux chan (i+1) ret
    else
      if i < header.ei_shoff + header.ei_shentsize * header.ei_shnum then
	begin
	  let buf = Buffer.create header.ei_shentsize in
	  Buffer.add_channel buf chan header.ei_shentsize;
	  let sh_name = int_bytes buf 0 4 in
	  let sh_type = Decode.sh_type (int_bytes buf 4 4) in
	  let sh_flags = int_bytes buf 8 conf.addr_size in
	  let sh_addr = int_bytes buf (8+conf.addr_size) conf.addr_size in
	  let sh_off = int_bytes buf (8+conf.addr_size*2) conf.addr_size in
	  let sh_size = int_bytes buf (8+conf.addr_size*3) conf.addr_size in
	  let sh_link = int_bytes buf (8+conf.addr_size*4) 4 in
	  let sh_info = int_bytes buf (12+conf.addr_size*4) 4 in
	  let sh_addralign =
	    int_bytes buf (16+conf.addr_size*4) conf.addr_size in
	  let sh_entsize = int_bytes buf (16+conf.addr_size*5) conf.addr_size in
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
    let shnames = get_strtab_content filename ~off ~size endianness in
    let name_section (id,sh_type,sh_flags,sh_addr,sh_off,sh_size,sh_link,
		      sh_info,sh_addralign,sh_entsize) =
      let end_index = String.index_from shnames id (char_of_int 0) in
      let sh_name = String.sub shnames id (end_index - id) in
      { sh_name; sh_type; sh_flags; sh_addr; sh_off; sh_size; sh_link;
	sh_info; sh_addralign; sh_entsize }
    in
    List.map name_section r
  with exn ->
    close_in chan;
    raise exn
;;

let symtbl ~filename ~symtbl_name ~strtab_name sections endianness conf =
  let strtab_section = List.find (fun x -> x.sh_name = strtab_name) sections in
  let off = strtab_section.sh_off in
  let size = strtab_section.sh_size in
  let strtab_content = get_strtab_content filename ~off ~size endianness in
  let int_bytes = int_bytes endianness in
  let symtbl_section = List.find (fun x -> x.sh_name = symtbl_name) sections in
  let entry_size = symtbl_section.sh_entsize in
  let chan = open_in_bin filename in
  let rec aux chan i ret =
    if i < symtbl_section.sh_off then
      let _ = input_byte chan in
      aux chan (i+1) ret
    else
      if i < symtbl_section.sh_off + symtbl_section.sh_size then
	begin
	  let buf = Buffer.create entry_size in
	  Buffer.add_channel buf chan entry_size;
	  let name_id = int_bytes buf conf.name_offset conf.name_size in
	  let end_index =
	    String.index_from strtab_content name_id (char_of_int 0) in
	  let info = int_bytes buf conf.info_offset conf.info_size in
	  let other = int_bytes buf conf.other_offset conf.other_size in
	  let shndex = int_bytes buf conf.shndex_offset conf.shndex_size in
	  let symbol = {
	    name = String.sub strtab_content name_id (end_index - name_id);
	    value = int_bytes buf conf.value_offset conf.value_size;
	    size = int_bytes buf conf.size_offset conf.size_size;
	    bind = Decode.sym_binding (info lsr 4);
	    symtype = Decode.sym_type (info land 15);
	    vis = Decode.sym_visibility other;
	    ndx = Decode.sym_ndx shndex } in
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
    raise exn
;;

let x86_instrs modes str =
  let handle = Capstone.cs_open Capstone.CS_ARCH_X86 modes in
  let _ =
    Capstone.cs_option
      handle Capstone.CS_OPT_DETAIL Capstone._CS_OPT_ON in
  let instr = Capstone.cs_disasm handle str 0x1000L 0L in
  if Capstone.cs_close handle <> 0 then
    failwith "Decoding x86 instructions with Capstone: Failed to close handle";
  instr
;;

let instrs ~filename ~secname {ei_machine} eclass_conf sections symbols =
  let section = List.find (fun x -> x.sh_name = secname) sections in
  let main = List.find (fun x -> x.name = "main") symbols in
  let chan = open_in_bin filename in
  let rec aux chan i =
    let beg = main.value - section.sh_addr + section.sh_off in
    if i < beg then
      let _ = input_byte chan in
      aux chan (i+1)
    else
      if i < beg + main.size then
	let buf = Buffer.create main.size in
	Buffer.add_channel buf chan main.size;
	let buf_str = Buffer.contents buf in
	match ei_machine with
	| X86 | X86_64 -> x86_instrs eclass_conf.modes buf_str
	| _ -> failwith "Unsupported instruction set"
      else
	assert false; (* unreachable *)
  in
  try
    let instrs = aux chan 0 in
    close_in chan;
    instrs
  with exn ->
    close_in chan;
    raise exn
;;
