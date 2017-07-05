
exception Invalid_Elf

open Elf_types


let parse_class_endianness filename =
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
    Format.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;

let multi_bytes endianness buf off nb =
  let order = match endianness with BigEndian -> (fun x -> x) | _ -> List.rev in
  let rec aux ret = function
    | x when x = nb -> ret
    | x ->
       try aux ((Buffer.nth buf (off+x))::ret) (x+1)
       with Invalid_argument _ ->
	 failwith (Printf.sprintf "Elf_header.multi_bytes %i %i" off nb)
  in
  let l = List.rev_map int_of_char (aux [] 0) in
  order l
;;

let multi_bytes_int endianness buf off nb =
  let rec aux ret i = function
    | [] -> ret
    | h :: t ->
       aux (ret lor (h lsl (8*(nb-i-1)))) (i+1) t
  in
  aux 0 0 (multi_bytes endianness buf off nb)
;;

let get_strtab snames index =
  let end_index = String.index_from snames index (char_of_int 0) in
  String.sub snames index (end_index - index)
;;
    
let parse_elf_header eclass_conf ei_endian filename =
  let multi_bytes_int = multi_bytes_int ei_endian in
  let chan = open_in_bin filename in
  let buf = Buffer.create eclass_conf.header_size in
  try
    Buffer.add_channel buf chan eclass_conf.header_size;
    let ei_class = eclass_conf.eclass in
    let ei_version = int_of_char (Buffer.nth buf 6) in
    let ei_osabi = int_of_char (Buffer.nth buf 7) in
    let ei_abiversion = int_of_char (Buffer.nth buf 8) in
    let ei_type = Decode.etype (multi_bytes_int buf 16 2) in
    let ei_machine = Decode.emachine (multi_bytes_int buf 18 2) in
    let version' = multi_bytes_int buf 20 4 in
    assert (ei_version = version');
    let ei_entry = multi_bytes_int buf 24 eclass_conf.addr_size in
    let ei_phoff = multi_bytes_int buf (24+eclass_conf.addr_size) eclass_conf.addr_size in
    let ei_shoff = multi_bytes_int buf (24+eclass_conf.addr_size*2) eclass_conf.addr_size in
    let ei_flags = multi_bytes_int buf (24+eclass_conf.addr_size*3) 4 in
    let ei_ehsize = multi_bytes_int buf (28+eclass_conf.addr_size*3) 2 in
    assert (ei_ehsize = eclass_conf.header_size);
    let ei_phentsize = multi_bytes_int buf (30+eclass_conf.addr_size*3) 2 in
    let ei_phnum = multi_bytes_int buf (32+eclass_conf.addr_size*3) 2 in
    let ei_shentsize = multi_bytes_int buf (34+eclass_conf.addr_size*3) 2 in
    let ei_shnum = multi_bytes_int buf (36+eclass_conf.addr_size*3) 2 in
    let ei_shstrndx = multi_bytes_int buf (38+eclass_conf.addr_size*3) 2 in
    close_in chan;
    { ei_class; ei_endian; ei_version; ei_osabi; ei_abiversion; ei_type;
      ei_machine; ei_entry; ei_phoff; ei_shoff; ei_flags; ei_ehsize;
      ei_phentsize; ei_phnum;	ei_shentsize; ei_shnum; ei_shstrndx }
  with exn ->
    close_in chan;
    Format.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;
  
let parse_strtab filename ~off ~size endianness =
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

let parse_program_header endianness eclass_conf header filename =
  let multi_bytes_int = multi_bytes_int endianness in
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
	  let ph_type =
	    Decode.ph_type
	      (multi_bytes_int buf eclass_conf.type_offset eclass_conf.type_size) in
	  let ph_off = multi_bytes_int buf eclass_conf.offset_offset eclass_conf.offset_size in
	  let ph_vaddr = multi_bytes_int buf eclass_conf.vaddr_offset eclass_conf.vaddr_size in
	  let ph_addr = multi_bytes_int buf eclass_conf.paddr_offset eclass_conf.paddr_size in
	  let ph_filesz = multi_bytes_int buf eclass_conf.filesz_offset eclass_conf.filesz_size in
	  let ph_memsz = multi_bytes_int buf eclass_conf.memsz_offset eclass_conf.memsz_size in
	  let ph_flags = multi_bytes_int buf eclass_conf.flags_offset eclass_conf.flags_size in
	  let ph_align = multi_bytes_int buf eclass_conf.align_offset eclass_conf.align_size in
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

let strtab ~filename ~tablename sections endianness =
  let strtab = List.find (fun x -> x.sh_name = tablename) sections in
  let off = strtab.sh_off in
  let size = strtab.sh_size in
  parse_strtab filename ~off ~size endianness
;;

let parse_section_header endianness eclass_conf header filename =
  let multi_bytes_int = multi_bytes_int endianness in
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
	  let sh_type = Decode.sh_type (multi_bytes_int buf 4 4) in
	  let sh_flags = multi_bytes_int buf 8 eclass_conf.addr_size in
	  let sh_addr = multi_bytes_int buf (8+eclass_conf.addr_size) eclass_conf.addr_size in
	  let sh_off = multi_bytes_int buf (8+eclass_conf.addr_size*2) eclass_conf.addr_size in
	  let sh_size = multi_bytes_int buf (8+eclass_conf.addr_size*3) eclass_conf.addr_size in
	  let sh_link = multi_bytes_int buf (8+eclass_conf.addr_size*4) 4 in
	  let sh_info = multi_bytes_int buf (12+eclass_conf.addr_size*4) 4 in
	  let sh_addralign = multi_bytes_int buf (16+eclass_conf.addr_size*4) eclass_conf.addr_size in
	  let sh_entsize = multi_bytes_int buf (16+eclass_conf.addr_size*5) eclass_conf.addr_size in
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
    let shnames = parse_strtab filename ~off ~size endianness in
    let name_section (id,sh_type,sh_flags,sh_addr,sh_off,sh_size,sh_link,
		      sh_info,sh_addralign,sh_entsize) =
      let sh_name = get_strtab shnames id in
      { sh_name; sh_type; sh_flags; sh_addr; sh_off; sh_size; sh_link;
	sh_info; sh_addralign; sh_entsize }
    in
    List.map name_section r
  with exn ->
    close_in chan;
    Format.printf "%s" (Printexc.to_string exn);
    raise Invalid_Elf
;;
    
let parse_symtbl ~filename ~tablename ~strtab sections endianness eclass_conf =
  let multi_bytes_int = multi_bytes_int endianness in
  let section = List.find (fun x -> x.sh_name = tablename) sections in
  let offset = section.sh_off in
  let size = section.sh_size in
  let entry_size = section.sh_entsize in
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
	  let name_id = multi_bytes_int buf eclass_conf.name_offset eclass_conf.name_size in
	  let value = multi_bytes_int buf eclass_conf.value_offset eclass_conf.value_size in
	  let size = multi_bytes_int buf eclass_conf.size_offset eclass_conf.size_size in
	  let info = multi_bytes_int buf eclass_conf.info_offset eclass_conf.info_size in
	  let other = multi_bytes_int buf eclass_conf.other_offset eclass_conf.other_size in
	  let shndex = multi_bytes_int buf eclass_conf.shndex_offset eclass_conf.shndex_size in
	  let name = get_strtab strtab name_id in
	  let bind = Decode.sym_binding (info lsr 4) in
	  let symtype = Decode.sym_type (info land 15) in
	  let vis = Decode.sym_visibility other in
	  let ndx = Decode.sym_ndx shndex in
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

let decode_instr ~filename ~secname {ei_machine} eclass_conf sections symbols =
  let section = List.find (fun x -> x.sh_name = secname) sections in
  let offset = section.sh_off in
  let _size = section.sh_size in
  let start_addr = section.sh_addr in
  let main = List.find (fun x -> x.name = "main") symbols in
  let main_value = main.value in
  let main_size = main.size in
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
	  let decode_instrs = match ei_machine with
	    | ARM -> Decode_arm.decode
	    | X86 | X86_64 -> Decode_x86.decode
	    | _ -> failwith (Format.sprintf "Unsupported instruction set")
	  in
	  decode_instrs eclass_conf.modes buf_str
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
