
open Elf_types

module Addr32 =
struct
  let eclass = C32
  let size = 4
  let header_size = 52
  let word_size = 4
  let addr_size  = 4
  let half_size = 2
  let xword_size = 4
  let off_size = 4
    
  (* program header field sizes *)
  let type_size = word_size
  let offset_size = off_size
  let vaddr_size = addr_size
  let paddr_size = addr_size
  let filesz_size = word_size
  let memsz_size = word_size
  let flags_size = word_size
  let align_size = word_size
    
  (* program header field offsets *)
  let type_offset = 0
  let offset_offset = type_offset + type_size
  let vaddr_offset = offset_offset + offset_size
  let paddr_offset = vaddr_offset + vaddr_size
  let filesz_offset = paddr_offset + paddr_size
  let memsz_offset = filesz_offset + filesz_size
  let flags_offset = memsz_offset + memsz_size
  let align_offset = flags_offset + flags_size
      
  (* symbol field sizes *)
  let name_size = word_size
  let value_size = addr_size
  let size_size = word_size
  let info_size = 1
  let other_size = 1
  let shndex_size = half_size
    
  (* symbol field offsets *)
  let name_offset = 0
  let value_offset = name_offset + name_size
  let size_offset = value_offset + value_size
  let info_offset = size_offset + size_size
  let other_offset = info_offset + info_size
  let shndex_offset = other_offset + other_size

  let modes = [Capstone.CS_MODE_32]
end;;

module Addr64 =
struct
  let eclass = C64
  let size = 8
  let header_size = 64
  let word_size = 4
  let addr_size  = 8
  let half_size = 2
  let xword_size = 8
  let off_size = 8
    
  (* program header field sizes *)
  let type_size = word_size
  let offset_size = off_size
  let vaddr_size = addr_size
  let paddr_size = addr_size
  let filesz_size = xword_size
  let memsz_size = xword_size
  let flags_size = word_size
  let align_size = xword_size

  (* program header field offsets *)
  let type_offset = 0
  let flags_offset = type_offset + type_size
  let offset_offset = flags_offset + flags_size
  let vaddr_offset = offset_offset + offset_size
  let paddr_offset = vaddr_offset + vaddr_size
  let filesz_offset = paddr_offset + paddr_size
  let memsz_offset = filesz_offset + filesz_size
  let align_offset = memsz_offset + memsz_size
      
  (* symbol field sizes *)
  let name_size = word_size
  let value_size = addr_size
  let size_size = xword_size
  let info_size = 1
  let other_size = 1
  let shndex_size = half_size
    
  (* symbol field offsets *)
  let name_offset = 0
  let info_offset = name_offset + name_size
  let other_offset = info_offset + info_size
  let shndex_offset = other_offset + other_size
  let value_offset = shndex_offset + shndex_size
  let size_offset = value_offset + value_size
      
  let modes = [Capstone.CS_MODE_64]
end;;

let addr = function
  | C32 -> (module Addr32 : Addr)
  | C64 -> (module Addr64 : Addr)
;;
