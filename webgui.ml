
let onload _ =
  let main =
    Js.Opt.get (Dom_html.window##document##getElementById(Js.string "main"))
      (fun () -> assert false)
  in
  let filename = "/home/gp/obaf/tests/hello.arm" in
  let _ = Main.parse_class_endianness filename in
  Js._false
;;

let _ =
  Dom_html.window##onload <- Dom_html.handler onload
;;
