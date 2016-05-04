
let locale = GtkMain.Main.init ();;

let opened_file = ref None;;

let main () =
  let window =
    GWindow.window ~width:520 ~height:240 ~title:"Simple lablgtk program" () in
  let vbox = GPack.vbox ~packing:window#add () in
  ignore (window#connect#destroy ~callback:GMain.Main.quit);

  let view = GText.view ~packing:vbox#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in

  let open_file() =
    let chooser = GWindow.file_selection ~deletable:true () in
    chooser#set_select_multiple false;
    let callback () =
      let filename = chooser#filename in
      let str = Printf.sprintf "file %s selected" filename in
      let buf = view#buffer in
      buf#set_text str;
      opened_file := Some filename;
      chooser#destroy ()
    in
    let callback_aux = function
      | `OK -> callback ()
      | _ ->
  	 let str = "no file selected" in
  	 let buf = view#buffer in
  	 buf#set_text str;
  	 view#set_buffer buf;
  	 chooser#destroy ()
    in
    ignore (chooser#connect#response ~callback:callback_aux);
    let destroy () = chooser#destroy () in
    ignore (chooser#cancel_button#connect#clicked ~callback:destroy);
    ignore (chooser#ok_button#connect#clicked ~callback);
    ignore (chooser#run ())
  in

  (* let open_file() = *)
  (*   let chooser = *)
  (*     GWindow.file_chooser_dialog *)
  (* 	~action:`OPEN ~deletable:true ~resizable:true () in *)
  (*   chooser#set_select_multiple false; *)
  (*   let callback () = *)
  (*     match chooser#filename with *)
  (*     | Some x -> *)
  (* 	 let str = Printf.sprintf "file %s activated" x in *)
  (* 	 let buf = view#buffer in *)
  (* 	 buf#set_text str; *)
  (* 	 opened_file := Some x; *)
  (* 	 chooser#destroy () *)
  (*     | None -> () *)
  (*   in *)
  (*   let callback_aux = function *)
  (*     | `OK -> *)
  (* 	 begin *)
  (* 	   match chooser#filename with *)
  (* 	   | Some x -> *)
  (* 	      let str = Printf.sprintf "file %s activated" x in *)
  (* 	      let buf = view#buffer in *)
  (* 	      buf#set_text str; *)
  (* 	      opened_file := Some str; *)
  (* 	      chooser#destroy () *)
  (* 	   | None -> () *)
  (* 	 end *)
  (*     | `Cancel -> *)
  (* 	 let str = "no file selected" in *)
  (* 	 let buf = view#buffer in *)
  (* 	 buf#set_text str; *)
  (* 	 view#set_buffer buf; *)
  (* 	 chooser#destroy () *)
  (*     | `DELETE_EVENT -> () *)
  (*   in *)
  (*   ignore (chooser#connect#response ~callback:callback_aux); *)
  (*   chooser#add_select_button "Open" `OK; *)
  (*   chooser#add_select_button "Cancel" `Cancel; *)
  (*   (\*ignore (chooser#connect#file_activated ~callback);*\) *)
  (*   ignore (chooser#connect#close ~callback:chooser#destroy); *)
  (*   ignore (chooser#run ()); *)
  (* in *)
  
  ignore (factory#add_item "Open" ~key:GdkKeysyms._O ~callback:open_file);
  ignore (factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback:GMain.Main.quit);

  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  window#show ();
  GMain.Main.main ()
;;

let () = main ();;
