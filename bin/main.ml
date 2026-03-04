let () =
  if Array.length Sys.argv < 2 then (
    prerr_endline "Usage: compiler <source_file>";
    exit 1
  );

  let file_path = Sys.argv.(1) in

  let content =
    try
      let ic = open_in file_path in
      let len = in_channel_length ic in
      let text = really_input_string ic len in
      close_in ic;
      text
    with Sys_error msg ->
      prerr_endline ("Error reading file: " ^ msg);
      exit 1
  in

  print_endline "File content:";
  print_endline content
