open Graph

let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and source = Sys.argv.(2)
  and sink = Sys.argv.(3)
  and outfile = Sys.argv.(4) in

  let graph = Gfile.from_file infile in

  (* Test of Graph.map *)
  let graph2 = Graph.map graph int_of_string int_of_string in
  
  
  let () = Gfile.export graph2 "dotGraphFrance" string_of_int  in

  let flow = Ford_fulkerson.ford_fulk graph2 source sink in
  print_endline ("The max flow found for this graph is of : " ^ (string_of_int flow));
   
  ()



 


