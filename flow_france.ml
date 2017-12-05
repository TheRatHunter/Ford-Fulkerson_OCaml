open Graph

let () =

  if Array.length Sys.argv <> 4 then
    begin
      Printf.printf "\nUsage: %s infile source sink \n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and source = Sys.argv.(2)
  and sink = Sys.argv.(3) in

  let graph = Gfile.from_file infile in  
  
  let () = Gfile.export graph "dotGraphFrance" (fun x -> x)  in

  (* Test of Graph.map *)
  let graph2 = Graph.map graph (fun x -> x) int_of_string in

   print_endline "----------------------------------";
   print_endline " Ford Fulkerson algorithm started ";
   print_endline "----------------------------------"; 
   
   let flow = Ford_fulkerson.ford_fulk graph2 source sink in
   
   print_endline "----------------------------------";
   print_endline ("The maximum flow of people using public transportation is : " ^ (string_of_int flow));

   
  ()



 


