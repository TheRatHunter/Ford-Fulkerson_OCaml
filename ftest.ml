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
  print_endline "-----------------------------------";
  print_endline "      Mapping test started...     ";
  let graph2 = Graph.map graph int_of_string int_of_string in
  let graph3 = Graph.map graph2 (fun x -> 10*x) (fun x -> x+1) in     
  let graph4 = Graph.map graph3 string_of_int string_of_int in
  
  
  (* Test of export *)
  (*
  let () = Gfile.write_file outfile graph4 in
  *)
  let () = Gfile.export graph4 "dotGraphAfter" (fun x -> x)  in
  let () = Gfile.export graph "dotGraphBefore" (fun x -> x)  in
  
  print_endline "Dot graph files have been created !";
  
  
  (* Test of findAugmentingPath*)
  print_endline "-----------------------------------";
  print_endline "   Augmenting path research test  "; 
    
  let augmentingPath = (Ford_fulkerson.findAugmentingPath graph2 source sink) in
  let b = List.map (fun (a,b) -> b) augmentingPath in (*Creates a list with only the label values from the path *)
  let strg = String.concat ", " b in (*Creates a string from the list*)
  print_endline ("Path found : "^source^", "^strg); (*Displays the string *)
  
  (*Test of createResidualGraph*)
  print_endline "-----------------------------------";
  print_endline "   Residual graph creation test   ";
  
  let residualGraph = Ford_fulkerson.createResidualGraph graph2 augmentingPath in
  let graph5 = Graph.map residualGraph string_of_int string_of_int in
  let () = Gfile.write_file outfile graph5 in
  let () = Gfile.export graph5 "notredotgraphOut" (fun x -> x)  in
  print_endline "A new graph file has been created !"; 
  
  print_endline "-----------------------------------";
  print_endline " Ford Fulkerson algorithm started ";
  print_endline "-----------------------------------"; 

  (* Test of Ford_Fulkerson *)
  let flow = Ford_fulkerson.ford_fulk graph2 source sink in
  print_endline "-----------------------------------"; 
  print_endline ("The max flow found for this graph is of : " ^ (string_of_int flow));
  print_endline "-----------------------------------"; 
  
  
  
  ()



 


