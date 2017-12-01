open Graph

type flow = int
type id = string
type gpath = (flow * id) list
(*To use this package the graph should have already been mapped to have int values on edges *)

(**

initialize flow to 0
path = findAugmentingPath(G, s, t)
while path exists:
    augment flow along path                 #This is purposefully ambiguous for now
    G_f = createResidualGraph()
    path = findAugmentingPath(G_f, s, t)
return flow

**)


let rec contains lst elt = match lst with
	| [] -> false
	| a::rest when a=elt -> true
	| _::rest -> contains rest elt




let findAugmentingPath myGraph sourceid sinkid = 

	(*We gather the vertex from the IDs*)
	let source = find_vertex myGraph sourceid in
	let sink = find_vertex myGraph sinkid in 

	let rec findAugmentingPath2 source precedingPath =

		(*Condidtion used to filter the list of edges : non null value and not already in the path*)
		let cond (a,b) = (not (contains precedingPath (a,b) )) && (a>0) in

		(*Function to test all arcs that are proven to be correct*)
		let rec tester_tous_les_arcs arcsCorrects =
			match arcsCorrects with
					(*If the list is empty we cannot find a path*)
					| [] -> raise Not_found 
					(*Else we try to find paths for all correct edges 
					and we stop when we have found a correct path*)
					| (a,b)::rest -> 
						begin
							try
								findAugmentingPath2 (find_vertex myGraph b) ((a,b)::precedingPath)
							with
								| Not_found -> tester_tous_les_arcs rest
						end
		in

		(*If the source is equal to the sink then it's finished*)
		match (source,sink) with
		| (a,b) when a=b -> precedingPath
		| _ -> 
			(*Else we search all correct edges (non null and non already in the path) 
			amongst all those starting from source*)
			begin
				let arcsCorrects = List.filter cond source.outedges in
				(*And we try to find a complete path with all of them*)
				tester_tous_les_arcs arcsCorrects		
			end
	in

	(*Call of the intern function with a null accumulator*)
	let finalpath = findAugmentingPath2 source [] in 
	(*The list in the accumulator has ro be reversed before being returned*)
	List.rev finalpath 

let createResidualGraph myGraph source augmentingPath =

	(*Declaration of the new graph to be returned*)
	let outgraph = new_graph () in
	
	(*These functions allow us to get the minimum flow from the path *)
	let rec find_min min = function
		| (e,id)::rest -> if (e < min) then (find_min e rest) else (find_min min rest)
		| [] -> min
	in  
	let min_of_AP = 
		match augmentingPath with
			| (e,id)::rest -> find_min e augmentingPath
			| [] -> raise Not_found
	in

	(*Remove an edge from the outedges list *)
	let rec remove elt lst =
		match lst with
			| e::rest when (e=elt) -> rest
			| a::rest -> a::(remove elt rest) 
			| [] -> []
	in
	
	(*Function to sub the min flow to the edge label *)
	let rec sub_value elt lst =
		match lst with
			| (e,id)::rest when ((e,id)=elt) -> ((e-min_of_AP),id)::rest
			| a::rest -> a::(sub_value elt rest) 
			| [] -> []
	in
	(*Function to add the min flow to the edge label *)
	let rec add_value elt lst =
		match lst with
			| (e,id)::rest when ((e,id)=elt) -> ((e+min_of_AP),id)::rest
			| a::rest -> a::(add_value elt rest) 
			| [] -> []
	in

	let rec maj_arcs id outedges_old inedges_old src path =
		match path with
			(*If we are in the edge in the path that arrives to the vertex we are considering...*)
			| (e,k)::rest when k=id -> 
				begin 		
				(*We update the value of the out vertex going to the  *)						
				let outedges_new = (if (e=min_of_AP) then (remove (e,id) outedges_old) else (sub_value (e,id) outedges_old)	) in
				
				let inedges_new = (if (contains inedges_old (e,k)) then (add_value (e,k) inedges_old) else ((e,k)::inedges_old)	) in
			
					(outedges_new,inedges_new)
				end
			(*If we reach the end of the path then the current node is not concerned by the change, we leave it as it is *)
			| [] -> (outedges_old, inedges_old)
			(*If the current node is not concerned by this edge of the path then we skip to the next one *)
			| (a,b)::rest -> maj_arcs b outedges_old inedges_old rest 
		match path with
			| (e,k)::rest when k=id -> 
				begin
					
	in
		
	let f1 info = 	
		(*We add this vertex to the new graph*)
		add_vertex outgraph info.label info.id;
		()				  
	in
	
	let f2 info = 	
		(*We get in a/b the new list of edges going out/in of this vertex for the updated graph*)
		let (a,b)=(maj_arcs info.id info.outedges info.inedges source augmentingPath) in
		(*And we add the new edges to it*)
		let rec add_a = function
			| (x,y)::rest -> add_edge outgraph info.id y x ; ()
			| [] -> ()
		in	
		let rec add_b = function
			| (x,y)::rest -> add_edge outgraph y info.id x ; ()
			| [] -> ()
		in
		add_a a; 
		add_b b;
		()				  
	in		

	(* We are going to iterate in the whole hashmap and apply the f functions *)
	(* First we add all the vertices *)
	v_iter myGraph f1;
	(* Then we add all the edges *)
	v_iter myGraph f2;
	outgraph

let ford_fulk myGraph source sink = (*Gerer not found*)
	let out_flow = 0 in

	out_flow

