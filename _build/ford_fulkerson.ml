open Graph

type flow = int
type id = string
type gpath = (flow * id) list
(*To use this package the graph should have already been mapped to have int values on edges *)


(* Finds a path from source to sink in the graph if it is possible *)
let findAugmentingPath myGraph sourceid sinkid = 	

	let rec findAugmentingPath2 sourceid2 precedingPath =
	
		(*We gather the vertex from the IDs*)
		let source = find_vertex myGraph sourceid2 in
		let sink = find_vertex myGraph sinkid in

		(*Condidtion used to filter the list of edges : not already in the path*)
		let cond x =
			(*Creates a list with only the destinations *)
			(*(because otherwise it would fail once the value of an edge has been changed)*)
			(*(we use the fact that every vertex can be in the path only once *)
			let b = List.map (fun (a,b) -> b) precedingPath in 
			( (not (List.mem (snd x) b)) && (not ((snd x)=sourceid)) ) 
		in

		(*Function to test all arcs that are proven to be correct*)
		let rec tester_tous_les_arcs arcs =
			match arcs with
					(*If the list is empty we cannot find a path*)
					| [] -> raise Not_found 
					(*Else we try to find paths for all correct edges 
					and we stop when we have found a correct path*)
					| (a,b)::rest -> 
						begin
							try
								findAugmentingPath2 b ((a,b)::precedingPath)
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
	let finalpath = findAugmentingPath2 sourceid [] in 
	(*The list in the accumulator has ro be reversed before being returned*)
	List.rev finalpath 



(* Return the maximum flow possible from an augmenting path *)
let getFlowFromPath augmentingPath= 
	let rec find_min min = function
		| (e,id)::rest -> if (e < min) then (find_min e rest) else (find_min min rest)
		| [] -> min
	in  
	match augmentingPath with
		| (e,id)::rest -> find_min e augmentingPath
		| [] -> raise Not_found
	

	(* Updates the graph with a given path *)
	let createResidualGraph myGraph augmentingPath =
			(*Declaration of the new graph to be returned*)
		let outgraph = new_graph () in
		let f1 info = 	
			(*We add this vertex to the new graph*)
			add_vertex outgraph info.label info.id;
			()				  
		in
		
		let rec add_edges id outedges_old =
		(*For all outedges of this vertex, if it is in the path then we update the edges, else we leave it as it is*)
		match outedges_old with
			| (label, id_arrivee)::rest -> 
				begin if (List.mem (label, id_arrivee) augmentingPath)(*If this outedge is part of the path *)
				then 
					begin
					(* We modify the outgoing edge (only if it should still exist)*)
					(if ((label-(getFlowFromPath augmentingPath))>0) 
						then add_edge outgraph id id_arrivee (label-(getFlowFromPath augmentingPath)) else () );
												(* We modify the incoming edge*)
					let incomingLabelOption = (find_edge myGraph id_arrivee id) in
					let incomingLabel =
						match incomingLabelOption with
							| None -> 0
							| Some a -> a
						in					
						(); (*add_edge outgraph id_arrivee id (incomingLabel+(getFlowFromPath augmentingPath));	*)			
					end
					else (add_edge outgraph id id_arrivee label) end;
					add_edges id rest
				| [] -> ()
			in
						let f2 info = 			
				(* For all vertices*)
				add_edges info.id info.outedges	;	
				()				  
			in		
					(* First we add all the vertices *)
			v_iter myGraph f1;
			(* Then we add all the edges *)
			v_iter myGraph f2;
		outgraph

(* Main algorithm *)
let ford_fulk myGraph source sink =
	

	let rec inner_ford_fulk myGraph source sink out_flow =
		try
			(* As long as we find a path we make one more iteration with the new graph and the augmented flow *)
			let aPath = findAugmentingPath myGraph source sink in	
			
			let b = List.map (fun (a,b) -> b) aPath in (*Creates a list with only the label values from the path *)
			let strg = String.concat ", " b in (*Creates a string from the list*)
			print_endline ("Path found : "^source^", "^strg); (*Displays the string *)
			
			print_endline ("Flow : "^(string_of_int (getFlowFromPath aPath)));
			
			inner_ford_fulk (createResidualGraph myGraph aPath) source sink (out_flow + (getFlowFromPath aPath))
		with
			(* When finding a path is no longer possible we return the final flow *)
			| Not_found -> out_flow	
	in 
	inner_ford_fulk myGraph source sink 0

