open Graph
type id = string
type flow = int

(* To use this package the graph should have already been mapped to have int values on edges *)
(* (We think that it is a better solution than always giving an "int of e" method) *)

(*type gpath*)
type gpath = (flow * id) list(*On passe l'argument en public le temps du test de findAugmentingPath*)

(* Finds an augmenting path in the graph *)
val findAugmentingPath: ('v, flow) graph -> id -> id -> gpath

(* Return the maximum flow possible from an augmenting path *)
val getFlowFromPath: gpath -> flow

(* Creates the residual graph from the initial one and the augmenting path *)
val createResidualGraph: ('v, flow) graph -> gpath -> ('v, flow) graph

(* Research of maximum flow in the graph from source to sink using Ford Fulkerson algorithm *)
val ford_fulk: ('v,flow) graph -> id -> id -> flow
