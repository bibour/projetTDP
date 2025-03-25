(* Semantics of instructions *)
open Lang
open Graphstruct
open Instr


  (* State of the program, essentially a graph structure and a binding in form of a table,
  and as convenience info an overapproximation of the maximal node number
  allocated in the graph (could in principle be recomputed each time)
  Nodes have a unique identifier (an int) which is incremented when creating new nodes.
  When deleting nodes, the max node number is currently not decremented, 
  thus does not reflect the number of nodes in the graph, but one could think
  of a garbage collector when running out of node identifiers.
   *)

(* Naive implementation of bindings as tables, ie. 
   a heading (variable list 'v list) and a list of lines containing nodes 
   that each have the same length as the variable list  *)

type ('v, 'n) table = Table of 'v list * 'n list list
  [@@deriving show]

type vname_nodeid_table = (vname, nodeid) table
  [@@deriving show]

let empty_table = Table([], [[]])

(* Add a single variable v, bound to a single node n, to a table,
  as during node creation (old interpretation, now superseded, 
  see create_node and add_var_mult_nodes_to_table) *)
let add_var_single_node_to_table v n (Table (vns, lns)) = 
    Table (v::vns, List.map (fun ln -> n::ln) lns)

(* Add multiple nodes contained in ns for a new variable v to a table,
  one node per line. ns and lns have to have the same length.  *)
let add_var_mult_nodes_to_table v ns (Table (vns, lns)) = 
      Table (v::vns, List.map2 (fun n ln -> n::ln) ns lns)
      


type attrib_val = fieldname * value
  [@@deriving show]
type attrib_struct = label * (attrib_val list)
  [@@deriving show]
      
type db_graph_struct = (Graphstruct.nodeid, attrib_struct, label) Graphstruct.db_graph
  [@@deriving show]
 

type state = State of db_graph_struct * (vname, nodeid) table * nodeid
let initial_state = State(empty_graph, empty_table, 0)

let create_node v lb (State(g, tab, mn)) = 
  let Table(_vns, lns) = tab in 
  let new_node_ids = List.init (List.length lns) (fun i -> mn +i) in 
  let new_nodes = List.init (List.length lns) (fun i -> DBN(mn + i , (lb, []))) in
  let new_tab = add_var_mult_nodes_to_table v new_node_ids tab in
  let new_graph = add_nodes_to_graph new_nodes g in 
  State (new_graph, new_tab, mn + 1)


(* TODO: complete following definition *)
let exec_instr (State (graph, table, next_id) as state) instr =
  match instr with
  | IActOnNode (CreateAct, v, lb) ->
      if Hashtbl.mem table v then state
      else
        let id = next_id in
        Hashtbl.add table v id;
        let graph' = create_node graph id lb in
        State (graph', table, next_id + 1)

  | IActOnNode (MatchAct, v, lb) ->
      (* Match ne modifie pas le graphe, mais dans une vraie version 
         tu pourrais récupérer l'ID correspondant au label *)
      state  (* À adapter selon ce que "Match" fait *)

  | IActOnRel (CreateAct, v1, lb, v2) ->
      if Hashtbl.mem table v1 && Hashtbl.mem table v2 then
        let id1 = Hashtbl.find table v1 in
        let id2 = Hashtbl.find table v2 in
        let graph' = create_rel graph id1 lb id2 in
        State (graph', table, next_id)
      else state

  | IActOnRel (MatchAct, v1, lb, v2) ->
      (* Comme pour MatchAct sur Node, si besoin d'un effet => l'ajouter *)
      state

  | IDeleteNode v ->
      if Hashtbl.mem table v then
        let id = Hashtbl.find table v in
        Hashtbl.remove table v;
        let graph' = delete_node graph id in
        State (graph', table, next_id)
      else state

  | IDeleteRel (v1, lb, v2) ->
      if Hashtbl.mem table v1 && Hashtbl.mem table v2 then
        let id1 = Hashtbl.find table v1 in
        let id2 = Hashtbl.find table v2 in
        let graph' = delete_rel graph id1 lb id2 in
        State (graph', table, next_id)
      else state

  | IReturn vars ->
      (* Ici, soit tu ignores (car RETURN sert juste à afficher),
         soit tu stockes quelque part le résultat si besoin *)
      state

  | IWhere expr ->
      (* WHERE va filtrer dans une exécution d'un bloc ou d'un programme complet,
         mais seul, il n'a pas d'effet sur l'état *)
      state

  | ISet (v, field, expr) ->
      if Hashtbl.mem table v then
        let id = Hashtbl.find table v in
        let graph' = set_field graph id field expr in
        State (graph', table, next_id)
      else state
  

let exec (NormProg(_tps, NormQuery(instrs))) = 
  List.fold_left exec_instr initial_state instrs

