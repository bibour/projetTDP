open Graphstruct
open Lang
open Instr
 
type environment = { types:  db_tp; bindings: (vname * label) list }
  [@@deriving show]

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)
  
exception FieldAccError of string
exception TypeError of string


type tc_result = (environment, string list) result

(* Functions for manipulating the environment *)

let add_var vn t (env:environment) = 
  {env with bindings = (vn,t)::env.bindings}

let remove_var vn env = 
  {env with bindings = List.remove_assoc vn env.bindings}









(* TODO: add more auxiliary functions here *)

(*

(*renvoie le type de noeud associé à la variable vn dans l'environnement env*)
let get_node_type env vn= List.assoc_opt vn env.bindings

(*prend (DBG(ntdecls,_)) contenant une liste de déclarations de nœuds et un type de nœud (nt), et cherche dans la liste ntdecls celle qui correspond à ce type*)
let get_node_decl (DBG(ntdecls,_)) nt= List.find_opt (fun (DBN(n,_))->n=nt) ntdecls

(*cherche le type d'un attribut attrib dans la déclaration d'un nœud*)
let attrib_tp (DBN(_,attrib)) fn= List.assoc_opt fn attrib

*)

(*ectrait les types de noeud de la liste des déclarations*)
let nt_list ntdecls= (List.map (fun (DBN(n, _)) -> n) ntdecls)
let rt_list rtdecls=(List.map(fun(DBR(n1,r,n2))->(n1,r,n2)) rtdecls)

(* retoçurne true si il n'y a pas de doublons dans la liste*)
let rec no_doublons = function
| [] -> true
| (x :: xs) -> not (List.mem x xs) && (no_doublons xs);;


(* vérifie que les types de noeuds sont uniques dans la liste de leurs déclarations *)
let node_type_unique ntdecls = 
no_doublons (nt_list ntdecls)

(* vérifie que les relations sont uniques dans la liste de leurs déclarations *)
let relation_type_unique rtdecls=
no_doublons(rt_list rtdecls)









(* TODO: fill in details *)

(*on veut que check_graph_types vérifie que chaque noeud est unique ET que chaque relation soit entre des types de noeudds existants*)
let check_graph_types (DBG (ntdecls, rtdecls)) = 
  (*On vérifie si il n'y à pas de doublons dans les types de noeuds et les relations sinon on renvoie une erreur*)
    if not(node_type_unique ntdecls) then Result.Error " Doublons dans les déclaratiosn de types de noeuds"
    else if not(relation_type_unique rtdecls) then Result.Error " Doublons dans les déclaratiosn de relations"
  (* vérification de chaque relation, une erreur est renvoyée dès qu'une relation référence un type non déclaré *)
    else let liste_noeuds = nt_list ntdecls in
      let rec check_relations = function
    | [] -> None
    | DBR(n1, r, n2) :: rest ->
        if not(List.mem n1 liste_noeuds) then
          Some ("Pour la relation (:"^n1^") -[:"^r^"]-> (:"^n2^"), ce noeud n'est pas déclaré : " ^n1)
        else if not(List.mem n2 (liste_noeuds)) then
          Some ("Pour la relation (:"^n1^") -[:"^r^"]-> (:"^n2^"), ce noeud n'est pas déclaré : " ^n2)
        else
          check_relations rest in
          match check_relations rtdecls with
          |Some erreur -> Result.Error erreur
          |_ -> Result.Ok (DBG(ntdecls,rtdecls))
  




(* TODO: fill in details *)
let rec tp_expr env = function
  Const v -> (match v with
      |BoolV _ -> BoolT
      |StringV _-> StringT
      |IntV _ -> IntT)
  | AttribAcc (vn, fn) -> IntT
  | BinOp (bop, e1, e2) -> tp_expr env e1

(* check expression e with expected type et in environment env *)
let check_expr e et env : tc_result = 
  try 
    if tp_expr env e = et
    then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et)]
  with 
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]
  

  let tc_instr (i: instruction) (env: environment) : tc_result =
    match i with
    | IActOnNode (act, vn, lb) -> 
        let (DBG(ntdecls, _)) = env.types in
        (* Le type de nœud doit exister *)
        Printf.printf "%s\n" (show_environment env);
        if List.exists (fun (DBN(lbl, _)) -> lbl = lb) ntdecls then
          
          
              
              Result.Ok (add_var vn lb env)
        else
          Result.Error ["Type de nœud non déclaré: " ^ lb]
  
    | IActOnRel (act, v1, rel, v2) ->
        let (DBG(_, rtdecls)) = env.types in
        (match (List.assoc_opt v1 env.bindings, List.assoc_opt v2 env.bindings) with
        | Some t1, Some t2 ->
            if List.exists (fun (DBR(n1, r, n2)) -> r = rel && n1 = t1 && n2 = t2) rtdecls then
              Result.Ok env
            else
              Result.Error ["Relation non déclarée ou types incompatibles pour "^v1^"-["^rel^"]->"^v2]
        | _ -> Result.Error ["Variables " ^ v1 ^ " ou " ^ v2 ^ " non déclarées"])
  
    | IDeleteNode vn ->
        if List.mem_assoc vn env.bindings then Result.Ok env
        else Result.Error ["Variable "^vn^" non déclarée"]
  
    | IDeleteRel (v1, rel, v2) ->
        if List.mem_assoc v1 env.bindings && List.mem_assoc v2 env.bindings then Result.Ok env
        else Result.Error ["Variables "^v1^" ou "^v2^" non déclarées"]
  
    | IReturn vns ->
        let undeclared = List.filter (fun vn -> not (List.mem_assoc vn env.bindings)) vns in
        if undeclared = [] then Result.Ok env
        else Result.Error (List.map (fun vn -> "Variable "^vn^" non déclarée") undeclared)
  
    | IWhere expr ->
      (*
        check_expr expr BoolT env
  *)
        Result.Ok env
    | ISet (vn, fn, e) ->
        let (DBG(ntdecls, _)) = env.types in
        match List.assoc_opt vn env.bindings with
        | None -> Result.Error ["Variable "^vn^" non déclarée"]
        | Some lbl ->
            (match List.find_opt (fun (DBN(l, _)) -> l = lbl) ntdecls with
            | None -> Result.Error ["Type de nœud " ^ lbl ^ " non trouvé dans la déclaration"]
            | Some (DBN(_, fields)) ->
                (match List.assoc_opt fn fields with
                | None -> Result.Error ["Champ "^fn^" non déclaré pour le type "^lbl]
                | Some expected_tp -> check_expr e expected_tp env))
  

(* type check list of instructions and stop on error *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs


  (* TODO: typecheck instructions *)
  let typecheck_instructions continue gt instrs np = 
    let r = tc_instrs_stop gt instrs in
    match r with
    | Result.Error etc -> 
        Printf.printf "%s\n" (String.concat "\n" etc); 
        failwith "stopped"
    | _ -> np
  

  (* Typecheck program; 
     If continue is true, continue typechecking even 
     when errors have been discovered (can be ignored in a first version) *)  
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                        failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np
  



let test_types =  
    DBG ([( DBN ("P", [("nom", Lang. StringT ); ("age", Lang.IntT )]));
          (DBN ("E", [("nom", Lang. StringT ); ("pme", Lang. BoolT )]))] ,
      [( DBR ("P", "ami", "P"));
       (DBR ("P", "emp", "E"));
        (DBR ("E", "f", "E"))]) ;;

