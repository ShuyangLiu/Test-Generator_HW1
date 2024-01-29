module NonTerminal = struct
  type t = string
  let compare = String.compare
end

module Terminal = struct
  type t = {
    name : string;
    content : string option;
  }
  let compare t1 t2 = String.compare t1.name t2.name
end

module Symbol = struct
  type t = NT of NonTerminal.t | T of Terminal.t
end

module Production = struct
  type t = {
    lhs : NonTerminal.t;
    rhs : Symbol.t list;
  }
  let get_lhs p = p.lhs
  let get_rhs p = p.rhs
end

module Grammar = struct
  module GrammarMap = Map.Make(NonTerminal)

  (* a Grammar is a map from NonTerminal to a Production list *)
  type t = (Production.t list) GrammarMap.t

  let empty = GrammarMap.empty

  (* add a single rule to a grammar map *)
  let add_rule gmap rule : (Production.t list) GrammarMap.t =
    let lhs_key = Production.get_lhs rule in
    let res = GrammarMap.find_opt lhs_key gmap in
    match res with
    | Some p_lst -> GrammarMap.add lhs_key (rule::p_lst) gmap
    | None -> GrammarMap.add lhs_key [rule] gmap

  (* add all rules from a list and construct a grammar *)
  let add_all_rules l = List.fold_left add_rule empty l
end




