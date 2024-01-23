(* a simple program to generate more testcases for cmsi-585-hw1 *)
(* in the future, I might want to parameterize the grammar      *)
(* but right now, I'm just hardcoding them                      *)
(* *
 * The grammar we want to use:
 * P  -> SL $$
 * SL -> S ; SL | epsilon
 * S  -> if (C) {SL} else {SL}
 *    |  while (C) {SL} 
 *    |  id := E
 *    | print (E)
 * C  -> E R E | true | false
 * E  -> T TT
 * TT -> B T TT | epsilon
 * T  -> (E) | id | read id | num
 * R  -> = | < | >
 * B  -> + | -
 * digit = [0-9]
 * id    = [a-z]([a-z]|digit)*
 * num   = digit+ 
 * *)
exception Illegal_expansion of string

type symbol = NT of string | T of string

type rule = Rule of (symbol list)
type production = Production of (symbol * (rule list))

module NonTerm = struct
  type t = symbol
  let compare s1 s2 = 
    match s1, s2 with 
    | NT n1, NT n2 -> String.compare n1 n2
    | _, _ -> raise (Illegal_expansion "cannot compare")
end

module GrammarMap = Map.Make (NonTerm)


(* nonterminals *)
let p  = NT "P"
let sl = NT "SL"
let s  = NT "S"
let c  = NT "C"
let e  = NT "E"
let tt = NT "TT"
let t  = NT "T"
let r  = NT "R"
let b  = NT "B"

(* terminals *)
let _eps       = T ""
let _semicolon = T ";"
let _if        = T "if"
let _else      = T "else"
let _while     = T "while"
let _true      = T "true"
let _false     = T "false"
let _read      = T "read"
let _print     = T "print"
let _assign    = T ":="
let _lbracket  = T "{"
let _rbracket  = T "}"
let _lparen    = T "("
let _rparen    = T ")"
let _plus      = T "+"
let _minus     = T "-"
let _eq        = T "="
let _lt        = T "<"
let _gt        = T ">"
let _space     = T " "

(* TODO: needs to be more flexible *)
let _id        = T "var"
let _num       = T "42"

(*
let nonterminals = [p; sl; s; c; e; tt; t; r; b]
let terminals    = [_eps; _semicolon; _if; _else; _while; _true; _false;
                    _read; _print; _assign; _lbracket; _rbracket; _lparen;
                    _rparen; _plus; _minus; _eq; _lt; _gt]
*)

let grammar = GrammarMap.empty 
            |> GrammarMap.add p  (Production (p,  [Rule [sl]]))
            |> GrammarMap.add sl (Production (sl, [Rule [s; _semicolon; sl];
                                                   Rule [_eps]]))
            |> GrammarMap.add s  (Production (s,  [Rule [_if; _lparen; c; _rparen; 
                                                         _lbracket; sl; _rbracket; 
                                                         _else; _lbracket; sl; _rbracket];
                                                   Rule [_while; _lparen; c; _rparen;
                                                         _lbracket; sl; _rbracket];
                                                   Rule [_id; _assign; e];
                                                   Rule [_print; _lparen; e; _rparen]]))
            |> GrammarMap.add c  (Production (c,  [Rule [e; r; e];
                                                   Rule [_true];
                                                   Rule [_false]]))
            |> GrammarMap.add e  (Production (e,  [Rule [t; tt]]))
            |> GrammarMap.add tt (Production (tt, [Rule [b; t; tt];
                                                   Rule [_eps]]))
            |> GrammarMap.add t  (Production (t,  [Rule [_lparen; e; _rparen];
                                                   Rule [_id];
                                                   Rule [_read;_space; _id];
                                                   Rule [_num]]))
            |> GrammarMap.add r  (Production (r,  [Rule [_eq];
                                                   Rule [_lt];
                                                   Rule [_gt]]))
            |> GrammarMap.add b  (Production (b,  [Rule [_plus];
                                                   Rule [_minus]]))



let string_of_symbol s =
  match s with
  | NT name
  | T  name -> name

let randomly_choose l =
    Random.self_init () ; List.nth l (Random.int (List.length l))

let random_whitespace _ =
  let whitespace = [" "; ""; "\t"; "\n"] in
  randomly_choose whitespace


let rec string_of_symbol_list (sl:(symbol list)) =
  match sl with
  | [] -> ""
  | hd :: tl -> (string_of_symbol hd)^(random_whitespace ())^(string_of_symbol_list tl)
 

(* randomly choose a rule from the production *)
let choose_rule (p : production) : rule =
  match p with
  | Production (s, rules) ->
    match s with
    | T t -> raise (Illegal_expansion ("cannot expand terminal"^t))
    | NT _ -> randomly_choose rules

let expand (nt : symbol) : (symbol list) = 
    let p = GrammarMap.find nt grammar in
    match (choose_rule p) with
    | Rule rl -> rl

let rec derive (sl : (symbol list)) : (symbol list) =
    match sl with
    | [] -> []
    | hd :: tl -> match hd with
                  | T _ -> hd::(derive tl)
                  | NT _ -> let new_hd = expand hd in
                             derive (List.append new_hd tl)
                
let () = print_endline (string_of_symbol_list (derive [p]))
