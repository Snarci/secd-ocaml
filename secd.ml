(* This is an OCaml editor.
Enter your program here and send it to the toplevel using the "Eval code"
    button. *)

(*Identificatori (ide), definiti con una striga, (Ide)*)
type ide = Ide of string;;
(*definizione del tipo (exp) delle espressioni da valutare*)
type exp = 
    Var of string 
  | Int of int
  | Bool of bool
  | Sum of exp * exp
  | Diff of exp * exp
  | Times of exp * exp 
  | If of bool * exp * exp 
  | Fun of exp*exp*exp
  | Apply of exp * exp
  | EIf
  | ESum  
  | EDiff 
  | ETimes 
  | App
  | Undefined 
    
;;
(*Sum = prim(sum)*)

let rec stringify = function
    Var(s) -> s
  | Int(n) ->(string_of_int n)
  | Bool (b)->(string_of_bool b)
  | Sum(e1,e2) -> "sum("^(stringify e1)^","^(stringify e2)^")"
  | Diff(e1,e2) -> "diff("^(stringify e1)^","^(stringify e2)^")"   
  | Times(e1,e2) -> "times("^(stringify e1)^","^(stringify e2)^")"  
  | If(e1,e2,e3) -> "if("^(string_of_bool e1)^","^(stringify e2)^","^(stringify e3)^")" 
  | Fun(e1,e2,e3) -> "fun("^(stringify e1)^","^(stringify e2)^","^(stringify e3)^")" 
  | Apply(e1,e2) -> "apply("^(stringify e1)^","^(stringify e2)^")" 
  | EIf -> "If" 
  | ESum -> "Prim(sum)" 
  | EDiff -> "Prim(diff)" 
  | ETimes -> "Prim(times)" 
  | App->"App"
  | Undefined->"Undefined" 
  | _->""
;;
let rec listToString l= match l with
  |[]->"[]"
  |hd::tl->(stringify hd)^"::"^(listToString tl);;
let rec envToString l= match l with
  |[]->"#"
  |(f,s)::tl->"("^(stringify f)^"/"^(stringify s)^")::"^(envToString tl);;
let rec transformSEC d = match d with 
  |[]->"" 
  |(s,e,c)::rep->
      "("^(listToString s)^","^(envToString e)^","^(listToString c)^")::[]"
      ^(transformSEC rep)^"\n"
      
(*funzione dell'ambiante, ad un identificatore si restituisce un eval*)
type env = exp -> exp
(*tipi restituiti dalla valutazione delle espressioni*)
  


(*ambiente vuoto, qualsiasi ide restituirà Undefined*)
let emptyenv = function (y:ide) -> Undefined;;
(* env è l'ambiente, x è il nome, la funzione apply restituisce
il valore di x in r *)
let rec applyenv(r,x) = match r with
  |[]-> Undefined
  |(y,vy)::tl->if(y=x) then vy else applyenv(tl,x);;
(*restituisce un ambiente che è il vecchio ambiente (r) con l'aggiunta della 
  associazione dell'ide (x) all'espressione (e)*)
let bind (r,x,e) = match r with
  |[]->[( x,e)]
  |hd::tl->( x,e)::hd::tl;;




let rec secd d = match d with
  (*sum*)
  
  |(s, e, (Int op)::tl)::tld->
      print_string (transformSEC d) ; secd (((Int op)::s,e,tl)::tld) 
  |((s, e ,Sum(op1,op2)::tl)::tld)->
      print_string (transformSEC d) ;secd ((s,e,(op1::op2::ESum::tl))::tld)        
  |((Int op1):: (Int op2)::tls,e,ESum::tlc)::tld->
      print_string (transformSEC d) ; secd (((Int (op1+op2))::tls,e,tlc)::tld)  
(*diff*)
  |((s, e ,Diff(op1,op2)::tl)::tld)->
      print_string (transformSEC d) ; secd ((s,e,(op1::op2::EDiff::tl))::tld)         
  |((Int op1):: (Int op2)::tls,e,EDiff::tlc)::tld->
      print_string (transformSEC d) ; secd (((Int (op1-op2))::tls,e,tlc)::tld)  
(*times*)
  |((s, e ,Times(op1,op2)::tl)::tld)->
      print_string (transformSEC d) ;  secd ((s,e,(op1::op2::ETimes::tl))::tld)           
  |((Int op1):: (Int op2)::tls,e,ETimes::tlc)::tld->
      print_string (transformSEC d) ;secd (((Int (op1*op2))::tls,e,tlc)::tld) 
(*boold if*)
  |(s, e, (Bool op)::tl)::tld->
      print_string (transformSEC d) ; secd (((Bool op)::s,e,tl)::tld)  
        
  |(s,e,If(t1,t2,t3)::tl)::tld->
      print_string (transformSEC d) ;  secd ((s, e ,(Bool t1)::EIf::t2::t3::tl )::tld) 
        
  |((Bool true)::tls,e,EIf::t2::t3::tlc)::tld->
      print_string (transformSEC d) ; secd ((tls,e,t2::tlc)::tld) 
        
  |((Bool false)::tls,e,EIf::t2::t3::tlc)::tld->
      print_string (transformSEC d) ; secd ((tls,e,t3::tlc)::tld) 
        
(*funzioni*)   
  |(s,e,Fun(f,x,t)::tl)::tld->
      print_string (transformSEC d) ;  secd ((Fun(f,x,t)::s,e,tl)::tld) 
        
  |(s,e,Apply(t1,t2)::tl)::tld->
      print_string (transformSEC d) ;  secd ((s,e,t1::t2::App::tl)::tld) 
        
  |(s,e,Var(x)::tl)::tld->
      print_string (transformSEC d) ;  secd (((applyenv(e,(Var(x))))::s,e,tl)::tld)
        
  |(v2::(Fun(f,x,t))::s,e,App::tl)::tld->
      print_string (transformSEC d) ;   secd (([], 
                                               bind((bind (e,x,v2)),f,(Fun(f,x,t))) ,
                                               t::[]
                                              )::(s,e,tl)::tld) 
  |(v::s,e,[])::(s',e',c')::tld->
      print_string (transformSEC d) ;   secd ((v::s',e',c')::tld) 
      
  |_ ->  print_string (transformSEC d) ;;

(*
secd (([],[],Sum(Sum(Int 1,Int 2),Int 7)::[])::[]) ;;


  secd (([],[],Apply(Fun (Var "f",Var "x",Sum(Var "x", Int 1) ),Sum(Int 1,Int 2))::[])::[]) ;;

  secd (([],[],Apply(Fun (Var "f",Var "x",Sum(Var "x", Int 7) ),Int 1)::[])::[]) ;;
  secd ((([],[],(Apply(Fun((Var "f"),(Var "x"),Apply(Var "x",Fun(Var "g",Var "y",Sum(Var "y",Int 1)))),
                       Fun(Var "h",Var "z",Apply(Var "z",Int 5))))::[]))::[]) ;;

*)
secd ((([],[],(Apply(Fun(Var "f",Var "x",Apply(Var "x",Apply(Fun(Var "g",Var "y",Var "y"), Int 1))),Fun(Var "h",Var "z",Times(Var "z",Int 4))))::[]))::[]) ;;


