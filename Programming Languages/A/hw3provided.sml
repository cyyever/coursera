(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(strs) =
  List.filter(fn str => Char.isUpper(String.sub(str,0))) strs

fun longest_string1(strs) =
  foldl(fn (str,acc_str) => if String.size(str)>String.size(acc_str) then str else acc_str) "" strs

fun longest_string2(strs) =
  foldl(fn (str,acc_str) => if String.size(str)>=String.size(acc_str) then str else acc_str) "" strs

fun longest_string_helper f strs =
  foldl(fn (str,acc_str) => if f(String.size(str),String.size(acc_str)) then str else acc_str) "" strs
       
val longest_string3 = fn strs => longest_string_helper(Int.>) strs		      
val longest_string4 = fn strs => longest_string_helper(Int.>=) strs

val longest_capitalized = longest_string1 o only_capitals

val rev_string = implode o rev o explode 

fun first_answer f xs =
  case xs of
      [] => raise NoAnswer
    | x::xs' =>
      case f(x) of
	  NONE => first_answer f xs'
       | SOME v => v

fun all_answers f xs =
  let
      fun remain_all_answers(acc,xs) =
	case xs of
	    [] => SOME acc
	  | x::xs' =>
	    case f(x) of
		NONE => NONE		    
	      | SOME v => remain_all_answers(acc@v,xs')
  in
      remain_all_answers([],xs)
  end

val count_wildcards = g (fn() => 1) (fn(str) => 0)

val count_wild_and_variable_lengths = g (fn() => 1) (fn(str) => String.size(str))

fun count_some_var(str,p) =
  g (fn() => 0) (fn(pstr) => if pstr=str then 1 else 0) p
    
fun check_pat(p) =
  let
      fun all_var(p) =
	case p of
	    Variable x => [x]
	  | TupleP ps         => List.foldl (fn (p,acc) => all_var(p)@acc) [] ps
	  | ConstructorP(_,p) => all_var(p)
	  | _                 => []

      fun has_repeats(strs) =
	case strs of
	    [] => false
	  | str::strs' =>
	    List.exists (fn str2 => str=str2) strs'
  in
    not(has_repeats(all_var(p)))  
  end

fun match(v,p) =
	case (v,p) of
	    (_,Wildcard) => SOME []
	  | (_,Variable x) => SOME [(x,v)]
	  | (Unit,UnitP) => SOME []
	  | (Const a,ConstP b) => if a=b then SOME[] else NONE
	  | (Tuple vs,TupleP ps) => (
	      if length(vs)=length(ps)
	      then
		  all_answers match (ListPair.zip(vs,ps))
	      else
		  NONE
	  )
	  | (Constructor(s1,v),ConstructorP(s2,p)) => if s1=s2 then match(v,p) else NONE
	  | _ => NONE

fun first_match v ps =
  (SOME (first_answer(fn p => match(v,p)) ps)) handle NoAnswer => NONE
								     
datatype typ = Anything (* any type of value is okay *)
	     | UnitT (* type for Unit *)
	     | IntT (* type for integers *)
	     | TupleT of typ list (* tuple types *)
	     | Datatype of string (* some named datatype *)

fun is_more_lenient_type(t1,t2) =
  if t1=t2
  then
      true
  else
      case (t1,t2) of 
	  (Anything,_) => true
	| (TupleT ts1,TupleT ts2) => 
	  length(ts1)=length(ts2) andalso foldl (fn (ts,acc) => acc andalso is_more_lenient_type ts) true (ListPair.zip(ts1,ts2))
	| _ => false
    			       
fun pattern_type(conts,p) =
  case p of
      Wildcard => SOME Anything
    | Variable x => SOME Anything
    | UnitP => SOME UnitT		     
    | ConstP a => SOME IntT
    | TupleP ps => (
      	case ps of
	    [] => SOME (TupleT [])
	  | p::ps' => (
	      case (pattern_type(conts,p),pattern_type(conts,TupleP ps')) of
		  (SOME t1,SOME (TupleT t2)) => SOME (TupleT([t1]@t2))
		| _ => NONE
	  )
    )
    | ConstructorP(s,p) => (
	case pattern_type(conts,p) of
	    NONE => NONE
	  | SOME t => ( 
	      case List.find (fn (cont_name,_,_) => cont_name=s) conts of
		  NONE => NONE
		| SOME (_,ret_tname,arg_t) => (
		    case is_more_lenient_type(arg_t,t) of
			false => NONE
		      | true => SOME (Datatype ret_tname)
 		)
	  )
    )
			      
fun get_common_type(t1,t2) =
  if t1=t2
  then
      SOME t1
  else
      case (t1,t2) of
	   (Anything,_) => SOME t2 
	 | (_,Anything) => SOME t1
	 | (TupleT ts1,TupleT ts2) => (
	     if length(ts1) = length(ts2)
	     then
		 foldl (fn ((t1,t2),acc) => 
			   case (acc,get_common_type(t1,t2)) of
			       (SOME (TupleT a),SOME b) => SOME (TupleT(a@[b]))
			     | _ => NONE
		       ) (SOME(TupleT[])) (ListPair.zip(ts1,ts2))
	     else
		 NONE
	 )
	 | _=> NONE		  
	
fun typecheck_patterns(conts,ps) =
  case ps of
      [] => NONE
    | _ => foldl (fn (p,t) =>
		     case (pattern_type(conts,p),t) of
			 (SOME t1,SOME t2) => get_common_type(t1,t2)
		       | _ => NONE
		 ) (SOME Anything) ps
		 
