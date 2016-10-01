(* Problems 1-4 use these type definitions: *)

type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail
				
(* Note that the grade might be absent (presumably because the student unregistered from the course). *)
				
(* 1.Write a function pass_or_fail of type {grade : int option, id : 'a} -> pass_fail that takes a final_grade (or, as the type indicates, a more general type) and returns pass if the grade field contains SOMEi for an i≥75 (else fail). *)
fun pass_or_fail({grade = g, id = _}) =
  case g of
      SOME i =>
      if i>=75
      then
	  pass
      else
	  fail
    | _ => fail

(* 2.Using pass_or_fail as a helper function, write a function has_passed of type {grade : int option, id : 'a} -> bool that returns true if and only if the the grade field contains SOMEi for an i≥75. *)	      
fun has_passed(grade) =
  case pass_or_fail(grade) of
      pass => true
    | fail => false

(* 3.Using has_passed as a helper function, write a function number_passed that takes a list of type final_grade (or a more general type) and returns how many list elements have passing (again, ≥75) grades. *)
fun number_passed(gs) =
  case gs of
      [] => 0
    | g::gs' =>
      case pass_or_fail(g) of
	  pass => 1+number_passed(gs')
       | _ => number_passed(gs') 

				
(* 4.Write a function number_misgraded of type (pass_fail * final_grade) list -> int that indicates how many list elements are "mislabeled" where mislabeling means a pair (pass,x) where has_passed x is false or (fail,x) where has_passed x is true. *)
fun number_misgraded(xs) =
  case xs of
      [] => 0
    | (pass,g)::xs' => 
	1 - number_passed [g] + number_misgraded(xs')
    | (fail,g)::xs' => 
	number_passed [g] + number_misgraded(xs')

(* Problems 5-7 use these type definitions: *)
					    
datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me


(* 5. Write a function tree_height that accepts an 'a tree and evaluates to a height of this tree. The height of a tree is the length of the longest path to a leaf. Thus the height of a leaf is 0. *)
fun tree_height(t) =
  case t of
      leaf => 0
    | node {value=_,left=t_l,right=t_r} => 1+Int.max(tree_height(t_l),tree_height(t_r)) 

(* 6. Write a function sum_tree that takes an int tree and evaluates to the sum of all values in the nodes. *)
fun sum_tree(t) =
  case t of
      leaf => 0
    | node {value=v,left=t_l,right=t_r} => v+sum_tree(t_l)+sum_tree(t_r)

(* 7. Write a function gardener of type flag tree -> flag tree such that its structure is identical to the original tree except all nodes of the input containing prune_me are (along with all their descendants) replaced with a leaf. *)
fun gardener(t) =
  case t of
      node {value=leave_me_alone,left=t_1,right=t_r} => node {value=leave_me_alone,left=gardener(t_1),right=gardener(t_r)}
   | _ => leaf 
						  
(* Problems 9-16 use this type definition for natural numbers: *)

datatype nat = ZERO | SUCC of nat
	      
(* 9. Write is_positive : nat -> bool, which given a "natural number" returns whether that number is positive (i.e. not zero). *)
fun is_positive(n) =
  case n of
      ZERO => false
    | _ => true
	       
(* 10. Write pred : nat -> nat, which given a "natural number" returns its predecessor. Since 0 does not have a predecessor in the natural numbers, throw an exception Negative (will need to define it first). *)
	   
exception Negative
fun pred(n) =
  case n of
      ZERO => raise Negative
    | SUCC n' => n'
			    
(* 11. Write nat_to_int : nat -> int, which given a "natural number" returns the corresponding int. For example, nat_to_int (SUCC (SUCC ZERO)) = 2. (Do not use this function for problems 13-16 -- it makes them too easy.) *)
fun nat_to_int(n) =
  case n of
      ZERO => 0
    | SUCC n' => nat_to_int(n')+1

(* 12. Write int_to_nat : int -> nat which given an integer returns a "natural number" representation for it, or throws a Negative exception if the integer was negative. (Again, do not use this function in the next few problems.) *)
fun int_to_nat(n) =
  case n < 0 of
      true => raise Negative
    | false =>
      case n of
	  0 => ZERO
	| _ => SUCC(int_to_nat(n-1))
   
(* 13. Write add : nat * nat -> nat to perform addition. *)
fun add(n,m) =
  case n of
      ZERO => m
    | SUCC n' => SUCC(add(n',m))
		    
(* 14. Write sub : nat * nat -> nat to perform subtraction. (Hint: Use pred.) *)
fun sub(n,m) =
  case m of
      ZERO => n
    | SUCC m' => pred(sub(n,m'))

(* 15. Write mult : nat * nat -> nat to perform multiplication. (Hint: Use add.) *)
fun mult(n,m) =
  case m of
      ZERO => ZERO
    | SUCC m' => add(n,mult(n,m'))

(* 16. Write less_than : nat * nat -> bool to return true when the first argument is less than the second. *)
fun less_than(n,m) =
  (sub(n,m);false) handle Negative => true

(* The remaining problems use this datatype, which represents sets of integers: *)
datatype intSet = 
	 Elems of int list (*list of integers, possibly with duplicates to be ignored*)
	 | Range of { from : int, to : int }  (* integers from one number to another *)
	 | Union of intSet * intSet (* union of the two sets *)
	 | Intersection of intSet * intSet (* intersection of the two sets *)
					    
(* 18. Write contains: intSet * int -> bool that returns whether the set contains a certain element or not. *)
fun contains(set,n) =
  case set of
      Elems ns => (
       let
	   fun list_contains(xs) =
	     case xs of
		 [] => false
	      |  x::xs'=> x=n orelse list_contains(xs') 
       in
	   list_contains(ns)
       end
   )
    | Range {from=a,to=b} => n>=a andalso n<=b
    | Union (a,b) => contains(a,n) orelse contains(b,n)
    | Intersection (a,b) => contains(a,n) andalso contains(b,n)
							  
(* 19. Write toList : intSet -> int list that returns a list with the set's elements, without duplicates. *)
fun toList(set) =
  let
      fun append(n,sub_set) =
	case contains(sub_set,n) of
	    true => toList(sub_set)
	  | false => n::toList(sub_set)
  in
      case set of
	  Elems ns => (
	   case ns of
	       [] => []
	     | n::ns' => append(n,Elems ns')
       )
	| Range {from=a,to=b} => (
	    case a<=b of
		true => a::toList(Range{from=a+1,to=b})
	      | false => []
	)
	| Union (a,b) => (
	    case toList(a) of
		[] => toList(b)
	      | n::ns' => append(n,Union(Elems(ns'),b))
	)
	| Intersection (a,b) => (
	    case toList(a) of
		[] => []
	      | n::ns' =>
		let
		    val sub_set = Elems(toList(b))
		in
		    if contains(sub_set,n)
		    then
			n::toList(Intersection(Elems ns',sub_set))
		    else
			toList(Intersection(Elems ns',sub_set))
		end		    				       
	)			   
  end

(* 17. Write isEmpty : intSet -> bool that determines if the set is empty or not. *)
fun isEmpty(ns) =
  null(toList(ns))
