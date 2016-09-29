(* Problems 1-4 use these type definitions: *)

type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail
				
(* Note that the grade might be absent (presumably because the student unregistered from the course). *)
				
(* 1.Write a function pass_or_fail of type {grade : int option, id : 'a} -> pass_fail that takes a final_grade (or, as the type indicates, a more general type) and returns pass if the grade field contains SOMEi for an i≥75 (else fail). *)
fun pass_or_fail({grade : g, id : _}) =
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

				
(* Write a function number_misgraded of type (pass_fail * final_grade) list -> int that indicates how many list elements are "mislabeled" where mislabeling means a pair (pass,x) where has_passed x is false or (fail,x) where has_passed x is true. *)
fun number_misgraded(xs) =
  case xs of
      [] => 0
    | (pass,grade)::xs' =>
      1 - number_passed [grade] + number_misgraded(xs')
    | _ => 
      number_passed [grade] + number_misgraded(xs')

