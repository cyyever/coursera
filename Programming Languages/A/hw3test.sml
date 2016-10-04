(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","cd","C"] = "bc"

val test3 = longest_string2 ["A","bc","cd","C"] = "cd"

val test4a = longest_string3 ["A","bc","cd","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7a = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test7b = (first_answer (fn x => if x > 100 then SOME x else NONE) [1,2,3,4,5];false) handle NoAnswer => true
														
val test8a = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test8b = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]
											 
val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test10a = check_pat (Variable("x")) = true

val test11a = match (Const(1), UnitP) = NONE

val test11b = match (Const(1), Variable "x" ) = SOME [("x",Const(1))]

val test11c = match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]) = NONE
					 
val test12a = first_match Unit [UnitP] = SOME []
					      
val test12b = first_match (Const 1) [UnitP] = NONE

val test13a = typecheck_patterns([],[TupleP[Variable("x"),Variable("y")],TupleP[Wildcard,Wildcard]]) = SOME (TupleT[Anything,Anything])

val test13b = typecheck_patterns([],[TupleP[Wildcard,Wildcard],TupleP[Wildcard,TupleP[Wildcard,Wildcard]]]) = SOME (TupleT[Anything,TupleT[Anything,Anything]])

val test13c = typecheck_patterns([],[UnitP,ConstP 10]) = NONE

val test13d = typecheck_patterns([],[]) = NONE
					      
val test13e = typecheck_patterns([],[UnitP,Variable "aaa"]) = SOME UnitT
