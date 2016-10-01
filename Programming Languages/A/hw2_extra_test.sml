use "hw2_extra.sml";

val test1_1 = pass_or_fail {id=7,grade= SOME 100} = pass
val test1_2 = pass_or_fail {id=7,grade= SOME 50} = fail
val test1_3 = pass_or_fail {id=7,grade= NONE} = fail

val test2_1 = has_passed {id=7,grade= SOME 100} = true
val test2_2 = has_passed {id=7,grade= SOME 50} = false
val test2_3 = has_passed {id=7,grade= NONE} = false

val test3 = number_passed [{id=1,grade = SOME 100},{id=2,grade=SOME 1}] = 1
val test4 = number_misgraded [(fail,{id=1,grade = SOME 100}),(pass,{id=2,grade=SOME 1})] = 2
											       
val test5_1 = tree_height(leaf) = 0
val test5_2 = tree_height(node {value=1,left=leaf,right=node {value=1,left=leaf,right=leaf}}) = 2

val test6_1 = sum_tree(leaf) = 0
val test6_2 = sum_tree(node {value=1,left=leaf,right=node {value=2,left=leaf,right=leaf}}) = 3

val test7 = gardener(node {value=leave_me_alone,left=node {value=leave_me_alone,left=leaf,right=leaf},right=node {value=prune_me,left=leaf,right=leaf}}) = node {value=leave_me_alone,left=node {value=leave_me_alone,left=leaf,right=leaf},right=leaf}

val test9 = is_positive ZERO = false

val test10_1 = (pred(ZERO); false) handle Negative => true
val test10_2 = pred(SUCC ZERO) = ZERO

val test11_1 = nat_to_int(ZERO) = 0
val test11_2 = nat_to_int(SUCC ZERO) = 1

val test12_1 = (int_to_nat(~1); false) handle Negative => true
val test12_2 = int_to_nat(0) = ZERO				   
val test12_3 = int_to_nat(1) = SUCC ZERO
val test12_4 = int_to_nat(2) = SUCC(SUCC ZERO)
				   
val test13_1 = add(ZERO,SUCC ZERO) = SUCC ZERO
val test13_2 = add(SUCC (SUCC ZERO),SUCC (SUCC ZERO)) = SUCC(SUCC(SUCC(SUCC ZERO)))

val test14_1 = sub(SUCC ZERO,ZERO) = SUCC ZERO
val test14_2 = (sub(ZERO,SUCC ZERO); false) handle Negative => true

val test15_1 = mult(ZERO,SUCC ZERO) = ZERO
val test15_2 = mult(SUCC (SUCC (SUCC ZERO)),SUCC (SUCC ZERO)) = SUCC(SUCC(SUCC(SUCC(SUCC(SUCC ZERO)))))

val test16_1 = less_than(ZERO,SUCC ZERO) = true
val test16_2 = less_than(ZERO,ZERO) = false
val test16_3 = less_than(SUCC ZERO,ZERO) = false

val test18_1 = contains(Elems [1,2,3],3) = true
val test18_2 = contains(Range {from=1,to=3},3) = true
val test18_3 = contains(Union (Elems [7,8,9],Range {from=1,to=3}),7) = true
val test18_4 = contains(Intersection (Elems [7,8,9],Range {from=1,to=3}),7) = false
val test18_5 = contains(Intersection (Elems [1,7,8,9],Range {from=1,to=3}),1) = true

val test19_1 = toList(Elems [1,2,3]) = [1,2,3]
val test19_2 = toList(Range {from=1,to=3}) = [1,2,3]
val test19_3 = toList(Union (Elems [7,8,9],Range {from=1,to=3})) = [7,8,9,1,2,3]
val test19_4 = toList(Intersection (Elems [7,8,9],Range {from=1,to=3})) = []
val test19_5 = toList(Intersection (Elems [1,7,8,9],Range {from=1,to=3})) = [1]	       

val test17_1 = isEmpty(Union (Elems [7,8,9],Range {from=1,to=3})) = false
val test17_2 = isEmpty(Intersection (Elems [7,8,9],Range {from=1,to=3})) = true
val test17_3 = isEmpty(Intersection (Elems [1,7,8,9],Range {from=1,to=3})) = false
										
