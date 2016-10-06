use "hw3_extra.sml";

val test1a = compose_opt
		(fn x =>
		    case x of
			0=> NONE
		      | v => SOME (v+1))
		(fn x =>
		    case x of
			0=> NONE
		      | v => SOME (v-1)) 5 = SOME 5
					     
val test1b = compose_opt
		(fn x =>
		    case x of
			0=> NONE
		      | v => SOME (v+1))
		(fn x =>
		    case x of
			0=> NONE
		      | v => SOME (v-1)) 1 = NONE

val test2 = do_while (fn x => x div 2) (fn x => x mod 2 <> 1) 24 = 3

val test3 = factorial 5 = 120

val test4 = fixed_point (fn x => if x < 10 then x+1 else x-1) 5 = 5

val test5 = map2 (fn x => x+1) (1,2) = (2,3)

fun f n = [n, 2 * n, 3 * n]
val test6 = app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9]

val test7 = my_foldr Int.+ 100 [1,2,3] = 106
					   
val test8 = partition (fn x => x>=5) [1,2,3,4,5,6,7] = ([5,6,7],[1,2,3,4])

val test9 = unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]

val test10 = factorial2 5 = 120

val test11 = my_map (fn x => x+1) [0,1,2] = [1,2,3]

val test12 = my_filter (fn x => x>5) [0,1,2,5,6,7] = [6,7]
				
val test13 = my_foldl Int.div 2 [4,16,64] = 8

val test14a = tree_map (fn x => x+1) (node(5,leaf,leaf)) = node(6,leaf,leaf) 
    
val test14b = tree_fold_preorder (Int.+) 1 (node(5,leaf,leaf)) = 6

val test14c = tree_filter (fn x=> x<10) (node(5,leaf,node(100,leaf,leaf))) = node(5,leaf,leaf)
