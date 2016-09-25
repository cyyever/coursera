use "hw1_extra.sml";

val test1 = alternate [1,2,3,4] = ~2
val test2 = min_max [1,2,3,4,5,4,3,2,1] = (1,5)
val test3 = cumsum [1,4,20] = [1,5,25]
val test4_1 = greeting (SOME "cyy") = "Hello there, cyy!"
val test4_2 = greeting NONE = "Hello there, you!"
val test5 = repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]
val test6_1 = addOpt (SOME 1,SOME 2) = SOME 3
val test6_2 = addOpt (SOME 1,NONE) = NONE
val test7_1 = addAllOpt [SOME 1,NONE,SOME 2] = SOME 3
val test7_2 = addAllOpt [] = NONE
val test7_3 = addAllOpt [NONE,NONE] = NONE
val test8_1 = any [] = false
val test8_2 = any [false,true] = true
val test8_3 = any [false,false] = false
val test9_1 = all [] = true
val test9_2 = all [false,true] = false
val test9_3 = all [true,true] = true
val test10 = zip ([1,2,3], [4, 6]) = [(1,4), (2,6)]
val test11 = zipRecycle ([1,2,3], [1, 2, 3, 4, 5, 6, 7]) = [(1,1), (2,2), (3, 3), (1,4), (2,5), (3,6), (1,7)];
val test12_1 = zipOpt ([1,2,3], [4, 6]) = NONE
val test12_2 = zipOpt ([1,2], [4, 6]) = SOME [(1,4),(2,6)]
val test13_1 = lookup ([("a" ,1),("b",2),("c",3)],"b") = SOME 2
val test13_2 = lookup ([("a" ,1),("b",2),("c",3)],"d") = NONE
val test14 = splitup [1,~1,2,~2,3,~3,4] = ([1,2,3,4],[~1,~2,~3])
val test15 = splitAt ([1,~1,2,~2,3,~3,4],3) = ([3,4],[1,~1,2,~2,~3])
val test16_1 = isSorted [1,~1,2,3,4] =  false
val test16_2 = isSorted [8,99] = true
val test16_3 = isSorted [] = true
val test17_1 = isAnySorted [1,2,3] = true
val test17_2 = isAnySorted [3,2,1] = true
val test18 = sortedMerge ([1,4,7], [5,8,9]) = [1,4,5,7,8,9]
val test19 = qsort [8,5,2,6,7,1,9,4,3] = [1,2,3,4,5,6,7,8,9]
val test20 = divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])
val test21 = not_so_quick_sort [1,4,7,5,8,9] = [1,4,5,7,8,9]
val test22_1 = fullDivide (2,40) = (3, 5)
val test22_2 = fullDivide (3,10) = (0, 10)
val test23_1 = factorize(20) = [(2,2), (5,1)]
val test23_2 = factorize(36) = [(2,2), (3,2)]
val test23_3 = factorize(1) = []
val test24 = multiply [(2,2), (3,2)] = 36
val test25_1 = all_products([(2,2)]) = [1,2,4]
val test25_2 = all_products([(2,2), (5,1)]) = [1,2,4,5,10,20]

