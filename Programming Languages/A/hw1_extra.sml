(* Write a function alternate : int list -> int that takes a list of numbers and adds them with alternating sign. For example alternate [1,2,3,4] = 1 - 2 + 3 - 4 = -2.*)
fun alternate (xs:int list) =
  let
      fun alternate_sign (sign:int ,xs :int list) =
	if null xs
	then
	    0
	else 
	    (hd xs)*sign+alternate_sign(sign*(~1),tl xs)
  in
      alternate_sign(1,xs)
  end

(* Write a function min_max : int list -> int * int that takes a non-empty list of numbers, and returns a pair (min, max) of the minimum and maximum of the numbers in the list.*)
fun min_max (xs:int list) =
  let
      val first = hd xs
      val rest = tl xs 
  in 
      if null rest 
      then
	  (first ,first)
      else
	  let 
	      val min_and_max =  min_max (rest)
	  in
	      if first < #1 min_and_max
	      then
		  (first ,#2 min_and_max)
	      else 
		  if first > #2 min_and_max
		  then
		      (#1 min_and_max,first)
		  else
		      min_and_max
	  end
  end


(* Write a function cumsum : int list -> int list that takes a list of numbers and returns a list of the partial sums of those numbers. For example cumsum [1,4,20] = [1,5,25].*)
fun cumsum(xs:int list) =
  let
      fun cumsum_helper(prev_sum:int,xs:int list) =
	if null xs
	then
	    []
	else
	    let
		val cur_sum = hd xs + prev_sum
	    in
		cur_sum::cumsum_helper(cur_sum,tl xs)
	    end
  in
      cumsum_helper(0,xs)
  end

(* Write a function greeting : string option -> string that given a string option SOME name returns the string "Hello there, ...!" where the dots would be replaced by name. Note that the name is given as an option, so if it is NONE then replace the dots with "you".*)
fun greeting (name:string option) = 
  if isSome name
  then
      "Hello there, "^ valOf name ^ "!"
  else
      "Hello there, you!"

(* Write a function repeat : int list * int list -> int list that given a list of integers and another list of nonnegative integers, repeats the integers in the first list according to the numbers indicated by the second list. For example: repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]. *)
fun repeat (lists:int list * int list) =
  if null (#1 lists)
  then
      []
  else
      let 
	  fun repeat_single (num:int,count:int) =
	    if count= 0
	    then
		[]
	    else
		num::repeat_single(num,count-1)
      in
	  repeat_single(hd (#1 lists), hd (#2 lists)) @ repeat(tl (#1 lists), tl (#2 lists))
      end

(* Write a function addOpt : int option * int option -> int option that given two "optional" integers, adds them if they are both present (returning SOME of their sum), or returns NONE if at least one of the two arguments is NONE.*)
fun addOpt(a:int option,b:int option) =
  if isSome a andalso isSome b
  then
      SOME (valOf a+ valOf b)
  else
      NONE

(* Write a function addAllOpt : int option list -> int option that given a list of "optional" integers, adds those integers that are there (i.e. adds all the SOME i). For example: addOpt ([SOME 1, NONE, SOME 3]) = SOME 4. If the list does not contain any SOME is in it, i.e. they are all NONE or the list is empty, the function should return NONE. *)
fun addAllOpt (xs : int option list) =
  if null xs
  then
      NONE
  else
      let
	  val first = hd xs
	  val res = addAllOpt(tl xs)
      in
	  if isSome first
	  then
	      if isSome res
	      then
		  SOME (valOf first + valOf res)
	      else
		  first
	  else
	      res
      end			

(* Write a function any : bool list -> bool that given a list of booleans returns true if there is at least one of them that is true, otherwise returns false. (If the list is empty it should return false because there is no true.) *)
fun any(bools:bool list) =
  if null bools
  then
      false
  else
      hd bools orelse any(tl bools)

(* Write a function all : bool list -> bool that given a list of booleans returns true if all of them true, otherwise returns false. (If the list is empty it should return true because there is no false.) *)
fun all(bools:bool list) =
  if null bools
  then
      true
  else
      hd bools andalso all (tl bools)

(*  Write a function zip : int list * int list -> int * int list that given two lists of integers creates consecutive pairs, and stops when one of the lists is empty. For example: zip ([1,2,3], [4, 6]) = [(1,4), (2,6)]. *)		
fun zip(xs1:int list,xs2:int list) =
  if null xs1 orelse null xs2
  then
      []
  else
      (hd xs1,hd xs2)::zip(tl xs1,tl xs2)
			  
fun length(xs:'a list) =
  if null xs
  then
      0
  else
      1+length(tl xs)
			  
(* Challenge: Write a version zipRecycle of zip, where when one list is empty it starts recycling from its start until the other list completes. For example: zipRecycle ([1,2,3], [1, 2, 3, 4, 5, 6, 7]) = [(1,1), (2,2), (3, 3), (1,4), (2,5), (3,6), (1,7)]. *)
fun zipRecycle(xs1 : int list,xs2 : int list) =
  let
      val xs1_len=length xs1
      val xs2_len=length xs2
      fun repeat(xs:int list,cnt:int) =
	if cnt = 0
	then
	    []
	else
	    xs@repeat(xs,cnt - 1)
  in
      if xs1_len <xs2_len
      then
	  zip(repeat(xs1,((xs2_len-xs1_len) div xs1_len)+2) , xs2)
      else
	  if xs1_len > xs2_len
	  then
	      zip(xs1,repeat(xs2,((xs1_len-xs2_len) div xs2_len)+2))
	  else
	      zip(xs1,xs2)
  end


	
(* Lesser challenge: Write a version zipOpt of zip with return type (int * int) list option. This version should return SOME of a list when the original lists have the same length, and NONE if they do not. *)
fun zipOpt(xs1:int list ,xs2:int list) =
  let
      val xs1_len=length xs1
      val xs2_len=length xs2
  in
      if xs1_len = xs2_len
      then
	  SOME (zip(xs1,xs2))
      else
	  NONE
  end
      
(* Write a function lookup : (string * int) list * string -> int option that takes a list of pairs (s, i) and also a string s2 to look up. It then goes through the list of pairs looking for the string s2 in the first component. If it finds a match with corresponding number i, then it returns SOME i. If it does not, it returns NONE. *)      
fun lookup(pairs:(string * int) list,str:string) =
  if null pairs
  then
      NONE
  else
      if #1 (hd pairs) = str
      then
	  SOME (#2 (hd pairs))
      else
	  lookup(tl pairs,str)
		
(* Write a function splitup : int list -> int list * int list that given a list of integers creates two lists of integers, one containing the non-negative entries, the other containing the negative entries. Relative order must be preserved: All non-negative entries must appear in the same order in which they were on the original list, and similarly for the negative entries. *)
fun splitup(xs:int list) =
  if null xs
  then
      ([],[])
  else
      let
	  val first = hd xs
	  val rest = splitup(tl xs)
      in
	  if first >= 0
	  then
	      (first::(#1 rest),#2 rest)
	  else	
	      (#1 rest,first::(#2 rest))
      end
			 
  
(* Write a version splitAt : int list * int -> int list * int list of the previous function that takes an extra "threshold" parameter, and uses that instead of 0 as the separating point for the two resulting lists. *)	  
fun splitAt(xs:int list,threshold:int) =
  if null xs
  then
      ([],[])
  else
      let
	  val first = hd xs
	  val rest = splitAt(tl xs,threshold)
      in
	  if first >= threshold
	  then
	      (first::(#1 rest),#2 rest)
	  else	
	      (#1 rest,first::(#2 rest))
      end

(* Write a function isSorted : int list -> boolean that given a list of integers determines whether the list is sorted in increasing order. *)
fun isSorted(xs:int list) =
  if length xs <= 1
  then
      true
  else	  
      let
	  val first = hd xs
	  val rest = tl xs
      in
	  if first > hd rest
	  then
	      false
	  else
	      isSorted(rest)
      end
	  
(* Write a function isAnySorted : int list -> boolean, that given a list of integers determines whether the list is sorted in either increasing or decreasing order. *)
fun isAnySorted(xs:int list) =
  let
      fun reverse(xs:int list) =
	if null xs
	then
	    []
	else
	    reverse(tl xs)@[hd xs]
  in
      isSorted(xs) orelse isSorted(reverse(xs))
  end
      
(* Write a function sortedMerge : int list * int list -> int list that takes two lists of integers that are each sorted from smallest to largest, and merges them into one sorted list. For example: sortedMerge ([1,4,7], [5,8,9]) = [1,4,5,7,8,9].*)
fun sortedMerge(xs1:int list,xs2:int list) =
  if null xs1
  then
      xs2
  else
      if null xs2
      then
	  xs1
      else
	  let
	      val first1 = hd xs1
	      val rest1 = tl xs1
	      val first2 = hd xs2
	      val rest2 = tl xs2
	  in
	      if first1 < first2
	      then
		  first1 :: sortedMerge(rest1,xs2)
	      else
		  first2 :: sortedMerge(xs1,rest2)
				       
	  end
	      
(* Write a sorting function qsort : int list -> int list that works as follows: Takes the first element out, and uses it as the "threshold" for splitAt. It then recursively sorts the two lists produced by splitAt. Finally it brings the two lists together. (Don't forget that element you took out, it needs to get back in at some point). You could use sortedMerge for the "bring together" part, but you do not need to as all the numbers in one list are less than all the numbers in the other.) *)
fun qsort(xs:int list) =
  if null xs
  then
      []
  else
      let
	  val first = hd xs
	  val sub_lists = splitAt(xs,first) 
      in
	  qsort(#2 sub_lists) @ [first] @ qsort(tl (#1 sub_lists))
      end

(* Write a function divide : int list -> int list * int list that takes a list of integers and produces two lists by alternating elements between the two lists. For example: divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6]). *)	  
fun divide(xs:int list) =
  if length xs <= 1
  then
      (xs,[])
  else
      let
	  val first = hd xs
	  val rest = tl xs
      in
	  let
	      val sub_divide = divide(tl rest)
	  in
	      (first::(#1 sub_divide) , (hd rest)::(#2 sub_divide))
	  end
      end
	  
(* Write another sorting function not_so_quick_sort : int list -> int list that works as follows: Given the initial list of integers, splits it in two lists using divide, then recursively sorts those two lists, then merges them together with sortedMerge. *)
fun not_so_quick_sort(xs:int list) =
  if length(xs) <= 1
  then
      xs
  else
      let
	  val pair = divide(xs)
	  val sub_list1 = not_so_quick_sort(#1 pair)
	  val sub_list2 = not_so_quick_sort(#2 pair)
      in
	  sortedMerge(sub_list1,sub_list2)
      end
	  
      
(* Write a function fullDivide : int * int -> int * int that given two numbers k and n it attempts to evenly divide k into n as many times as possible, and returns a pair (d, n2) where d is the number of times while n2 is the resulting n after all those divisions. Examples: fullDivide (2, 40) = (3, 5) because 2*2*2*5 = 40 and fullDivide((3,10)) = (0, 10) because 3 does not divide 10. *)
fun fullDivide(k:int ,n:int) =
  let
      fun helper(cnt:int,remain:int) =
	if remain mod k = 0
	then
	    helper (cnt+1,remain div k)
	else
	    (cnt,remain)
  in
      helper(0,n)
  end

      
(* Using fullDivide, write a function factorize : int -> (int * int) list that given a number n returns a list of pairs (d, k) where d is a prime number dividing n and k is the number of times it fits. The pairs should be in increasing order of prime factor, and the process should stop when the divisor considered surpasses the square root of n. If you make sure to use the reduced number n2 given by fullDivide for each next step, you should not need to test if the divisors are prime: If a number divides into n, it must be prime (if it had prime factors, they would have been earlier prime factors of n and thus reduced earlier). Examples: factorize(20) = [(2,2), (5,1)]; factorize(36) = [(2,2), (3,2)]; factorize(1) = []. *)

fun factorize(n:int) =
  let
      fun divide_by_prime(n,prime) =
	if n = 1
	then
	    []
	else
	    if prime <= n andalso prime * prime > n
	    then
		[(n,1)]
	    else
		let
		    val pair = fullDivide(prime,n)
		in
		    if #1 pair <> 0
		    then
			(prime,#1 pair)::divide_by_prime(#2 pair,prime+1)
		    else
			divide_by_prime(n,prime+1)
		end
  in
      divide_by_prime(n,2)
  end

fun pow(x:int,y:int) =
  if y = 0
  then
      1
  else
      x*pow(x,y-1)

(* Write a function multiply : (int * int) list -> int that given a factorization of a number n as described in the previous problem computes back the number n. So this should do the opposite of factorize. *)
fun multiply(pairs:(int * int) list) =
  if null pairs
  then
      1
  else
      pow(#1 (hd pairs),#2 (hd pairs)) * multiply(tl pairs)

						 
(* Challenge (hard): Write a function all_products : (int * int) list -> int list that given a factorization list result from factorize creates a list all of possible products produced from using some or all of those prime factors no more than the number of times they are available. This should end up being a list of all the divisors of the number n that gave rise to the list. Example: all_products([(2,2), (5,1)]) = [1,2,4,5,10,20]. For extra challenge, your recursive process should return the numbers in this order, as opposed to sorting them afterwards. *)
fun all_products(pairs:(int * int) list) =
  if null pairs
  then
      [1]
  else
      let
	  fun multiply_list(a:int,xs:int list) =
	    if null xs
	    then
		[]
	    else
		(a*hd xs)::multiply_list(a,tl xs)
	  fun pow_multiply_and_merge(base:int,cnt:int,xs:int list) =
	    if cnt = 0
	    then
		xs
	    else
		sortedMerge(multiply_list(pow(base,cnt),xs),pow_multiply_and_merge(base,cnt-1,xs))
	  val pair = hd pairs
      in
	  pow_multiply_and_merge(#1 pair,#2 pair,all_products(tl pairs))
      end
	  
	  
	  
	  
		
	    




					   

	  
