(* 1.Write a function compose_opt : ('b -> 'c option) -> ('a -> 'b option) -> 'a -> 'c option that composes two functions with "optional" values. If either function returns NONE, then the result is NONE. *)

fun compose_opt g f x =
  case f(x) of
      NONE => NONE
   | SOME y => g(y)

(* 2.Write a function do_while : ('a -> 'a) -> ('a -> bool) -> 'a -> 'a. do_until f p x will apply f to x and f again to that result and so on until p x is false. Example: do_while (fn x => x div 2) (fn x => x mod 2 <> 1) will evaluate to a function of type int->int that divides its argument by 2 until it reaches an odd number. In effect, it will remove all factors of 2 its argument. *)

fun do_while f p x =
  if p(x)
  then
      do_while f p (f x)
  else
      x


(* 3.Use do_while to implement factorial. *)

fun factorial n =
  case do_while (fn (n,fact) => (n-1,fact*n)) (fn (n,_) => n > 1) (n,1) of
      (_,fact) => fact

(* 4.Using do_while from the previous problem, write a function fixed_point: (''a -> ''a) -> ''a -> ''a that given a function f and an initial value x applies f to x until f x = x. (Notice the use of '' to indicate equality types.) *)

fun fixed_point f x =
  do_while (fn x' => f x') (fn x' => x' <> x ) x
	   
(* 5.Write a function map2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b that given a function that takes 'a values to 'b values and a pair of 'a values returns the corresponding pair of 'b values. *)

fun map2 f (a,b) = (f a,f b)

(* 6.Write a function app_all : ('b -> 'c list) -> ('a -> 'b list) -> 'a -> 'c list, so that: app_all f g x will apply f to every element of the list g x and concatenate the results into a single list. For example, for fun f n = [n, 2 * n, 3 * n], we have app_all f f 1 = [1, 2, 3, 2, 4, 6, 3, 6, 9]. *)

fun app_all f g x =
  foldl (fn (x,acc) => acc@x ) [] (map f (g x))

(* 7.Implement List.foldr (see http://sml-family.org/Basis/list.html#SIG:LIST.foldr:VAL). *)

fun my_foldr f init xs =
  foldl f init (List.rev xs)
	
(* 8.Write a function partition : ('a -> bool) -> 'a list -> 'a list * 'a list where the first part of the result contains the second argument elements for which the first element evaluates to true and the second part of the result contains the other second argument elements. Traverse the second argument only once. *)

fun partition f xs =
  foldl (fn (x,(acc1,acc2)) => if f(x) then (acc1@[x],acc2) else (acc1,acc2@[x])) ([],[]) xs


(* 9.Write a function unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list that produces a list of 'b values given a "seed" of type 'a and a function that given a seed produces SOME of a pair of a 'b value and a new seed, or NONE if it is done seeding. For example, here is an elaborate way to count down from 5: unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) 5 = [5, 4, 3, 2, 1]. *)

fun unfold f x =
  let
      fun unfold2 (acc,x) =
	case f(x) of
	    SOME (x,s) => unfold2(acc@[x],s)
	  | NONE => acc
  in
      unfold2([],x)
  end
		  

(* 10.Use unfold and $\verb|foldl|$$ to implement factorial. *)
      
fun factorial2 n =
  foldl Int.* 1 (unfold (fn n => if n = 0 then NONE else SOME(n, n-1)) n)
    
(* 11.Implement map using List.foldr. *)

fun my_map f l =
  case l of
      [] => []
   | x::xs' => (f x)::(my_map f xs') 

(* 12.Implement filter using List.foldr. *)

fun my_filter f l =
  List.foldr (fn (x,acc) => if f x then x::acc else acc) [] l

(* 13.Implement foldl using foldr on functions. (This is challenging.) *)
	     
fun my_foldl f init l =
 (foldr (fn (x,outer_f)=> ( fn inner_init => outer_f(f(x,inner_init)) )) ( fn inner_init => inner_init ) l ) init

(* Define a (polymorphic) type for binary trees where data is at internal nodes but not at leaves. Define map and fold functions over such trees. You can define filter as well where we interpret a "false" as meaning the entire subtree rooted at the node with data that produced false should be replaced with a leaf. *)

datatype 'a tree = leaf | node of ('a * 'a tree * 'a tree)

fun tree_map f t =
  case t of
      leaf => leaf
   | node(n,left,right) => node (f n,tree_map f left,tree_map f right)
		
		   
fun tree_fold_preorder f init t =
  case t of
      leaf => init 
    | node(n,left,right) => tree_fold_preorder f (tree_fold_preorder f (f(n,init)) left) right
					       
fun tree_filter f t =
  case t of
      leaf => leaf
    | node(n,left,right) =>
      if f n
      then
	  node(n,tree_filter f left,tree_filter f right)
      else
	  leaf
