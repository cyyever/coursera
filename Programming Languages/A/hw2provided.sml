(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str,strs) =
	case strs of
	    [] => NONE
	  | a::strs' =>
	    if same_string(a, str)
	    then
		SOME strs'
	    else
		case all_except_option(str,strs') of
		    NONE => NONE
		  | SOME s => SOME (a::s)

fun get_substitutions1(strss,s) =
  case strss of
      [] => []
    | strs::strss' =>
	case all_except_option(s,strs) of
	  NONE => get_substitutions1(strss',s)
	 | SOME a => a @ get_substitutions1(strss',s)	   									 
fun get_substitutions2(strss,s) =
  let
      fun get_by_tail_recursion(acc,strss) =
	case strss of
	    [] => acc
	  | strs::strss' =>
	    case all_except_option(s,strs) of
		NONE => get_by_tail_recursion(acc,strss')
	      | SOME a => get_by_tail_recursion(acc@a,strss')
  in
      get_by_tail_recursion([],strss)
  end
 
fun similar_names(substitutions,full_name) =
  case full_name of
      {first=a,middle=b,last=c} =>
      let
	  fun other_names(other_first_names) =
	    case other_first_names of
		[] => []
	      | other_first::other_first' =>
		{first=other_first,middle=b,last=c} :: other_names(other_first')
      in
	  full_name::other_names(get_substitutions2(substitutions,a))
      end
	  
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(card) =
  case card of
      (Spades , _) => Black
    | (Clubs , _) => Black
    | _ => Red 
  
fun card_value(card) =
  case card of
      (_,Num i) => i
    | (_,Ace) => 11
    | _  => 10

fun remove_card(cs,c,e) =
  case cs of
      [] => raise e
    | a::cards' =>
      if a=c
      then
	  cards'
      else
	  a::remove_card(cards',c,e)

fun all_same_color(cs) =
  case cs of
      [] => true
    | [c] => true
    | c::(d::cs') => card_color(c)=card_color(d) andalso all_same_color(d::cs')
									
fun sum_cards(cs) =
  let
      fun sum_remain_cards(acc,cs) =
	case cs of
	    [] => acc
	  | c::cs' => sum_remain_cards(acc+card_value(c),cs')
  in
      sum_remain_cards(0,cs)
  end

fun compute_preliminary_score(sum,goal) =
  if sum > goal
  then
      3 * (sum -goal)
  else
      goal -sum
      
fun score(cs,goal) =
  let
      val preliminary_score = compute_preliminary_score(sum_cards(cs),goal)
  in
      if all_same_color(cs)
      then
	  preliminary_score div 2
      else
	  preliminary_score
  end

fun officiate (cs,move,goal) =
  let
      fun state(cs,move,hold) =
        let
            val sum = sum_cards(hold)
        in
            if sum > goal
            then
                score(hold,goal)
            else
                case move of
                    [] => score(hold,goal)
                  | m::move' =>
                    case m of
                        Draw => (case cs of
                                     [] => score(hold,goal)
                                   | c::cs' => state(cs',move',c::hold))
                      | Discard c => state(cs,move',remove_card(hold,c,IllegalMove))
        end
  in
      state(cs,move,[])
  end
      
fun score_challenge(cs,goal) =
  let
      fun ace_card_num(cs) =
	case cs of
	    [] => 0
	  | c::cs' => 
	    case c of
		(_,Ace) => 1+ace_card_num(cs')
	      | _  => ace_card_num(cs')
				  
      fun min_preliminary_score(sum,ace_num) =	
	let
	    val cur_score= compute_preliminary_score(sum,goal)
	in
	    if ace_num = 0
	    then
		cur_score
	    else
		Int.min( min_preliminary_score(sum-10,ace_num-1),cur_score)
	end
      val preliminary_score = min_preliminary_score(sum_cards(cs),ace_card_num(cs))
  in
      if all_same_color(cs)
      then
	  preliminary_score div 2
      else
	  preliminary_score
  end
