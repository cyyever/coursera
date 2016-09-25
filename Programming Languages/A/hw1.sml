(* Dan Grossman, Coursera PL, HW1 *)

fun is_older(first_date : int * int * int, second_date : int * int * int) =
  (*
  #1 first_date < #1 second_date orelse
  #1 first_date = #1 second_date andalso #2 first_date < #2 second_date orelse 
  #1 first_date = #1 second_date andalso #2 first_date = #2 second_date andalso #3 first_date < #3 first_date
  *)
  #1 first_date * 12 * 31 + #2 first_date * 31 + #3 first_date <  #1 second_date * 12 * 31 + #2 second_date * 31 + #3 second_date
 

fun number_in_month(dates : (int * int * int) list,month : int) =
  if null dates
  then
      0
  else
      if #2 (hd dates) = month
      then
	  1 + number_in_month(tl dates,month)
      else
	  0 + number_in_month(tl dates,month)

fun number_in_months(dates : (int * int * int) list,months : int list) =
  if null months
  then
      0
  else
      number_in_month(dates,hd months)+number_in_months(dates,tl months)

fun dates_in_month(dates : (int * int * int) list,month : int) =
  if null dates
  then
      []
  else if #2 (hd dates) = month
  then
      (hd dates)::dates_in_month(tl dates,month)
  else
      dates_in_month(tl dates,month)

fun dates_in_months(dates : (int * int * int) list,months : int list) =
  if null months
  then
      []
  else
      dates_in_month(dates,hd months) @ dates_in_months(dates,tl months)

fun get_nth(strings : string list,n : int) =
  if n=1
  then
      hd strings
  else
      get_nth(tl strings,n-1)

fun date_to_string(date : int * int * int) =
  let
      val month_names=[ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in      
     get_nth(month_names,#2 date)^" "^Int.toString(#3 date)^", "^Int.toString(#1 date)
  end
  
fun number_before_reaching_sum(sum : int, nums : int list) =
  if sum <= hd nums
  then
      0
  else
      1+number_before_reaching_sum(sum - hd nums,tl nums)

fun what_month(day_of_year : int) =
  let
      val day_num_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day_of_year,day_num_of_month) + 1
  end
      
fun month_range(day1 : int,day2 : int) =
  if day1 > day2
  then
      []
  else
      what_month(day1)::month_range(day1 + 1,day2)
  
fun oldest(dates : (int * int * int) list) =
  if null dates
  then
      NONE
  else
      let
	  val later_oldest_date = oldest(tl dates)
      in
	  if isSome later_oldest_date andalso is_older(valOf later_oldest_date,hd dates)
	  then
	      later_oldest_date
	  else
	      SOME (hd dates)
      end

fun uniq(xs : int list) =
  if null xs
  then
      []
  else
      let
	  fun is_in_list(x : int,xs : int list) =
	    if null xs
	    then
		false
	    else if x =  hd xs
	    then
		true
	    else
		is_in_list(x,tl xs)
	  val res_list = uniq(tl xs)
      in
	  if not (is_in_list(hd xs,res_list))
	  then
	      (hd xs)::res_list
	  else
	      res_list
      end

fun number_in_months_challenge(dates : (int * int * int) list,months : int list) =
  number_in_months(dates,uniq(months))

fun dates_in_months_challenge(dates : (int * int * int) list,months : int list) =
  dates_in_months(dates,uniq(months))

fun reasonable_date(date : int * int * int) =
  (* first filter out obvious invalid date *)
  if #1 date < 1 orelse #2 date <1 orelse #2 date > 12 orelse #3 date < 1 orelse #3 date > 31
  then
      false
  else if (#1 date mod 400) = 0 orelse (#1 date mod 4) = 0 andalso (#1 date mod 100) <> 0 (* leap year *)
  then
      if #3 date <= 29
      then
	  true
      else
	  false
  else
      let
	  val day_num_of_month = [31,28,31,30,31,30,31,31,30,31,30,31]
	  fun get_nth(xs : int list,n : int) =
	    if n=1
	    then
		hd xs
	    else
		get_nth(tl xs,n-1)
      in
	  if get_nth(day_num_of_month,#2 date) < #3 date
	  then
	      false
	  else
	      true
      end	  
	  
				   
