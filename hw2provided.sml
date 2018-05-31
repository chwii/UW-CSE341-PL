(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (str, strs) =
  case strs of
       [] => NONE
     | x::xs' => if same_string(str, x) then SOME xs' else 
                                             case all_except_option(str, xs') of
                                                  NONE => NONE
                                                | SOME y => SOME (x::y)

fun get_substitutions1 (ssts, str) = 
    case ssts of
         [] => []
       | ss::[] => valOf (all_except_option(str, ss))
       | ss::ss' => (case (all_except_option(str, ss)) of
                         SOME b => b | _ => [])@(get_substitutions1(ss', str))

fun get_substitutions2 (ssts, str) =
  let fun helper (strs, rs) =
        case strs of
             [] => rs
           | x::[] => rs@(case all_except_option(str, x) of SOME a => a | _ =>
               [])
           | x::xs' =>helper(xs', rs@(case (all_except_option(str, x)) of SOME
           [b] => [b] | _ => []))
  in
    helper(ssts, [])
  end

fun similar_names (ssts, {first=a,middle=b,last=c}) =
  let val all = get_substitutions2(ssts, a)
  in
    let fun helper (st, rs) = 
            case st of
                 [] => rs
               | x::[] => rs@[{first=x, middle=b,last=c}]
               | x::xs' => helper(xs', (rs@[{first=x,middle= b, last=c}]))
    in
        helper(all, {first=a,middle=b,last=c}::[])
    end
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

fun card_color (s, r) =
  case s of
        Clubs => Black
     |  Spades => Black
     |  Diamonds => Red
     |  Hearts => Red

fun card_value (s, r) =
  case r of 
       Num v => v
     | _ => 10

fun remove_card (cs, c, e) =
  case cs of
    cd::[] => if cd=c then [] else raise e
  | cd::cds' => if cd=c then cds' else [cd]@(remove_card(cds', c, e))

fun all_same_color cards =
  case cards of
       [] => true
     | c::[] => true
     | c::c2::cs' => if (card_color c) = (card_color c2) 
                     then all_same_color (c2::cs') 
                     else false

fun sum_cards cards =
  let fun helper (cs, sum) =
         case cs of
              [] => sum
            | c::cs' => helper(cs', sum + card_value c)
  in
    helper(cards, 0)
  end
  
fun score (cards, goal) =
  let val sum = sum_cards cards
      val score = if sum > goal then 3 * (sum-goal) else (goal-sum)
  in
      if all_same_color cards then score div 2 else score
  end
