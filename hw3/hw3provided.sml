exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
 
fun only_capitals strs =
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) strs

fun longest_string1 strs =
  List.foldl (fn (x, y) => if (String.size x) > (String.size y) then x else y) "" strs

fun longest_string2 strs =
  List.foldl (fn (x, y) => if (String.size x) >= (String.size y) then x else y) "" strs

fun longest_string_helper f strs =
  List.foldl (fn (x, y) => if f(String.size x, String.size y) then x else y) "" strs

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

fun longest_capitalized strs =
  (longest_string1 o only_capitals) strs

fun rev_string str =
  (String.implode o List.rev o String.explode) str

fun first_answer f li =
  case li of
       [] => raise NoAnswer
     | x::xs' => case f x of
                    NONE => first_answer f xs'
                  | SOME v => v

fun all_answers f li = 
  let fun helper(li, ans)=
        case li of
             [] => SOME ans
           | x::xs' => case f x of
                            NONE => NONE
                          | SOME x => helper(xs', ans@x)
  in
      helper(li, [])
  end

fun count_wildcards p =
  g (fn _ => 1) (fn _ => 0) p

fun count_wild_and_variable_lengths p =
  g (fn _ => 1) (fn x => String.size x) p

fun count_some_var (s, p) =
  g (fn _ => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p =
  let fun helper p =
        case p of
             Variable x     => [x]
           | TupleP ps      => List.foldl (fn (x, y) => y@(helper x)) [] ps
           | ConstructorP(_,p)=> helper p
           | _ => []
  in
    let fun checker strs =
          case strs of
               [] => true
             | x::xs' => if List.exists (fn s => x = s) xs' then false else checker xs' 
    in
        (checker o helper) p
    end
  end

fun match (valu, pat) =
  case (valu, pat) of
     (_, Wildcard) => SOME []
   | (v, Variable s) => SOME [(s, v)]
   | (Unit, UnitP) => SOME []
   | (Const c, ConstP cp) => if c = cp then SOME [] else NONE
   | (Tuple t, TupleP tp) => all_answers match (ListPair.zip(t, tp))
   | (Constructor(s, v), ConstructorP(s', p)) => if s = s' then match(v, p) else NONE
   | _ => NONE

fun first_match v ps =
  SOME (first_answer match (List.map (fn p => (v, p)) ps))
  handle NoAnswer => NONE
