(* Coursera Programming Languages, Homework 3, Provided Code *)

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

fun only_capitals (sl) =
  List.filter (fn s => Char.isUpper(String.sub(s, 0))) sl

fun longest_string1 (sl) =
  List.foldl (fn (x,y) => if (String.size(x) > String.size(y)) then x else y) "" sl

fun longest_string2 (sl) =
  List.foldl (fn (x,y) => if (String.size(x) < String.size(y)) then y else x) "" sl

fun longest_string_helper f sl =
  List.foldl (fn (x,y) => if (f(String.size(x), String.size(y))) then x else y) "" sl

fun longest_string3 (sl) =
  let val f = fn (sz1, sz2) => if (sz1 > sz2) then true else false in
    longest_string_helper f sl
  end

fun longest_string4 (sl) =
  let val f = fn (sz1, sz2) => if (sz1 >= sz2) then true else false in
    longest_string_helper f sl
  end

fun longest_capitalized (sl) =
  let val f = longest_string1 o only_capitals in
    f sl
  end

fun rev_string (s) =
  (String.implode o List.rev o String.explode) s

fun first_answer f l =
  case (List.filter Option.isSome (List.map f l)) of
       [] => raise NoAnswer
     | h :: _ => Option.valOf h

fun all_answers f l =
  let
    fun combine (ol, acc) =
      case (ol, acc) of
           (_, NONE) => NONE
         | (NONE, _) => NONE
         | (SOME l, SOME acc') => SOME(l @ acc')
  in
    List.foldl combine (SOME([])) (List.map f l)
  end

fun count_wildcards p =
  let
    fun f1 (u: unit) = 1
    fun f2 (s: string) = 0
  in
    g f1 f2 p
  end

fun count_wild_and_variable_lengths p =
  let
    fun f1 (u: unit) = 1
    fun f2 (s: string) = String.size s
  in
    g f1 f2 p
  end

fun count_some_var (s, p) =
  let
    fun f1 (u: unit) = 0
    fun f2 (s': string) = if (s' = s) then 1 else 0
  in
    g f1 f2 p
  end

(* helper function to get a list of all variable names from a pattern *)
fun all_vars p =
  case p of
       Variable x        => [x]
     | TupleP ps         => List.foldl (fn (p',l) => l @ all_vars(p')) [] ps
     | ConstructorP(_,p) => all_vars(p)
     | _                 => []

fun has_repeats (l: string list) =
  case l of
       [] => false
     | h :: l' => (List.exists (fn x => x = h) l') orelse has_repeats(l')

fun check_pat p =
  not (has_repeats (all_vars p))

fun match (v, p) =
  case (p, v) of
       (Wildcard, _) => SOME []
     | (Variable s, v)  => SOME [(s, v)]
     | (UnitP, Unit) => SOME []
     | (ConstP pv, Const vv) => if (pv = vv) then SOME [] else NONE
     | (TupleP pl, Tuple vl) =>
         if (List.length(pl) <> List.length(vl)) then NONE
         else
           all_answers match (ListPair.zip(vl, pl))
     | (ConstructorP(s1, p'), Constructor(s2, v')) =>
         if (s1 = s2) then match(v', p') else NONE
     | _ => NONE

fun first_match v pl =
  SOME (first_answer match (List.map (fn p => (v, p)) pl))
  handle NoAnswer => NONE
