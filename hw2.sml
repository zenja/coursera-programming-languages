(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* Start of Part 1 *)

fun all_except_option (str: string, ss: string list) =
  case ss of
       [] => NONE
     | s :: tail => if (same_string(str, s)) then SOME(tail)
                    else case all_except_option(str, tail) of
                              NONE => NONE
                            | SOME(l) => SOME(s :: l)

fun get_substitutions1 (sls: string list list, s: string) =
  case sls of
       [] => []
     | sl :: tail =>
         case all_except_option(s, sl) of
              NONE => get_substitutions1(tail, s)
            | SOME(l) => l @ get_substitutions1(tail, s)

fun get_substitutions2 (sls: string list list, s: string) =
let fun f (lists, acc) =
      case lists of
           [] => acc
         | l :: lists' =>
             case all_except_option(s, l) of
                  NONE => f(lists', acc)
                | SOME(el) => f(lists', acc @ el)
in
  f(sls, [])
end

type person_name = {first:string,middle:string,last:string}

fun make_names (first_names: string list, name: person_name) =
  case first_names of
       [] => []
     | f :: tl =>
         case name of
              {first=_,middle=y,last=z} =>
                {first=f,middle=y,last=z} :: make_names(tl, name)

fun similar_names (subs: string list list, name: person_name) =
  case name of
       {first=x,middle=y,last=z} =>
         name :: make_names(get_substitutions1(subs, x), name)

(* Start of Part 2 *)

fun card_color (c: card) =
  case c of
       (Spades, _) => Black
     | (Clubs, _) => Black
     | _ => Red

fun card_value (c: card) = 
  case c of
       (_, Num(n)) => n
     | (_, Ace) => 11
     | _ => 10

fun remove_card (cs: card list, c: card, e) =
  case cs of
       [] => raise e
     | hd :: cs' => if (hd = c) then cs'
                    else remove_card(cs', c, e)

fun all_same_color (cs: card list) =
  case cs of
       [] => true
     | [_] => true
     | head :: neck :: tail =>
         card_color(head) = card_color(neck) andalso all_same_color(neck :: tail)

fun sum_cards (cs: card list) =
let fun sum (cl: card list, acc) =
      case cl of
           [] => acc
         | head :: cl' => sum(cl', acc + card_value(head))
in
  sum(cs, 0)
end

fun score (cs: card list, goal: int) =
let val sum = sum_cards(cs) in
  let val preliminary = if (sum > goal) then 3 * (sum-goal) else (goal-sum) in
    if (all_same_color(cs)) then preliminary div 2 else preliminary
  end
end

fun officiate (cl: card list, ml: move list, goal: int) =
let
  fun move(cur_cl: card list, held_cs: card list, remain_ml: move list) =
    case remain_ml of
         [] => score(held_cs, goal)
       | Discard(c) :: remain_ml' =>
           move(cur_cl, remove_card(held_cs, c, IllegalMove), remain_ml')
       | Draw :: remain_ml' =>
           case cur_cl of
                [] => score(held_cs, goal)
              | c :: cur_cl' =>
                  if (sum_cards(c :: held_cs) > goal) then
                    score(c :: held_cs, goal)
                  else move(cur_cl', c :: held_cs, remain_ml')
in
  move(cl, [], ml)
end
