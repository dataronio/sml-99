exception AssertionFail

fun println s = print (s ^ "\n")

(* #1 *)
val rec last =
 fn l => case l of
             [] => Option.NONE
          |  t::[] => Option.SOME t
          |  _::rest => last rest

(* #2 *)
fun last_two l =
    case l of
        [] => Option.NONE
     |  x::[] => Option.NONE
     | x::y::[] => Option.SOME (x, y)
     | _::rest => last_two rest

(* #3 *)
fun at k l =
    case l of [] => Option.NONE
            | x::rest => if k = 0 then
                             x
                         else
                             at (k-1) rest

(* #4 *)
fun length [] = 0
  | length (_::rest) = 1 + (length rest)

(* #5 *)
fun rev [] = []
  | rev (x::rest) = (rev rest) @ [x]

(* #6 *)
fun is_palindrome l = l = (rev l)

(* #7 *)
datatype 'a node =
         ONE of 'a
       | MANY of 'a node list

fun flattern [] = []
  | flattern ((ONE x)::rest) = x::(flattern rest)
  | flattern ((MANY l)::rest) =
    (flattern l) @ (flattern rest)

(* #8 *)
fun compress (x::y::rest) =
    if x = y then
        compress (y::rest)
    else
        x::y::(compress rest)
  | compress l = l

(* #9 *)
fun pack (l as x::y::rest) =
    let
        fun pack_acc acc l =
            if x = y then
                pack_acc (acc @ [x]) (y::rest)
            else
                (acc @ [x])::(pack (y::rest))
    in
        pack_acc [] l
    end
  | pack [] = []
  | pack (x::[]) = [[x]]
