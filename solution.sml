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
        x::(compress (y::rest))
  | compress l = l

(* #9 *)
fun pack l =
    let
        fun pack_acc acc l =
            case l of
                x::y::rest =>
                if x = y then
                    pack_acc (acc @ [x]) (y::rest)
                else
                    (acc @ [x])::(pack (y::rest))
              | [] => [acc]
              | x::[] => [acc @ [x]]

    in
        pack_acc [] l
    end

(* #10 *)
fun encode (l as (x::_)) =
    let
        val p = pack l
    in
        List.map (fn e => ((List.length e), (List.hd e))) p
    end
  | encode [] = []

(* #11 *)
datatype 'a rle =
         ONE of 'a
         | MANY of int * 'a

fun encode_rle l =
    let
        val enc = encode l
    in
        List.map (fn e =>
                     case e of
                         (1, x) => ONE x
                       | (n, x) => if n > 1 then MANY (n, x)
                                   else raise AssertionFail)
                 enc
    end

(* #31 *)
fun is_prime 1 = true
  | is_prime 2 = true
  | is_prime n =
    let
        fun test_mod k =
            if k * k > n then true
            else if n mod k = 0 then false
            else test_mod (k+1)
    in
        test_mod 2
    end
