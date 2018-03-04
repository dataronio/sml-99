exception AssertionFail

fun println s = print (s ^ "\n")

signature DICT =
sig
    type key = string
    type 'a entry = key * 'a
    type 'a dict
    val empty : 'a dict
    val insert : 'a dict * 'a entry -> 'a dict
    val lookup : 'a dict * key -> 'a option
end

structure RedBlackTree :> DICT =
struct
type key = string
type 'a entry = string * 'a
datatype 'a dict = Empty
                 | Red of 'a entry * 'a dict * 'a dict
                 | Black of 'a entry * 'a dict * 'a dict
val empty = Empty
fun lookup (dict, key) =
    let
        fun internal_lookup Empty = NONE
          | internal_lookup (Red tree) = internal_lookup' tree
          | internal_lookup (Black tree) = internal_lookup' tree
        and internal_lookup' ((k, v), left, right) =
            case String.compare (k, key) of
                EQUAL => SOME v
              | LESS => internal_lookup left
              | GREATER => internal_lookup right
    in
        internal_lookup dict
    end

fun restoreLeft
        (Black (z, Red (y, Red (x, d1, d2), d3), d4)) =
    Red (y, Black (x, d1, d2), Black (z, d3, d4))
  | restoreLeft
        (Black (z, Red (x, d1, Red (y, d2, d3)), d4)) =
    Red (y, Black (x, d1, d2), Black (z, d3, d4))
  | restoreLeft dict = dict

fun restoreRight
        (Black (x, d1, Red (y, d2, Red (z, d3, d4)))) =
    Red (y, Black (x, d1, d2), Black (z, d3, d4))
  | restoreRight
        (Black (x, d1, Red (z, Red (y, d2, d3), d4))) =
    Red (y, Black (x, d1, d2), Black (z, d3, d4))
  | restoreRight dict = dict

fun insert (dict, entry as (key, value)) =
    let
        fun insert' Empty = Red (entry, Empty, Empty)
          | insert' (Red (entry' as (key', value'), left, right)) =
            (case String.compare (key, key') of
                 EQUAL => Red (entry, left, right)
               | LESS => Red (entry', insert' left, right)
               | GREATER => Red (entry', left, insert' right))
          | insert' (Black (entry' as (key', value'), left, right)) =
            (case String.compare (key, key') of
                 EQUAL => Black (entry, left, right)
               | LESS => restoreLeft (Black (entry', insert' left, right))
               | GREATER => restoreRight (Black (entry', left, insert' right)))
    in
        case insert' dict of
            Red (t as (_, Red _, _)) => Black t
          | Red (t as (_, _, Red _)) => Black t
          | dict => dict
    end
end

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
    case l of
        [] => Option.NONE
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
datatype 'a node = ONE of 'a
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
datatype 'a rle = ONE of 'a
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

(* #32 *)
fun gcd a b =
    if a mod b = 0 then b
    else gcd b (a mod b)

(* #33 *)
fun coprime x y = gcd x y = 1

(* #34 *)
fun phi 1 = 1
  | phi n =
    let
        fun phi_acc acc k =
            if k = n then
                acc
            else if coprime n k then
                phi_acc (acc + 1) (k+1)
            else
                phi_acc acc (k+1)
    in
        phi_acc 0 1
    end
