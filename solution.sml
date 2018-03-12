structure Solution =
struct

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

(* #12 *)
fun decode_rle [] = []
  | decode_rle (x::rest) =
    let fun decode_single (ONE x) = [x]
          | decode_single (MANY (n, x)) = List.tabulate (n, (fn _ => x))
    in
        (decode_single x) @ (decode_rle rest)
    end

(* #14 *)
fun duplicate [] = []
  | duplicate (x::rest) = [x, x] @ (duplicate rest)

(* #15 *)
fun replicate [] _ = []
  | replicate (x::rest) k =
    if k <= 0 then
        []
    else
        List.tabulate (k, (fn _ => x)) @ (replicate rest k)

(* #16 *)
fun drop l k =
    let
        fun drop' prefix [] _ = prefix
          | drop' prefix (x::rest) k =
            if k = 0 then prefix @ rest
            else drop' (prefix @ [x]) rest (k-1)
    in
        drop' [] l k
    end

(* #17 *)
fun split l k =
    let
        fun split' prefix [] _ = (prefix, [])
          | split' prefix (l' as (x::rest)) k =
            if k = 0 then
                (prefix, l')
            else
                split' (prefix @ [x]) rest (k-1)
    in
        split' [] l k
    end

(* #18 *)
fun slice l a b =
    let
        val prefix = (#2 (split l a))
    in
        (#1 (split prefix (b-a)))
    end

(* #19 *)
fun rotate l k =
    let
        val (a, b) = split l k
    in
        b @ a
    end

(* #20 *)
fun remove_at k l = drop l k

(* #21 *)
fun insert_at x k l =
    let
        val (a, b) = split l k
    in
        a @ [x] @ b
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

end
