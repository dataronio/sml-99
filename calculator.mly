fun lookup "bogus" = 10000
  | lookup s = 0

fun println s = print (s ^ "\n")

%%

%eop EOF SEMI
%pos int
%left SUB PLUS
%left TIMES DIV
%right CARAT

%term ID of string
    | NUM of int
    | PLUS
    | TIMES
    | PRINT
    | SEMI
    | EOF
    | CARAT
    | DIV
    | SUB


%nonterm EXP of int
       | START of int option


%name Calc

%subst PRINT for ID
%prefer PLUS TIMES DIV SUB
%keyword PRINT SEMI
%noshift EOF
%value ID ("bogus")
%nodefault
%verbose

%%
START : PRINT EXP (println (Int.toString EXP);
                   Option.SOME EXP)
      | EXP (Option.SOME EXP)
      | (Option.NONE)

EXP : NUM (NUM)
    | ID (lookup ID)
    | EXP PLUS EXP (EXP1 + EXP2)
    | EXP TIMES EXP (EXP1 * EXP2)
    | EXP DIV EXP (EXP1 / EXP2)
    | EXP SUB EXP (EXP1 - EXP2)
    | EXP CARAT EXP (let fun e (m, 0) = 1
                             e (m, 1) = m * e(m, l-1)
                     in
                         e (EXP1, EXP2)
                     end)
