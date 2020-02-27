(* Header *)
{
    type index = INDICE of int * int;;
    type token = 
        FLOAT of float                  (* floating constant with optional sign w/o initial and trailing zeros *)
      | LP | RP                         (* left parenthesis "(", right parenthesis ")" *)
      | LB | RB                         (* left bracket "[", right bracket "]" *)
      | COMMA                           (* comma "," *)
      | COLON                           (* colon ":" *)
      | INDICE of int * int             (* indices "[i, j]" *)
      | RANGE of index * index          (* ranges "(l : l)" *)
      | COUNT | ROWCOUNT | COLCOUNT
      | SUM   | ROWSUM   | COLSUM
      | AVG   | ROWAVG   | COLAVG
      | MIN   | ROWMIN   | COLMIN
      | MAX   | ROWMAX   | COLMAX       (* Unary Operators *)
      | ADD | SUBT | MULT | DIV         (* Binary Operators *)
      | EQ                              (* assignment operator ":=" *)
      | DELIMITER                       (* semicolon ";" *);;
    exception InvalidToken of char ;;
}


let digit = ['0'-'9']
let digit_ = ['1'-'9']
(* Regex for natural numbers *)
let number = ('0' | ((digit_)digit*))
(* Regex for floating constants *)
let float_constant = ['+''-']?(number)['.']('0'|digit*(digit_))
(* Regex for whitespace *)
let sp = [' ' '\t']+

rule read = parse
      eof                       {[]}
    | sp                        {read lexbuf}
    | float_constant as f       {(FLOAT (float_of_string f)) :: (read lexbuf)}
    | '('                       {LP :: (read lexbuf)}
    | ')'                       {RP :: (read lexbuf)}
    | '['                       {LB :: (read lexbuf)}
    | ']'                       {RB :: (read lexbuf)}
    | ','                       {COMMA :: (read lexbuf)}
    | ':'                       {COLON :: (read lexbuf)}
    | ['[']sp*(number as n1)sp*[',']sp*(number as n2)sp*[']'] (* Regex for indice *)
        {INDICE((int_of_string n1), (int_of_string n2)) :: (read lexbuf)}
    | ['(']sp*['[']sp*(number as n1)sp*[',']sp*(number as n2)sp*[']']sp*[':']sp*['[']sp*(number as n3)sp*[',']sp*(number as n4)sp*[']']sp*[')'] (* Regex for range *)
        {RANGE(INDICE((int_of_string n1), (int_of_string n2)), INDICE((int_of_string n3), (int_of_string n4))) :: (read lexbuf)}
    | "COUNT"                   {COUNT :: (read lexbuf)}
    | "ROWCOUNT"                {ROWCOUNT :: (read lexbuf)}
    | "COLCOUNT"                {COLCOUNT :: (read lexbuf)}
    | "SUM"                     {SUM :: (read lexbuf)}
    | "ROWSUM"                  {ROWSUM :: (read lexbuf)}
    | "COLSUM"                  {COLSUM :: (read lexbuf)}
    | "AVG"                     {AVG :: (read lexbuf)}
    | "ROWAVG"                  {ROWAVG :: (read lexbuf)}
    | "COLAVG"                  {COLAVG :: (read lexbuf)}
    | "MIN"                     {MIN :: (read lexbuf)}
    | "ROWMIN"                  {ROWMIN :: (read lexbuf)}
    | "COLMIN"                  {COLMIN :: (read lexbuf)}
    | "MAX"                     {MAX :: (read lexbuf)}
    | "ROWMAX"                  {ROWMAX :: (read lexbuf)}
    | "COLMAX"                  {COLMAX :: (read lexbuf)}
    | "ADD"                     {ADD :: (read lexbuf)}
    | "SUBT"                    {SUBT :: (read lexbuf)}
    | "MULT"                    {MULT :: (read lexbuf)}
    | "DIV"                     {DIV :: (read lexbuf)}
    | ":="                      {EQ :: (read lexbuf)}
    | ';'                       {DELIMITER :: (read lexbuf)}
    | _ as s                    {raise (InvalidToken s)} (* An exception is raised if any string apart from expected strings are encountered *)


(* trailer *)
{
	let scanner s = read(Lexing.from_string s)          (* LEXER FUNCTION *)
}
