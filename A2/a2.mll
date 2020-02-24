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
      | SUM | ROWSUM | COLSUM
      | AVG | ROWAVG | COLAVG
      | MIN | ROWMIN | COLMIN
      | MAX | ROWMAX | COLMAX           (* Unary Operators *)
      | ADD | SUBT | MULT | DIV         (* Binary Operators *)
      | EQ                              (* assignment operator ":=" *)
      | DELIMITER                       (* semicolon ";" *);;
      exception InvalidToken of char ;;
    
    let find_num s =
      let rec find_num_util s i = if ((s.[i] >= '0') && (s.[i] <= '9')) then i
                                  else find_num_util s (i+1)
      in find_num_util s 0;;

    let find_non_num s = 
      let rec find_non_num_util s i = if (s.[i] < '0') || (s.[i] > '9') then i
                                      else find_non_num_util s (i+1)
      in find_non_num_util s 0;;

    let lex_indice1 (s): index = 
      let start1 = find_num s in
      let new_s = String.sub s start1 (String.length s - start1) in
      let end1 = find_non_num new_s in
      let first = int_of_string (String.sub new_s 0 end1) in
      let new_s2 = String.sub new_s end1 (String.length new_s - end1) in
      let start2 = find_num new_s2 in
      let new_s3 = String.sub new_s2 start2 (String.length new_s2 - start2) in
      let end2 = find_non_num new_s3 in
      let second = int_of_string (String.sub new_s3 0 end2) in
      INDICE(first, second);;

    let lex_indice2 (s): token = 
      let start1 = find_num s in
      let new_s = String.sub s start1 (String.length s - start1) in
      let end1 = find_non_num new_s in
      let first = int_of_string (String.sub new_s 0 end1) in
      let new_s2 = String.sub new_s end1 (String.length new_s - end1) in
      let start2 = find_num new_s2 in
      let new_s3 = String.sub new_s2 start2 (String.length new_s2 - start2) in
      let end2 = find_non_num new_s3 in
      let second = int_of_string (String.sub new_s3 0 end2) in
      INDICE(first, second);;

    let find_open_bracket s = 
      let rec find_open_bracket_util s i = if s.[i] = '[' then i
                                            else find_open_bracket_util s (i+1)
      in find_open_bracket_util s 0;;

    let find_close_bracket s = 
      let rec find_close_bracket_util s i = if s.[i] = ']' then (i+1)
                                            else find_close_bracket_util s (i+1)
      in find_close_bracket_util s 0;;

    let lex_range s =
      let start1 = find_open_bracket s in
      let new_s = String.sub s start1 (String.length s - start1) in
      let end1 = find_close_bracket new_s in
      let first = lex_indice1 (String.sub new_s 0 end1) in
      let new_s2 = String.sub new_s end1 (String.length new_s - end1) in
      let start2 = find_open_bracket new_s2 in
      let new_s3 = String.sub new_s2 start2 (String.length new_s2 - start2) in
      let end2 = find_close_bracket new_s3 in
      let second = lex_indice1 (String.sub new_s3 0 end2) in
      RANGE(first, second);;
}


let digit = ['0'-'9']
let digit_ = ['1'-'9']
(* Regex for natural numbers *)
let number = ('0'|((digit_)digit*))
(* Regex for floating constants *)
let float_constant = ['+''-']?(number)['.']('0' | digit*(digit_))
(* Regex for whitespace *)
let sp = [' ' '\t']+
(* Regex for indices *)
let indice = ['['](sp*)(number)(sp*)[','](sp*)(number)(sp*)[']']
(* Regex for ranges *)
let range = ['('](sp*)(indice)(sp*)[':'](sp*)(indice)(sp*)[')']

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
    | indice as i               {(lex_indice2 i) :: (read lexbuf)}
    | range as r                {(lex_range r) :: (read lexbuf)}
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
    | _ as s                    {raise (InvalidToken s)}

(* trailer *)
{
	let scanner s = read(Lexing.from_string s)
}
