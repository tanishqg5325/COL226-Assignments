{
  open Parser;;
  exception InvalidToken of char ;;
}

let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let var = ['A'-'Z'](alpha_num*)
let cons = ['a'-'z'](alpha_num*) | ("\"" [^ '\"']+ "\"")
let sp = [' ' '\t' '\n']+

rule read = parse
    eof                   {EOF}
  | sp                    {read lexbuf}
  | var as v              {VAR(v)}
  | cons as c             {CONS(c)}
  | '('                   {LP}
  | ')'                   {RP}
  | '['                   {LB}
  | ']'                   {RB}
  | ','                   {COMMA}
  | '='                   {EQ}
  | '|'                   {PIPE}
  | '.'                   {ENDL}
  | ":-"                  {COND}
  | '%'                   {single_line_comment lexbuf}
  | "/*"                  {multi_line_comment 0 lexbuf}
  | _ as s                {raise (InvalidToken s)}

and single_line_comment = parse
    eof                   {EOF}
  | '\n'                  {read lexbuf}
  |   _                   {single_line_comment lexbuf}

and multi_line_comment depth = parse
    eof                   {failwith "Syntax error: End of file in /* ... */ comment"}
  | "*/"                  {if depth = 0 then read lexbuf else multi_line_comment (depth-1) lexbuf}
  | "/*"                  {multi_line_comment (depth+1) lexbuf}
  |  _                    {multi_line_comment depth lexbuf}
