(* Header *)
{
  open Parser;;
  exception InvalidToken of char ;;
}

let alpha_num = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let var = ['A'-'Z'](alpha_num*)
let cons = ['a'-'z'](alpha_num*)
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
  | '.'                   {ENDL}
  | ":-"                  {COND}
  | _ as s                {raise (InvalidToken s)}
