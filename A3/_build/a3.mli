type token =
  | FLOAT of (float)
  | INDICE of (int * int)
  | RANGE of (Sheet.index * Sheet.index)
  | LP
  | RP
  | LB
  | RB
  | COMMA
  | COLON
  | COUNT
  | ROWCOUNT
  | COLCOUNT
  | SUM
  | ROWSUM
  | COLSUM
  | AVG
  | ROWAVG
  | COLAVG
  | MIN
  | ROWMIN
  | COLMIN
  | MAX
  | ROWMAX
  | COLMAX
  | ADD
  | SUBT
  | MULT
  | DIV
  | EQ
  | DELIMITER
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Sheet.formula
