%{
    open Sheet;;
%}

%token <float> FLOAT
%token <int * int> INDICE
%token <Sheet.index * Sheet.index> RANGE
%token LP RP LB RB COMMA COLON COUNT ROWCOUNT COLCOUNT SUM ROWSUM COLSUM AVG ROWAVG COLAVG MIN ROWMIN COLMIN MAX ROWMAX COLMAX ADD SUBT MULT DIV EQ DELIMITER EOF
%start main
%type <Sheet.formula> main
%%

main:
  formulas EOF              {$1}
;

formulas:
    INDICE EQ unary RANGE DELIMITER           {UNARY(INDICE((fst $1),(snd $1)), $3, RANGE((fst $4),(snd $4)))}
  | INDICE EQ binary RANGE RANGE DELIMITER    {BINARY1(INDICE((fst $1),(snd $1)), $3, RANGE((fst $4),(snd $4)), RANGE((fst $5),(snd $5)))}
  | INDICE EQ binary FLOAT RANGE DELIMITER    {BINARY2(INDICE((fst $1),(snd $1)), $3, $4, RANGE((fst $5),(snd $5)))}
  | INDICE EQ binary RANGE FLOAT DELIMITER    {BINARY3(INDICE((fst $1),(snd $1)), $3, RANGE((fst $4),(snd $4)), $5)}
  | INDICE EQ binary INDICE RANGE DELIMITER   {BINARY4(INDICE((fst $1),(snd $1)), $3, INDICE((fst $4),(snd $4)), RANGE((fst $5),(snd $5)))}
  | INDICE EQ binary RANGE INDICE DELIMITER   {BINARY5(INDICE((fst $1),(snd $1)), $3, RANGE((fst $4),(snd $4)), INDICE((fst $5),(snd $5)))}
;

unary:
    COUNT         {COUNT}
  | ROWCOUNT      {ROWCOUNT}
  | COLCOUNT      {COLCOUNT}
  | SUM           {SUM}
  | ROWSUM        {ROWSUM}
  | COLSUM        {COLSUM}
  | AVG           {AVG}
  | ROWAVG        {ROWAVG}
  | COLAVG        {COLAVG}
  | MIN           {MIN}
  | ROWMIN        {ROWMIN}
  | COLMIN        {COLMIN}
  | MAX           {MAX}
  | ROWMAX        {ROWMAX}
  | COLMAX        {COLMAX}
;

binary:
    ADD           {ADD}
  | SUBT          {SUBT}
  | MULT          {MULT}
  | DIV           {DIV}
;
