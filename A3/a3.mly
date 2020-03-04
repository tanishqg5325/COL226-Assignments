%{
    open Sheet
    let s = [[1.; 2.]; [3.; 4.]];;
%}

%token <float> FLOAT
%token <int * int> INDICE
%token <Sheet.index * Sheet.index> RANGE
%token LP RP LB RB COMMA COLON COUNT ROWCOUNT COLCOUNT SUM ROWSUM COLSUM AVG ROWAVG COLAVG MIN ROWMIN COLMIN MAX ROWMAX COLMAX ADD SUBT MULT DIV EQ DELIMITER
%start main
%type <Sheet.sheet> main
%%

main:
    INDICE EQ COUNT RANGE DELIMITER       {full_count s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ COLCOUNT RANGE DELIMITER    {col_count s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ ROWCOUNT RANGE DELIMITER    {row_count s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}

  | INDICE EQ SUM RANGE DELIMITER         {full_sum s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ ROWSUM RANGE DELIMITER      {row_sum s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ COLSUM RANGE DELIMITER      {col_sum s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}

  | INDICE EQ AVG RANGE DELIMITER         {full_avg s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ ROWAVG RANGE DELIMITER      {row_avg s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ COLAVG RANGE DELIMITER      {col_avg s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}

  | INDICE EQ MIN RANGE DELIMITER         {full_min s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ ROWMIN RANGE DELIMITER      {row_min s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ COLMIN RANGE DELIMITER      {col_min s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  
  | INDICE EQ MAX RANGE DELIMITER         {full_max s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ ROWMAX RANGE DELIMITER      {row_max s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}
  | INDICE EQ COLMAX RANGE DELIMITER      {col_max s (RANGE((fst $4),(snd $4))) (INDICE((fst $1),(snd $1)))}

  | INDICE EQ ADD RANGE RANGE DELIMITER   {add_range(s,$4,$5,$1)}
  | INDICE EQ ADD FLOAT RANGE DELIMITER   {add_const(s,$5,$4,$1)}
  | INDICE EQ ADD RANGE FLOAT DELIMITER   {add_range(s,$4,$5,$1)}
;
