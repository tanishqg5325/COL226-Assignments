%{
    open Interpreter;;
%}

%token <string> VAR CONS
%token LP RP LB RB COMMA EQ ENDL COND EOF

%start program goal
%type <Interpreter.program> program
%type <Interpreter.goal> goal
%%

program:
    EOF                                 {[]}
  | clause_list EOF                     {$1}
;

clause_list:
    clause                              {[$1]}
  | clause clause_list                  {($1)::$2}
;

clause:
    atom ENDL                           {F(H($1))}
  | atom COND atom_list ENDL            {R(H($1), B($3))}
;

goal:
    atom_list ENDL                      {G($1)}
;

atom_list:
    atom                                {[$1]}
  | atom COMMA atom_list                {($1)::$3}
;

atom:
    LP atom RP                          {$2}
  | CONS                                {A($1, [])}
  | CONS LP term_list RP                {A($1, $3)}
  | term EQ term                        {A("_eq", [$1; $3])}
;

term_list:
    term                                {[$1]}
  | term COMMA term_list                {($1)::$3}
;

term:
    /* LP term RP                          {$2} */
  | VAR                                 {V($1)}
  | CONS                                {Node($1, [])}
  | CONS LP term_list RP                {Node($1, $3)}
;
