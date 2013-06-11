%{
(* CSE 130: Programming Assignment 4
 * nanoParse.mly
 * Author: Daniel Shipps (A10239760)
 * Date: 5/19/2013 *)

(* See this for a tutorial on ocamlyacc 
 * http://plus.kaist.ac.kr/~shoh/ocaml/ocamllex-ocamlyacc/ocamlyacc-tutorial/ *)
open Nano 
%}

%token <int> Num
%token EOF

(*%token HD
%token TL*)

%token COLONCOLON
%token SEMI
%token RBRAC
%token LBRAC

%token RPAREN
%token LPAREN

%token OR
%token AND
%token NE
%token LE
%token LT
%token DIV
%token MUL
%token MINUS
%token PLUS

%token ELSE
%token THEN
%token IF
%token ARROW
%token FUN
%token IN
%token EQ
%token REC
%token LET

%token <string> Id
%token False
%token True

%nonassoc LET FUN IF
%left OR
%left AND
%left EQ NE LT LE
%right COLONCOLON SEMI RBRAC
%left PLUS MINUS
%left MUL DIV
%left APP

%start exp 
%type <Nano.expr> exp

%%

exp:
    | LET Id EQ exp IN exp      { Let($2,$4,$6) }
    | LET REC Id EQ exp IN exp  { Letrec($3,$5,$7) }
    | FUN Id ARROW exp          { Fun($2,$4) }
    | IF exp THEN exp ELSE exp  { If($2,$4,$6) }
    | exp2                      { $1 }

exp2:
    | exp2 MUL exp7             { Bin($1,Mul,$3)}
    | exp2 DIV exp7             { Bin($1,Div,$3)}
    | exp3                      { $1 }		
	
exp3:
    | exp3 PLUS exp2            { Bin($1,Plus,$3) }
    | exp3 MINUS exp2           { Bin($1,Minus,$3) }
    | exp4                      { $1 }	

exp4:
    | exp4 EQ exp6              { Bin($1,Eq,$3) }
    | exp4 NE exp6              { Bin($1,Ne,$3) }
    | exp4 LT exp6              { Bin($1,Lt,$3) }
    | exp4 LE exp6              { Bin($1,Le,$3) }
    | exp5                      { $1 }
	
exp5:
	| exp5 AND exp4             { Bin($1,And,$3) }
    | exp5 OR exp4              { Bin($1,Or,$3) }
    | exp6                      { $1 }	

exp6:
    | exp3 COLONCOLON exp6      { Bin($1,Cons,$3) }
    | exp3 SEMI exp6            { Bin($1,Cons,$3) }
    | exp6 RBRAC                { Bin($1,Cons,NilExpr) }
    | LBRAC exp6                { $2 }
    | exp7                      { $1 }

exp7:
    | exp7 exp8                 { App($1,$2) }
    | exp8                      { $1 }

exp8:
    | Num                       { Const($1) }
    | Id                        { Var($1) }
    | True                      { True }
    | False                     { False }
(*	| HD						{ Head }
	| TL						{ Tail } *)
    | LPAREN exp RPAREN         { $2 }
    | LBRAC RBRAC               { NilExpr }
   
   
   
   
   
   
   
   
   
   
   
   