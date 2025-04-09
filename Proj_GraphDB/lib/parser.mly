
%token <string> IDENTIFIER
%token <Lang.attrib_tp> TP
%token <bool> BCONSTANT
%token <int> INTCONSTANT
%token <string> STRINGCONSTANT
%token BLAND BLOR
%token EQ GE GT LE LT NE
%token ADD SUB MUL DIV MOD
%token LBRACE RBRACE LBRACKET RBRACKET LPAREN RPAREN 
%token DOT COMMA COLON
%token CREATE DELETE MATCH RETURN SET WHERE
%token ARROW
%token EOF

%start<Lang.prog> main

%left BLOR
%left BLAND
%left EQ GE GT LE LT NE
%left ADD SUB
%left MUL DIV MOD

%{ open Lang %}

%%

main: prog EOF { $1 }

prog: td = list(tpDecl);  q = query 
     { let (nts, rts) = List.partition_map Fun.id td in Prog (DBG(nts, rts), q) }

tpDecl:
| n = nodeTpDecl { Either.Left n }
| r = relTpDecl { Either.Right r }


query: cls = list(clause) { Query cls }

/* TODO: to be completed */
clause: 
| CREATE; pts = separated_list(COMMA, pattern) { Create pts }
| DELETE; pts = delete_pattern { Delete pts}
| MATCH; pts = separated_list(COMMA, pattern) { Match pts }
| RETURN; vrs = separated_list(COMMA, IDENTIFIER) { Return vrs }
| SET; att = separated_list(COMMA, attrib_assign_pattern) { Set att }
| WHERE; cond = expr { Where cond }


/* TODO: to be completed */

pattern: 
| np = npattern { SimpPattern np }
| np = npattern; SUB; LBRACKET; COLON; lab = IDENTIFIER; RBRACKET; ARROW; p = pattern { CompPattern(np, lab, p) }

npattern: 
| LPAREN; v = IDENTIFIER; COLON; t = IDENTIFIER; RPAREN { DeclPattern(v, t) }
| LPAREN; v = IDENTIFIER; RPAREN { VarRefPattern(v) }

delete_pattern:
| n = separated_list(COMMA, IDENTIFIER) { DeleteNodes n}
/*| r = separated_list(COMMA, rpattern) { DeleteRels r }*/

/*
rpattern:
v1 = IDENTIFIER; SUB; LBRACKET; COLON; label = IDENTIFIER; RBRACKET; ARROW; v2 = IDENTIFIER { (v1, label, v2) }
*/
attrib_assign_pattern:
v = IDENTIFIER; DOT; attr = IDENTIFIER; EQ; e = expr { (v, attr, e) }

/* Expressions */

primary_expr:
| vn = IDENTIFIER; DOT; fn = IDENTIFIER 
     { AttribAcc(vn, fn) }
| c = BCONSTANT
     { Const(BoolV(c)) }
| c = INTCONSTANT
     { Const(IntV(c)) }
| c = STRINGCONSTANT
     { Const(StringV(c)) }
| LPAREN e = expr RPAREN
     { e }

/* TODO: to be completed */
expr:
| a = primary_expr { a }
| e1 = expr; ADD; e2=expr { BinOp(BArith BAadd, e1, e2) }
| e1 = expr; SUB; e2=expr { BinOp(BArith BAsub, e1, e2) }
| e1 = expr; MUL; e2=expr { BinOp(BArith BAmul, e1, e2) }
| e1 = expr; DIV; e2=expr { BinOp(BArith BAdiv, e1, e2) }
| e1 = expr; MOD; e2=expr { BinOp(BArith BAmod, e1, e2) }
| e1 = expr; EQ; e2=expr { BinOp(BCompar BCeq, e1, e2) }
| e1 = expr; GE; e2=expr { BinOp(BCompar BCge, e1, e2) }
| e1 = expr; GT; e2=expr { BinOp(BCompar BCgt, e1, e2) }
| e1 = expr; LE; e2=expr { BinOp(BCompar BCle, e1, e2) }
| e1 = expr; LT; e2=expr { BinOp(BCompar BClt, e1, e2) }
| e1 = expr; NE; e2=expr { BinOp(BCompar BCne, e1, e2) }
| e1 = expr; BLAND; e2=expr { BinOp(BLogic BLand, e1, e2) }
| e1 = expr; BLOR; e2=expr { BinOp(BLogic BLor, e1, e2) }

/* Types */
nodeTpDecl: LPAREN; COLON; i = IDENTIFIER; a = attrib_declList; RPAREN  { DBN (i, a) }

attrib_decl: i = IDENTIFIER; t = TP { (i, t) }
attrib_declList: 
| LBRACE; ads = separated_list(COMMA, attrib_decl); RBRACE { ads }


/* Relational type declarations of the form (:nt1) -[:rt]-> (:nt2)
 */
nodeTpRef: LPAREN; COLON; si = IDENTIFIER; RPAREN { si }
relTpDecl: si = nodeTpRef;
           SUB; LBRACKET; COLON; rlab = IDENTIFIER; RBRACKET; ARROW; 
           ti = nodeTpRef
           { Graphstruct.DBR (si, rlab, ti) }

%%
