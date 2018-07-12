#lang brag
/program : stmt *

@stmt : rel | data | fact | rule | query | next

/rel : REL ID /SLASH NUMBER /DOT
/data : DATA ID [/SLASH NUMBER] /DOT
fact : clause /DOT
rule : clause IMPLIED-BY clauses /DOT
query : clause QMARK
/next : NEXT /DOT

@clauses : clause | clause /COMMA clauses
/clause : ID /LPAREN terms /RPAREN | ID
@terms : term | term /COMMA terms
@term : NUMBER | STRING | qterm | dterm | ID
/qterm : QUOTE ID
/dterm : ID /LPAREN terms /RPAREN
