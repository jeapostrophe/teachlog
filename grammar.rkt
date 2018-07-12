#lang brag
/program : stmt *

@stmt : rel | data | fact | rule | query | next

/rel : REL name /SLASH num /DOT
/data : DATA name [/SLASH num] /DOT
fact : clause /DOT
rule : clause IMPLIED-BY clauses /DOT
query : clause QMARK
/next : NEXT /DOT

@clauses : clause | clause /COMMA clauses
/clause : name /LPAREN terms /RPAREN | name
@terms : term | term /COMMA terms
@term : NUMBER | STRING | qterm | dterm | ID
/qterm : QUOTE ID
/dterm : ID /LPAREN terms /RPAREN

@num : NUMBER
@name : ID
