/********************************************************************************
*
* File: spl.lex
* The SPL scanner
*
********************************************************************************/

package edu.uta.spl;

import java_cup.runtime.Symbol;

%%
%class SplLex
%public
%line
%column
%cup

DIGIT=[0-9]
ID=[a-zA-Z][a-zA-Z0-9_]*

%{

  private Symbol symbol ( int type ) {
    return new Symbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol ( int type, Object value ) {
    return new Symbol(type, yyline+1, yycolumn+1, value);
  }

  public void lexical_error ( String message ) {
    System.err.println("*** Lexical Error: " + message + " (line: " + (yyline+1)
                       + ", position: " + (yycolumn+1) + ")");
    System.exit(1);
  }
%}

%%

{DIGIT}+                { return symbol(sym.INTEGER_LITERAL,new Integer(yytext())); }
{DIGIT}+"."{DIGIT}+     { return symbol(sym.FLOAT_LITERAL,new Float(yytext())); }
    
"array"         { return symbol(sym.ARRAY); }
"var"           { return symbol(sym.VAR); }

"by"            { return symbol(sym.BY); }
"exit"          { return symbol(sym.EXIT); }
"loop"          { return symbol(sym.LOOP); }
"not"           { return symbol(sym.NOT); }
"int"           { return symbol(sym.INT); }
"float"         { return symbol(sym.FLOAT); }
"string"        { return symbol(sym.STRING); }
"boolean"       { return symbol(sym.BOOLEAN); }
"def"           { return symbol(sym.DEF); }
"read"          { return symbol(sym.READ); }
"to"            { return symbol(sym.TO); }
"type"          { return symbol(sym.TYPE); }

"print"         { return symbol(sym.PRINT); }
"for"           { return symbol(sym.FOR); }
"while"         { return symbol(sym.WHILE); }
"if"            { return symbol(sym.IF); }
"else"          { return symbol(sym.ELSE); }
"return"        { return symbol(sym.RETURN); }
"true"          { return symbol(sym.TRUE); }
"false"         { return symbol(sym.FALSE); }



"="             { return symbol(sym.EQUAL); }
"&&"            { return symbol(sym.AND); }
"/"             { return symbol(sym.DIV); }
"%"             { return symbol(sym.MOD); }
"||"            { return symbol(sym.OR); }
"+"             { return symbol(sym.PLUS); }
"-"             { return symbol(sym.MINUS); }
"*"             { return symbol(sym.TIMES); }
"<"             { return symbol(sym.LT); }
"<="            { return symbol(sym.LEQ); }
">"             { return symbol(sym.GT); }
">="            { return symbol(sym.GEQ); }
"=="            { return symbol(sym.EQ); }
"<>"            { return symbol(sym.NEQ); }
":"             { return symbol(sym.COLON); }
";"             { return symbol(sym.SEMI); }
","             { return symbol(sym.COMMA); }
"#"             { return symbol(sym.SHARP); }
"."             { return symbol(sym.DOT); }
"("             { return symbol(sym.LP); }
")"             { return symbol(sym.RP); }
"{"             { return symbol(sym.LB); }
"}"             { return symbol(sym.RB); }
"["             { return symbol(sym.LSB); }
"]"             { return symbol(sym.RSB); }

{ID}            { return symbol(sym.ID,yytext()); }

\"[^\"]*\"      { return symbol(sym.STRING_LITERAL,yytext().substring(1,yytext().length()-1)); }

[ \t\r\n\f]     { /* ignore white spaces. */ }

"/*"([^*]|[*]+[^*/])*[*]+"/" { /* Ignore Comment */ }


.               { lexical_error("Illegal character"); }
