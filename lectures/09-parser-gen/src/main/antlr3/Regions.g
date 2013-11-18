//Call our grammar Regions.

grammar Regions;

options { language = Scala; }
@header{
  package regionsparser
  import regions._

//  object RegionsParser {
//  }
}

@lexer::header{
  package regionsparser
}

@members {
      def main(args: Array[String]) {
        val lex = new RegionsLexer(new ANTLRFileStream(args(0)))
        val tokens = new CommonTokenStream(lex)
        val parser = new RegionsParser(tokens)

        println(parser.region())
    }
}

start :  region EOF;

//1. Syntax bugs in semantic actions show up literally.
//Errors are in terms of the generated code.
//2. You can generate code which uses a library.
//
//We compare with SBT, even though the domain is different, because we are discussing language implementation techniques.
//The similarity is in the mixture of external syntax and target syntax.
//
//Like SBT, it is just much more extreme there because the library has a rather
//convenient syntax. So the desugaring is more transparent for the user.
//
//This is
//an interesting evaluation criteria, because it affects transparency of the
//abstraction.
//
//"Generation time" (where you can do lots of optimization, by complicating the desugaring).
//"execution time"
//
//LR parsers have a much more complicated generation/optimization step, so the
//generated code is impossible to follow and to debug.
//
region returns [Region r]:
     UNITCIRCLE { r = new UnitCircle(); }
     |
     UNION LPAR r1=region COMMA r2=region RPAR
     { r = new UnionRegion(r1,r2); }
     |
     //SCALE LPAR n=floatNumber COMMA r1=region RPAR
     SCALE LPAR n=floatNumber COMMA r1=region RPAR
     { r = new ScaleRegion(n,r1); }
     ;

//What happens if we try to use this rule?
//float returns [double value]
floatNumber returns [Double value]
    :   d=DOUBLE {value = Double.parseDouble(d.getText()); } ;


//number returns [int value]
//    :   i=INT {value = Integer.parseInt(i.getText());} ;
////What happens if we add an error to our source code?
////    :   i=INT {value = Integer.parseInt(i.text);} ;

// Lexer rules

UNION : 'union' ;
SCALE : 'scale' ;
UNITCIRCLE : 'unitcircle' ;
LPAR : '(';
RPAR : ')';
COMMA : ',';
//INT :   DIGIT+ ;
DOUBLE : DIGIT+ ('.' DIGIT+)?;
fragment DIGIT : '0' .. '9';

// Ignore whitespace.
WHITESPACE : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+    { $channel = HIDDEN; } ;
