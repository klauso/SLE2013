//This is a simple example of using Antlr3.
//Call our grammar Regions.

grammar Regions;

// This input is used to generate code.
// Hence, insert some snippets into the output.

@header{
  package regionsparser;
  import regions.*;
}

@lexer::header{
  package regionsparser;
}

@members {
  public static void main(String[] args) throws Exception {
        RegionsLexer lex = new RegionsLexer(new ANTLRFileStream(args[0]));
        CommonTokenStream tokens = new CommonTokenStream(lex);
        RegionsParser parser = new RegionsParser(tokens);

        try {
            System.out.println(parser.region());
        } catch (RecognitionException e)  {
            e.printStackTrace();
        }
    }
}

//We can verify the above code shows up literally.

//A first technological issue is that editor support for this language mixture
//is often hard.


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

// The lexer splits the input stream into a set of tokens before grammatical analysis.
// The above rules associate a regular expression to each token.

/*
 * Now, we have our parser.
 * It combines a grammar and a set of actions.
 * The grammar below would be written, in EBNF, as:
start
  ::= region EOF
region
  ::= 'unitcircle'
  |   'union' '(' region ',' region ')'
  |   'scale' '(' floatNumber ',' region ')'
 */

// Define the initial non-terminal.
start :  region EOF;

// This action returns a Region. We can set the return value by setting 'r'.
region returns [Region r]:
     //When this alternative matches, we execute the semantic action inside braces. Braces inside the code must balance!
     UNITCIRCLE { r = new UnitCircle(); }
     |
     // Here, we additionally save the values parsed by the occurrences of region.
     UNION LPAR r1=region COMMA r2=region RPAR
     { r = new UnionRegion(r1,r2); }
     |
     SCALE LPAR n=floatNumber COMMA r1=region RPAR
     { r = new ScaleRegion(n,r1); }
     ;

//What happens if we try to rename this rule to float?
//float returns [double value]
floatNumber returns [double value]
     :   d=DOUBLE { value = Double.parseDouble(d.getText());}
     ;


//What happens if we add an error to our source code?
//    :   i=INT {value = Integer.parseInt(i.text);} ;

//We compare with SBT, even though the domain is different, because we are discussing language implementation techniques.
//The similarity is in the mixture of external syntax and target syntax.

//1. Syntax bugs in semantic actions appear literally in the output.
// Errors are in terms of the generated code.
// Hence, we have to look at the output. At least, the tool supports us in doing that with extra comments.
//
// - This affects composability. Since this tool passes the code through, you can try to use a further preprocessor on the output.
// If this would parse the Java code, composability might be harder.
//
// - This DSL is *not* a proper abstraction. We are not hidden from the code.
//   The point of Antlr is that the generated code follows the structure from the grammar, so it is comparatively easy to debug.
//   However, this is still "not too bad", but not very elegant.

//In SBT, there is not really code generation: the code you write must have type
//`Setting[T] forSome {type T}`, and you get errors in terms of your code.

//2. Antlr can (and does) generate code which uses a library.
//
//Like SBT, it is just much more extreme there because the library has a rather
//convenient syntax. So the desugaring is more transparent for the user.

//This is
//an interesting evaluation criteria, because it affects transparency of the
//abstraction.

//"Generation time" (where you can do lots of optimization, by complicating the desugaring).
//"execution time"

//LR parsers have a much more complicated generation/optimization step, so the
//generated code is impossible to follow and to debug.

//Discussing about optimizations brings us to the next criteria.
//
//Let's take a real-world Antlr 3 lexer - the implementation of Antlr 4:
//
//https://github.com/antlr/antlr4/blob/master/tool/src/org/antlr/v4/parse/ANTLRLexer.g#L204

//References on Antlr 3 itself:
//http://www.antlr.org/wiki/display/ANTLR3/Five+minute+introduction+to+ANTLR+3
//http://www.antlr.org/wiki/display/ANTLR3/Quick+Starter+on+Parser+Grammars+-+No+Past+Experience+Required
//http://www.antlr.org/wiki/display/ANTLR3/FAQ+-+Getting+Started
