grammar SimpleCalc;

options {
    //http://www.antlr.org/wiki/display/ANTLR3/Grammar+options
    output = AST;
}

tokens {
    PLUS    = '+' ;
    MINUS   = '-' ;
    MULT    = '*' ;
    DIV = '/' ;
}

@header {
import org.antlr.stringtemplate.*;
}

@members {
    //Pretty-print a tree. Inspired by CommonTree's docs:
    //http://www.antlr3.org/api/Java/org/antlr/runtime/tree/CommonTree.html
    public static void showTree(CommonTree tree, int indent) {
        for (int i = 0; i < indent; i++)
            System.out.print(' ');
        System.out.println(tree);
        if (tree.getChildren() == null)
            return;
        for (Object childObj : tree.getChildren()) {
            try {
                showTree((CommonTree) childObj, indent + 2);
            } catch (Exception e) {
                e.printStackTrace(); //And resume from next child!
            }
        }
    }

    public static void showTree(CommonTree tree) {
        showTree(tree, 0);
    }

    /**
     * Generate a DOT tree representing the parse tree.
     * To render the tree, use GraphViz's `dot` program.
     */
    //Source extracted from http://stackoverflow.com/a/4933963/53974.
    public static void graphTree(CommonTree tree) {
        DOTTreeGenerator gen = new DOTTreeGenerator();
        StringTemplate st = gen.toDOT(tree);
        System.out.println(st);
    }

    private static boolean graphTree = false;
    public static void main(String[] args) throws Exception {
        SimpleCalcLexer lex = new SimpleCalcLexer(new ANTLRFileStream(args[0]));
        CommonTokenStream tokens = new CommonTokenStream(lex);

        SimpleCalcParser parser = new SimpleCalcParser(tokens);

        try {
            CommonTree tree = (CommonTree) parser.expr().getTree();
            if (graphTree)
                graphTree(tree);
            else
                showTree(tree);
        } catch (RecognitionException e)  {
            e.printStackTrace();
        }
    }
}

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

//A simpler version of the grammar (from http://www.antlr.org/wiki/display/ANTLR3/Five+minute+introduction+to+ANTLR+3).
/*
expr    : term ( ( PLUS | MINUS )  term )* ;

term    : factor ( ( MULT | DIV ) factor )* ;

factor  : NUMBER ;
 */

// The ^ annotations shape the tree; they follow nodes to be marked as root nodes.
// Similarly, one can use ! to annotate a token which must disappear from the AST.
// http://www.antlr.org/wiki/display/ANTLR3/Tree+construction, section on Operators.

start : expr^ EOF;

expr    : addend ( ( PLUS | MINUS )^  expr )? ;

addend  : factor ( ( MULT | DIV )^ addend )? ;

factor  : NUMBER ;

/*------------------------------------------------------------------
 * LEXER RULES
 *------------------------------------------------------------------*/

NUMBER  : (DIGIT)+ ;

//Hide whitespace away.
WHITESPACE : ( '\t' | ' ' | '\r' | '\n'| '\u000C' )+    { $channel = HIDDEN; } ;

fragment DIGIT  : '0'..'9' ;
