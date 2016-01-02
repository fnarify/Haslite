/**
 * Moama syntax analysis tests.
 *
 * Copyright 2014, Anthony Sloane, Macquarie University, All rights reserved.
 */

package haslite

import org.junit.runner.RunWith
import org.scalatest.FunSuiteLike
import org.scalatest.junit.JUnitRunner

/**
 * Tests that check that the syntax analyser works correctly.  I.e., it accepts
 * correct input and produces the appropriate trees, and it rejects illegal input.
 */
@RunWith(classOf[JUnitRunner])
class SyntaxAnalysisTests extends SyntaxAnalysis with FunSuiteLike {

    import HasliteTree._

    // Tests of parsing terminals

    test ("parsing an identifier of one letter produces the correct tree") {
        assertParseOk ("x", identifier, "x")
    }

    test ("parsing an identifier as an identifier produces the correct tree") {
        assertParseOk ("count", identifier, "count")
    }

    test ("parsing an identifier containing digits and underscores produces the correct tree") {
        assertParseOk ("x1_2_3", identifier, "x1_2_3")
    }

    test ("parsing an integer as an identifier gives an error") {
        assertParseError ("42", identifier, 1, 1,
            "identifier expected")
    }

    test ("parsing a non-identifier as an identifier gives an error (digit)") {
        assertParseError ("4foo", identifier, 1, 1,
            "identifier expected")
    }

    test ("parsing a non-identifier as an identifier gives an error (underscore)") {
        assertParseError ("_f3", identifier, 1, 1,
            "identifier expected")
    }

    test ("parsing a keyword as an identifier gives an error") {
        assertParseError ("let ", identifier, 1, 1, "Expected failure")
    }

    test ("parsing a keyword prefix as an identifier produces the correct tree") {
        assertParseOk ("letter", identifier, "letter")
    }

    test ("parsing an integer of one digit as an integer produces the correct tree") {
        assertParseOk ("8", integer, "8")
    }

    test ("parsing an integer as an integer produces the correct tree") {
        assertParseOk ("99", integer, "99")
    }

    test ("parsing a non-integer as an integer gives an error") {
        assertParseError ("total", integer, 1, 1,
            "string matching regex `[0-9]+' expected but `t' found")
    }

    // Tests of parsing basic expressions

    test ("parsing an equal expression produces the correct tree") {
        assertParseOk ("a == 1", exp, EqualExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a less than expression produces the correct tree") {
        assertParseOk ("a < 1", exp, LessExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an addition expression produces the correct tree") {
        assertParseOk ("a + 1", exp, PlusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a subtraction expression produces the correct tree") {
        assertParseOk ("a - 1", exp, MinusExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a multiplication expression produces the correct tree") {
        assertParseOk ("a * 1", exp, StarExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing a division expression produces the correct tree") {
        assertParseOk ("a / 1", exp, SlashExp (IdnUse ("a"), IntExp (1)))
    }

    test ("parsing an integer expression produces the correct tree") {
        assertParseOk ("823", exp, IntExp (823))
    }

    test ("parsing a true expression produces the correct tree") {
        assertParseOk ("true", exp, BoolExp (true))
    }

    test ("parsing a false expression produces the correct tree") {
        assertParseOk ("false", exp, BoolExp (false))
    }

    test ("parsing an identifier expression produces the correct tree") {
        assertParseOk ("v123", exp, IdnUse ("v123"))
    }

    test ("parsing a parenthesized expression produces the correct tree") {
        assertParseOk ("(a + 5)", exp,
            PlusExp (IdnUse ("a"), IntExp (5)))
    }

    test ("parsing an application expression produces the correct tree") {
        assertParseOk ("a b", exp,
            AppExp (IdnUse ("a"), IdnUse ("b")))
        assertParseOk ("(a) b", exp,
            AppExp (IdnUse ("a"), IdnUse ("b")))
        assertParseOk ("a (b)", exp,
            AppExp (IdnUse ("a"), IdnUse ("b")))
    }

    test ("parsing an if expression produces the correct tree") {
        assertParseOk ("if (true) then 3 else 4", exp,
            IfExp (BoolExp (true), IntExp (3), IntExp (4)))
    }

    test ("an if expression must have a parenthesized condition") {
        assertParseError ("if true then 3 else 4", exp, 1, 4,
            "`(' expected but `t' found")
    }

    // Tests of definitions and blocks

    test ("a value definition produces the correct tree") {
        assertParseOk ("x = 42", defn,
            Defn ("x", IntExp (42)))
    }

    test ("a function definition produces the correct tree") {
        assertParseOk ("f = \\x :: Int -> x + 1", defn,
            Defn ("f", LamExp (IdnDef ("x", IntType()), PlusExp (IdnUse ("x"), IntExp (1)))))
    }

    test ("a function definition can have many args if they are in many lambdas") {
        assertParseOk ("f = \\ x :: Int -> \\ y:: Int -> 12", defn,
            Defn ("f", LamExp (IdnDef ("x", IntType ()), LamExp (IdnDef ("y", IntType ()), IntExp (12)))))
    }

    test ("a defn with no body is an error") {
        assertParseError ("f = ", defn, 1, 5,
            "exp expected")
    }

    test ("a defn with a type is an error") {
        assertParseError ("f :: Int = \\x :: Int -> \\ y:: Int -> 12 ", defn, 1, 3,
            "`=' expected but `:' found")
    }

    test ("a let block with no body gives an error") {
        assertParseError ("let x = 5 in ", let, 1, 14,
            "exp expected")
    }

    test ("a block with no definitions gives an error") {
        assertParseError("let in 42", let, 1, 4,
            "Expected failure")
    }

    test ("a let with a single definition produces the correct tree") {
        assertParseOk ("let x = 1 in x * 2", let,
            LetExp (List (Defn ("x", IntExp (1))), StarExp (IdnUse ("x"), IntExp (2))))
    }

    test ("a let with a multiple definitions produces the correct tree") {
        assertParseOk ("let x = 1; f = \\ a :: Int -> a; g = \\ a :: Int -> a;  y = 2; h = \\a :: Int -> a in x", let,
            LetExp (List (Defn ("x", IntExp (1)),
                          Defn ("f", LamExp (IdnDef ("a", IntType ()), IdnUse ("a"))),
                          Defn ("g", LamExp (IdnDef ("a", IntType ()), IdnUse ("a"))),
                          Defn ("y", IntExp (2)),
                          Defn ("h", LamExp (IdnDef ("a", IntType ()), IdnUse ("a")))
                         )
                    ,
                    IdnUse ("x")
                    ))
    }

    // Tests of operator associativity

    test ("conditional expressions are right associative") {
        assertParseOk ("if (true) then 1 else if (false) then 3 else 4", exp,
            IfExp (BoolExp (true), IntExp(1), IfExp (BoolExp (false), IntExp (3), IntExp (4))))
    }

    test ("'==' is not associative with '=='") {
        assertParseError ("1 == 2 == 3", exp, 1, 12,
            "exp expected")
    }

    test ("'==' is not associative with '<'") {
        assertParseError ("1 == 2 < 3", exp, 1, 11,
            "exp expected")
    }

    test ("'<' is not associative with '<'") {
        assertParseError ("1 < 2 < 3", exp, 1, 10,
            "exp expected")
    }

    test ("'<' is not associative with '=='") {
        assertParseError ("1 < 2 == 3", exp, 1, 11,
            "exp expected")
    }

    test ("'+' is left associative") {
        assertParseOk ("1 + 2 + 3", exp,
            PlusExp (PlusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'-' is left associative") {
        assertParseOk ("1 - 2 - 3", exp,
            MinusExp (MinusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'*' is left associative") {
        assertParseOk ("1 * 2 * 3", exp,
            StarExp (StarExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'/' is left associative") {
        assertParseOk ("1 / 2 / 3", exp,
            SlashExp (SlashExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    // Tests of operator precedence

    test ("'if' has a lower precedence than '=='") {
        assertParseOk ("if (true) then 3 else 4 == 5", exp,
            IfExp (BoolExp (true), IntExp (3), EqualExp (IntExp (4), IntExp (5))))
    }

    test ("'if' has a lower precedence than '<'") {
        assertParseOk ("if (true) then 3 else 4 < 5", exp,
            IfExp (BoolExp (true), IntExp (3), LessExp (IntExp (4), IntExp (5))))
    }

    test ("'==' has a lower precedence than '+' (left)") {
        assertParseOk ("1 == 2 + 3", exp,
            EqualExp (IntExp (1), PlusExp (IntExp (2), IntExp (3))))
    }

    test ("'==' has a lower precedence than '+' (right)") {
        assertParseOk ("1 + 2 == 3", exp,
            EqualExp (PlusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'==' has a lower precedence than '-' (left)") {
        assertParseOk ("1 == 2 - 3", exp,
            EqualExp (IntExp (1), MinusExp (IntExp (2), IntExp (3))))
    }

    test ("'==' has a lower precedence than '-' (right)") {
        assertParseOk ("1 - 2 == 3", exp,
            EqualExp (MinusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'<' has a lower precedence than '+' (left)") {
        assertParseOk ("1 < 2 + 3", exp,
            LessExp (IntExp (1), PlusExp (IntExp (2), IntExp (3))))
    }

    test ("'<' has a lower precedence than '+' (right)") {
        assertParseOk ("1 + 2 < 3", exp,
            LessExp (PlusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'<' has a lower precedence than '-' (left)") {
        assertParseOk ("1 < 2 - 3", exp,
            LessExp (IntExp (1), MinusExp (IntExp (2), IntExp (3))))
    }

    test ("'<' has a lower precedence than '-' (right)") {
        assertParseOk ("1 - 2 < 3", exp,
            LessExp (MinusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'+' has a same precedence as '-'") {
        assertParseOk ("1 + 2 - 3", exp,
            MinusExp (PlusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'-' has a same precedence as '+'") {
        assertParseOk ("1 - 2 + 3", exp,
            PlusExp (MinusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'+' has a lower precedence than '*' (left)") {
        assertParseOk ("1 + 2 * 3", exp,
            PlusExp (IntExp (1), StarExp (IntExp (2), IntExp (3))))
    }

    test ("'+' has a lower precedence than '*' (right)") {
        assertParseOk ("1 * 2 + 3", exp,
            PlusExp (StarExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'+' has a lower precedence than '/' (left)") {
        assertParseOk ("1 + 2 / 3", exp,
            PlusExp (IntExp (1), SlashExp (IntExp (2), IntExp (3))))
    }

    test ("'+' has a lower precedence than '/' (right)") {
        assertParseOk ("1 / 2 + 3", exp,
            PlusExp (SlashExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'-' has a lower precedence than '*' (left)") {
        assertParseOk ("1 - 2 * 3", exp,
            MinusExp (IntExp (1), StarExp (IntExp (2), IntExp (3))))
    }

    test ("'-' has a lower precedence than '*' (right)") {
        assertParseOk ("1 * 2 - 3", exp,
            MinusExp (StarExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'-' has a lower precedence than '/' (left)") {
        assertParseOk ("1 - 2 / 3", exp,
            MinusExp (IntExp (1), SlashExp (IntExp (2), IntExp (3))))
    }

    test ("'-' has a lower precedence than '/' (right)") {
        assertParseOk ("1 / 2 - 3", exp,
            MinusExp (SlashExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("parentheses override associativity") {
        assertParseOk ("1 + (2 + 3)", exp,
            PlusExp (IntExp (1), PlusExp (IntExp (2), IntExp (3))))
    }

    test ("parentheses override precedence (left)") {
        assertParseOk ("(1 + 2) * 3", exp,
            StarExp (PlusExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("parentheses override precedence (right)") {
        assertParseOk ("1 * (2 + 3)", exp,
            StarExp (IntExp (1), PlusExp (IntExp (2), IntExp (3))))
    }

    test ("'*' has a same precedence as '/'") {
        assertParseOk ("1 * 2 / 3", exp,
            SlashExp (StarExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    test ("'/' has a same precedence as '*'") {
        assertParseOk ("1 / 2 * 3", exp,
            StarExp (SlashExp (IntExp (1), IntExp (2)), IntExp (3)))
    }

    // Tests of parsing types

    test ("parsing the int type produces the correct tree") {
        assertParseOk ("Int", tipe, IntType ())
    }

    test ("parsing the bool type produces the correct tree") {
        assertParseOk ("Bool", tipe, BoolType ())
    }

    test ("parsing a function type produces the correct tree") {
        assertParseOk ("Bool -> Int", tipe, FunType (BoolType (), IntType ()))
    }

    test ("function types are right associative") {
        assertParseOk ("Bool -> Int -> Bool", tipe, FunType (BoolType (), FunType (IntType (), BoolType ())))
    }

    test ("parentheses can override associativity of function types") {
        assertParseOk ("(Bool -> Int) -> Bool", tipe, FunType (FunType (BoolType (), IntType ()), BoolType ()))
    }

    // Tests of parsing programs

    test ("a program can be a single expression") {
        assertParseOk ("1 + 2", program,
            Program (PlusExp (IntExp (1), IntExp (2))))
    }

    test ("a program can be more than one expression if the parser can find an application in there") {
        assertParseOk ("(1 + 2) (3 * 4)", program,
            Program(AppExp(PlusExp(IntExp(1),IntExp(2)),StarExp(IntExp(3),IntExp(4)))))
        assertParseOk ("1 + 2 3 * 4", program,
            Program(AppExp(PlusExp(IntExp(1),IntExp(2)),StarExp(IntExp(3),IntExp(4)))))
    }


    /**
     * Assert that a parsing operation should be performed correctly.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to succeed and to produce the given result.  Fail if `p` doesn't
     * produce the given result or if `p` doesn't consume all of the input.
     */
    def assertParseOk[T] (str : String, p : Parser[T], result : T) {
        parseAll (p, str) match {
            case Success (r, in) =>
                if (r != result) fail ("found '" + r + "' not '" + result + "'")
                if (!in.atEnd) fail ("input remaining at " + in.pos)
            case f : Error =>
                fail ("parse error: " + f)
            case f : Failure =>
                fail ("parse failure: " + f)
        }
    }

    /**
     * Assert that a parsing operation should not result in success.
     * Try to parse `str` as a `T` using the parser `p`, which is expected
     * to not succeed, giving either a fatal error or failure (as specified
     * by the `iserr` parameter, which defaults to failure). Fail the test
     * if the parsing operation succeeds. Furthermore, fail the test if it
     * fails, but the error or failure is not indicated at the given `line`
     * and `column` location or doesn't contain the given message `msg`.
     */
    def assertParseError[T] (str : String, p : Parser[T], line : Int, column : Int,
                             msg : String, iserr : Boolean = false) {
        parseAll (p, str) match {
            case Success (r, _) =>
                fail ("expected to find parse error in " + str + " but it succeeded with " + r)
            case e : NoSuccess =>
                if (iserr && e.isInstanceOf[Failure])
                    fail ("got parse failure when expecting parse error")
                else if (!iserr & e.isInstanceOf[Error])
                    fail ("got parse error when expecting parse failure")
                assertResult (msg, "wrong message in error") (e.msg)
                assertResult (line, "wrong line number in error") (e.next.pos.line)
                assertResult (column, "wrong column number in error") (e.next.pos.column)
        }
    }

}
