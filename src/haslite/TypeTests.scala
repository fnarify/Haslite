package haslite

/**
 * Tests that check that the type analyser detects the appropriate errors.
 */
class TypeTests extends SemanticTests {

    import HasliteTree._
    import org.kiama.attribution.Attribution.initTree
    import scala.collection.immutable.Seq

    /**
     * Parse some test input and check that its type is as expected.
     */
    def typeTest (str : String, expectedType : Type) {
        val ast = parseProgram (str)
        initTree (ast)
        val tipe = SemanticAnalysis.tipe (ast.exp)
        assertResult (expectedType) (tipe)
    }

    // Basic expression type tests

    test ("a number expression is of integer type") {
        typeTest ("42", IntType ())
    }

    test ("true is of Boolean type") {
        typeTest ("true", BoolType ())
    }

    test ("false is of Boolean type") {
        typeTest ("false", BoolType ())
    }

    test ("a name that is not defined is of unknown type") {
        typeTest ("x", UnknownType ("variable not defined"))
    }

    test ("a block's type is the type of its final expression (val)") {
        typeTest ("let x = 1 in x", IntType ())
    }

    test ("a block's type is the type of its final expression") {
        typeTest ("let f = \\ x :: Int -> x in f", FunType (IntType (), IntType ()))
    }

    test ("a primitive addition is of integer type") {
        typeTest ("1 + 2", IntType ())
    }

    test ("a primitive divide is of integer type") {
        typeTest ("8 / 4", IntType ())
    }

    test ("a primitive multiplication is of integer type") {
        typeTest ("3 * 2", IntType ())
    }

    test ("a primitive subtraction is of integer type") {
        typeTest ("5 - 8", IntType ())
    }

    test ("a primitive less than is of Boolean type") {
        typeTest ("10 < 3", BoolType ())
    }

    test ("a primitive equality of integers is of Boolean type") {
        typeTest ("10 == 3", BoolType ())
    }

    test ("the type of a fun application is the fun result type") {
        typeTest ("let f = \\ x :: Int -> true  in f 42", BoolType ())
    }

    test ("a conditional expression's type is the type of its first branch - Int") {
        typeTest ("if (true) then 3 else 4", IntType ())
    }

    test ("a conditional expression's type is the type of its first branch - Bool") {
        typeTest ("if (true) then true else false", BoolType ())
    }

    test ("a conditional expression's type is the type of its first branch - Fun") {
        typeTest ("let f = \\ x :: Int -> true in if (true) then f else f",
                FunType(IntType(), BoolType ()))
    }

    // Tests of type compatibility

    test ("the left argument of an addition can't be a Boolean") {
        val messages = semanticTest ("true + 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the right argument of an addition can't be a Boolean") {
        val messages = semanticTest ("1 + false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the left argument of a division can't be a Boolean") {
        val messages = semanticTest ("true / 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the right argument of a division can't be a Boolean") {
        val messages = semanticTest ("1 / false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the left argument of a multiplication can't be a Boolean") {
        val messages = semanticTest ("true * 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the right argument of a multiplication can't be a Boolean") {
        val messages = semanticTest ("1 * false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the left argument of a subtraction can't be a Boolean") {
        val messages = semanticTest ("true - 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the right argument of a subtraction can't be a Boolean") {
        val messages = semanticTest ("1 - false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the left argument of an equality than can't be a Boolean") {
        val messages = semanticTest ("true == 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the right argument of a equality can't be a Boolean") {
        val messages = semanticTest ("1 == false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("cannot test equality of two Booleans") {
        val messages = semanticTest ("true == false")
        assert (messages.length >= 1)
    }

    test ("the left argument of a less than can't be a Boolean") {
        val messages = semanticTest ("true < 1")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the right argument of a less than can't be a Boolean") {
        val messages = semanticTest ("1 < false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("the tested expression in a conditional can't be an integer") {
        val messages = semanticTest ("if (20) then 3 else 4")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Bool got Int")
    }

    test ("the else expression in a conditional must have the same type as the then expression") {
        val messages = semanticTest ("if (true) then 3 else false")
        assert (messages.length === 1)
        assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }

    test ("an applied expression must be a function") {
        val messages = semanticTest ("let x = 1  in x 10")
        assertMessage (messages, 1, 1, 15, "application of non-function Int")
    }

    test ("the arguments in a fun application must match the function") {
        val messages = semanticTest ("let f = \\ x :: Int -> x in f true")
        assertMessage (messages, 1, 1, 28, "expected Int got Bool")
    }

    test ("comparing funtype against Int inside IF") {
        val messages = semanticTest ("let f = \\x :: Int -> x in if (true) then f else 1")
        assertMessage (messages, 1, 1, 27, "expected Int -> Int got Int")
    }

    test ("comparing two different funtypes inside IF") {
        val messages = semanticTest ("let f = \\ x :: (Bool -> Int) -> x;" +
                                     "    g = \\ x :: (Int -> Bool) -> x " +
                                     " in  if (true) then f else g")
        println(messages)
        assertMessage (messages, 1, 1, 74, "expected Bool -> Int -> Bool -> Int got Int -> Bool -> Int -> Bool")
    }

    test ("comparing two identical funtypes inside IF") {
        typeTest ("let f = \\ x :: (Int -> Bool) -> x;" +
                  "    g = \\ x :: (Int -> Bool) -> x " +
                  " in  if (true) then f else g",
                  FunType (FunType (IntType (), BoolType ()), FunType (IntType (), BoolType ())))
    }

    test ("declaring the same thing twice is an error") {
        val messages = semanticTest ("let f = 3; f = true in f")
        assert (messages.length === 2)
    }

    test ("backwards references don't work in the type inference version") {
        val messages = semanticTest ("let f = 5; y = f + 5 in y + 5")
        assert (messages.length > 0)
    }

    test ("even with the recursion restrictions you can nest lets to get what you want") {
        typeTest ("let f = 5 in let y = f + 5 in y + 5", IntType ())
    }

    test ("pulling a variable from outside and using at the right type"){
        typeTest("\\ x :: Int -> let y = x in y", FunType (IntType (), IntType()))
        typeTest("\\ x :: Int -> \\ x :: Bool -> let y = x in y", FunType (IntType (), FunType (BoolType (), BoolType())))
        typeTest("\\ x :: Int -> \\ x2 :: Bool -> let y = x in y", FunType (IntType (), FunType (BoolType (), IntType())))
    }

    test ("types of parts must be correct") {
        var messages = semanticTest ("x")
        messages = semanticTest ("true + 5"); println (messages); 
          assertMessage (messages, 0, 1, 1, "expected Int got Bool")
        messages = semanticTest ("5 + false"); println (messages); 
          assert (messages.length == 1)
          assertMessage (messages, 0, 1, 1, "expected Int got Bool")
    }
}
