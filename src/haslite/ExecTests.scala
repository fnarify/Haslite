package haslite

/**
 * Tests that check that the translation works correctly.
 */
class ExecTests extends SemanticTests {

    import org.kiama.attribution.Attribution.initTree
    import org.kiama.util.StringEmitter
    import haslite.SECTree._

    /**
     * Parse some test input, perform semantic analysis checks, expect no
     * semantic errors. Then translate into SEC machine code and run the code.
     * The value `expected` should be the output that we expect from this
     * run.
     */
    def execTest (str : String, expected : String) {
        val tree = parseProgram (str)
        initTree (tree)
        val messages = SemanticAnalysis.errors (tree)
        assert (messages.length === 0, messages.toString)

        val instrs = Translator.translate (tree)
        // println (instrs)

        val emitter = new StringEmitter ()
        val machine = new SECMachine (emitter)

        machine.run (instrs) match {
            case _ : machine.State =>
                // Terminated correctly in some state
                assertResult (expected + "\n", "wrong execution output") (emitter.result ())
            case machine.FatalError (message) =>
                fail (message)
        }
    }
    
    // Pass-level tests
    test ("an integer expression evaluates to the correct result") {
        execTest ("""
            |1
            """.stripMargin,
            "1")
    }

    test ("an addition expression evaluates to the correct result") {
        execTest ("""
            |3 + 4
            """.stripMargin,
            "7")
    }
	
	test ("a boolean expression evaluates to the correct result") {
		execTest ("""
			|false
			""".stripMargin,
			"false")
	}

    // credit level tests

    test ("a true less-than conditional expression evaluates to the correct result") {
        execTest ("""
            |if (1 < 2) then 15 else 0
            """.stripMargin,
            "15")
    }

    test ("a false less-than conditional expression evaluates to the correct result") {
        execTest ("""
            |if (4 < 2) then 15 else 0
            """.stripMargin,
            "0")
    }

    // distinction level tests

    test ("a single def let evaluates to the correct result") {
        execTest ("""
            |let
            |   x = 1
            |in x
            |""".stripMargin,
            "1")
    }

    test ("a multiple def let evaluates to the correct result (use first def)") {
        execTest ("""
            |let
            |   x = 1;
            |   y = 2
            |in x
            """.stripMargin,
            "1")
    }

    test ("a multiple def let evaluates to the correct result (use second def)") {
        execTest ("""
            |let
            |  x = 1;
            |  y = 2
            |in y
            """.stripMargin,
            "2")
    }

    test ("a multiple def let evaluates to the correct result (use both defs)") {
        execTest ("""
            |let
            |  x = 1;
            |  y = 2
            |in x + y
            """.stripMargin,
            "3")
    }

	/**
	 * My Tests.
	 */
	
	test ("a subtraction expression evaluates to the correct result") {
        execTest ("""
            |3 - 4
            """.stripMargin,
            "-1")
    }
	
	test ("a multiplication expression evaluates to the correct result") {
        execTest ("""
            |3 * 4
            """.stripMargin,
            "12")			
    }
	
	test ("a divison expression evaluates to the correct result") {
        execTest ("""
            |8 / 4
            """.stripMargin,
            "2")
    }
	
	test ("a less than expression evaluates to the correct result") {
        execTest ("""
            |2 < 4
            """.stripMargin,
            "true")
        execTest ("""
            |2 < 1
            """.stripMargin,
            "false")
    }
	
	test ("an equality expression evaluates to the correct result") {
        execTest ("""
            |2 == 3
            """.stripMargin,
            "false")
			
		execTest ("""
            |2 == 2
            """.stripMargin,
            "true")
    }
	
	test ("parenthesis allow us to alter precedence of operators") {
		execTest ("""
			|1 + 2 * 3
			""".stripMargin,
			"7")
		execTest ("""
			|(1 + 2) * 3
			""".stripMargin,
			"9")
	}
	
	test ("multiple binary operators evaluate correctly") {
		execTest ("""
			|(1 + 3 - 1 * 4) == 0
			""".stripMargin,
			"true")
	}
	
	test ("evaluation of binary operators is in the correct order") {
		execTest ("""
			|1 / 2 == 2 / 1
			""".stripMargin,
			"false")
		execTest ("""
			|1 / 2 < 2 / 1
			""".stripMargin,
			"true")
		execTest ("""
			|1 + 2 * 3
			""".stripMargin,
			"7")
		execTest ("""
			|2 * 3 - 1
			""".stripMargin,
			"5")
	}
	
	test ("a lambda expression evaluates correctly") {
		execTest ("""
			|\ a :: Int -> a + 1
			""".stripMargin,
			"function of a")
	}
	
	test ("multiple lambda expressions evaluate correctly") {
		execTest ("""
			|\ a :: Int -> \ b :: Int -> b + 1
			""".stripMargin,
			"function of a")
	}
	
	test ("we can pass include both variables in the output expression") {
		execTest ("""
			|(\ a :: Int -> (\ b :: Int -> a + b) 2) 3
			""".stripMargin,
			"5")
	}
	
	test ("we can pass an argument to a lambda expression, and it evaluates correctly") {
		execTest ("""
			|(\ x :: Int -> x + 1) 2
			""".stripMargin,
			"3")
	}
	
	test ("we can use conditionals in a lambda expression") {
		execTest ("""
			|(\ x :: Int -> if (x == 0) then false else true) 1
			""".stripMargin,
			"true")
		execTest ("""
			|(\ x :: Int -> if (x == 0) then false else true) 0
			""".stripMargin,
			"false")
	}
	
	test ("a single definition evaluates as if it were an anonymous function") {
		execTest ("""
			|let
			|  x = 1
			|in
			|  x + 1 == ((\ a :: Int -> a + 1) 1)
			""".stripMargin,
			"true")
	}
	
	test ("variables in a let are case-sensitive") {
		execTest ("""
			|let
			|  x = 10;
			|  X = 5
			|in
			|  x == X
			""".stripMargin,
			"false")
		execTest ("""
			|let
			|  x = 10;
			|  X = 5
			|in
			|  x
			""".stripMargin,
			"10")
		execTest ("""
			|let
			|  x = 10;
			|  X = 5
			|in
			|  X
			""".stripMargin,
			"5")
	}

    test ("multiple definitions in a let evaluate as if they were nested lets") {
		execTest ("""
		   |(let
		   |  x = 1;
		   |  y = 2;
		   |  z = 3
		   |in
		   |  x + y + z) 
		   |==
		   |(let
		   |   x = 1
		   | in
		   |   let
		   |  y = 2
		   |   in
		   |     let
		   |       z = 3
		   |     in 
		   |       x + y + z)
		   """.stripMargin,
		   "true")
    }
	
	test ("a nested let evaluates correctly") {
		execTest ("""
			|let
			|  x = 1
			|in
			|  let
			|    y = 2
			|  in y
			""".stripMargin,
			"2")
	}
	
	test ("if a variable has multiple occurrences in a let only the most recent occurrence matters") {
		execTest ("""
			|let
			|  x = 100
			|in
			|  let
			|    x = 101
			|  in
			|    x
			""".stripMargin,
			"101")
		execTest ("""
			|let
			|  x = 100
			|in
			|  let
			|    x = 101;
			|    inc = \ x :: Int -> x + 1
			|  in
			|    x
			""".stripMargin,
			"101")
	}
	
	test ("a nested let can use the defns defined in the previous let") {
		execTest ("""
			|let
			|  x = 1
			|in
			|  let
			|    y = x + 1
			|  in
			|    let
			|      z = y + 1
			|    in
			|      z 
			""".stripMargin,
			"3")
	}
	
	test ("a lambda expression in a let evaluates to the correct result") {
		execTest ("""
			|let
			|  dec = \ a :: Int -> a - 1
			|in
			|  dec 2
			""".stripMargin,
			"1")
	}
	
	test ("we can pass a definition as an argument for a lambda expression") {
		execTest ("""
			|let
			|  x = 10;
			|  half = \ a :: Int -> a / 2
			|in
			|  half x
			""".stripMargin,
			"5")
	}
	
	test ("we can do the above for boolean arguments too") {
		execTest ("""
			|let
			|  x = false;
			|  convert = \ a :: Bool -> 1
			|in
			|  convert x
			""".stripMargin,
			"1")
	}
	
	test ("we can use some lambda expressions multiple times in a let depending on type mapping") {
		execTest ("""
			|let
			|  x = 10;
			|  inc = \ a :: Int -> a + 1;
			|  dec = \ b :: Int -> b - 1
			|in
			|  inc dec dec x
			""".stripMargin,
			"9")
	}
	
	test ("we can apply lambda exps that map to another lambda exp in a let") {
		execTest ("""
			|let
			|  x = 10;
			|  y = 3;
			|  halfInc = ( \ a :: Int -> ( \ b :: Int -> a / 2 + b) 3)
			|in
			| halfInc x
			""".stripMargin,
			"8")
	}
	
	test ("we can use nested lambda expressions in a let") {
		execTest ("""
			|let 
			|  x = 5;
			|  mul = \ a :: Int -> a * a
			|in
			|  let
			|    op = \ b :: Int -> mul b
			|  in
			|    op x
			""".stripMargin,
			"25")
	}
	
	test ("we can use conditionals in the 'let' part of our let expression") {
		execTest ("""
			|let
			| x = 4
			|in
			|  let
			|    y = if (x < 5) then 2 else 1
			|  in
			|    y
			""".stripMargin,
			"2")
	}
	
	test ("we can also use conditionals in the 'in' part of our let expression") {
		execTest ("""
			|let
			|  x = 2
			|in
			|  if (x + 1 == 3) then true else false
			""".stripMargin,
			"true")
	}
	
	test ("a let expression can contain many definitions") {
		execTest ("""
			|let
			|  x = 1;
			|  y = 2;
			|  z = 3;
			|  a = 4;
			|  b = 5;
			|  c = 6;
			|  d = 7;
			|  w = false
			|in
			|  if (x - y - z - a - b - c - d < 0) then w else true
			""".stripMargin,
			"false")
	}
	
	test ("'1 + 2 * 3 / 1' will not parse correctly due to binary associativity, but this rearrangement will") {
		execTest ("""
			|2 * 3 / 1 + 1
			""".stripMargin,
			"7")
	}
}

