package haslite

import org.kiama.util.PositionedParserUtilities

/**
 * Module containing parsers for Haslite.
 */
class SyntaxAnalysis extends PositionedParserUtilities {

    import HasliteTree._
    import scala.collection.immutable.Seq
    import scala.language.postfixOps

    lazy val program : PackratParser[Program] =
        exp ^^ Program

    lazy val exp : PackratParser[Exp] =
        ("if" ~> "(" ~> exp <~ ")") ~ ("then" ~> exp) ~ ("else" ~> exp) ^^ {
            case c ~ t ~ e => IfExp (c, t, e)
        } |
        exp1

    lazy val exp1 : PackratParser[Exp] =
        exp2 ~ ("==" ~> exp2) ^^ { case e ~ t => EqualExp (e, t) } |
        exp2 ~ ("<" ~> exp2) ^^ { case e ~ t => LessExp (e, t) } |
        exp2

    lazy val exp2 : PackratParser[Exp] =
        exp2 ~ ("+" ~> exp3) ^^ { case e ~ t => PlusExp (e, t) } |
        exp2 ~ ("-" ~> exp3) ^^ { case e ~ t => MinusExp (e, t) } |
        exp3

    lazy val exp3 : PackratParser[Exp] =
        exp3 ~ ("*" ~> factor) ^^ { case e ~ t => StarExp (e, t) } |
        exp3 ~ ("/" ~> factor) ^^ { case e ~ t => SlashExp (e, t) } |
        factor

    lazy val factor : PackratParser[Exp] =
        app |
        let |
        lam |
        "false" ^^ (_ => BoolExp (false)) |
        "true" ^^ (_ => BoolExp (true)) |
        identifier ^^ {case e => IdnUse (e)} |
        integer ^^ (s => IntExp (s.toInt)) |
        "(" ~> exp <~ ")" |
        failure ("exp expected")

    lazy val tipe : PackratParser[Type] =
        basictipe ~ ("->" ~> tipe) ^^ {
            case l ~ r => FunType (l, r)
        } |
        basictipe

    lazy val basictipe : PackratParser[Type] =
        "Bool" ^^ (_ => BoolType ()) |
        "Int" ^^ (_ => IntType ()) |
        "(" ~> tipe <~ ")"
    
    lazy val app : PackratParser[AppExp] =
        exp ~ exp ^^ {
            case f ~ arg => AppExp (f, arg)
        }
    lazy val lam : PackratParser[LamExp] = {
        ("\\" ~> idndef) ~ ("->" ~> exp) ^^ {case idn ~ body => LamExp (idn, body)}
    }
    lazy val let : PackratParser[LetExp] =
        ("let" ~> definitions <~ "in") ~ exp ^^ {
            case ds ~ e => LetExp (ds, e)
        }

    lazy val definitions : PackratParser[List[Defn]] =
        rep1sep (defn, ";")

    lazy val defn : PackratParser[Defn] =
        identifier ~ ("=" ~> exp) ^^ {
            case i ~ e => Defn (i, e)
        }

    // NOTE: You should not need to change anything below here...

    lazy val integer =
        regex ("[0-9]+".r)

    lazy val idndef =
        (identifier <~ "::") ~ tipe ^^ {case i ~ t => IdnDef (i,t)}

    val keywordStrings =
        Seq ("let", "else", "false", "if", "then", "true", "in")

    lazy val keyword =
        keywords ("[^a-zA-Z0-9_]".r, keywordStrings)

    lazy val identifier =
        not (keyword) ~> identifierBase

    lazy val identifierBase =
        regex ("[a-zA-Z][a-zA-Z0-9_]*".r) |
        failure ("identifier expected")

    lazy val whitespaceParser : PackratParser[Any] =
        rep (whiteSpace | comment)

    lazy val comment : PackratParser[Any] =
        "{-" ~ rep (not ("*/") ~ (comment | any)) ~ "-}" |
        "--.*(\n|\\z)".r

}
