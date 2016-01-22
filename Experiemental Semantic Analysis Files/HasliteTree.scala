package haslite

/**
 * Module containing tree structures for representing Moama programs.
 */
object HasliteTree {

    import org.kiama.attribution.Attributable

    /**
     * The common supertype of all source tree nodes.
     */
    sealed abstract class HasliteNode extends Attributable

    /**
     * A Haslite program is an expression.
     */
    case class Program (exp : Exp) extends HasliteNode

    /**
     * Common class for all definitions.
     */
    case class Defn (idndef: Identifier, body: Exp) extends HasliteNode

    /**
     * Base class of all types.
     */
    sealed abstract class Type

    /**
     * Boolean type
     */
    case class BoolType () extends Type

    /**
     * Function type
     */
    case class FunType (from : Type, to : Type) extends Type

    /**
     * Integer type
     */
    case class IntType () extends Type

    /**
     * An unknown type, for example, one belonging to a name that is not declared
     * but is used in an expression.
     */
    case class UnknownType (error: String) extends Type

    /**
     * Common superclass of expressions.
     */
    sealed abstract class Exp extends HasliteNode

    /**
     * Application expression.
     */
    case class AppExp (fn : Exp, arg : Exp) extends Exp

     /**
     * Function argument and body (lambda expression).
     */
    case class LamExp (arg : IdnDef, body : Exp) extends Exp

   /**
     * Let expression.
     */
    case class LetExp (defns : List[Defn], exp : Exp) extends Exp

    /**
     * Boolean-valued expression (True or False).
     */
    case class BoolExp (b : Boolean) extends Exp

    /**
     * Equality expression compares the left and right expressions for equality.
     */
    case class EqualExp (left : Exp, right : Exp) extends Exp

    /**
     * Conditional expression.
     */
    case class IfExp (cond : Exp, thenExp : Exp, elseExp : Exp) extends Exp

    /**
     * Integer-valued numeric expression.
     */
    case class IntExp (n : Int) extends Exp

    /**
     * Less than expression compares the left and right numeric expressions for less-than order.
     */
    case class LessExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the difference between the values of
     * two expressions.
     */
    case class MinusExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the sum of the values of two expressions.
     */
    case class PlusExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the division of the values of two expressions.
     */
    case class SlashExp (left : Exp, right : Exp) extends Exp

    /**
     * An expression whose value is the product of the values of two expressions.
     */
    case class StarExp (left : Exp, right : Exp) extends Exp
	
	/**
	 * An unknown expression that has no value, and with type UnknownType. 
	 */
	case class UnknownExp (error: String) extends Exp

    /**
     * An identifier reference.
     */
    sealed trait Idn extends HasliteNode {
        def idn : Identifier
    }

    /**
     * A defining occurrence (def) of an identifier.
     */
    case class IdnDef (idn : Identifier, tipe: Type) extends Idn

    /**
     * An applied occurrence (use) of an identifier.
     */
    case class IdnUse (idn : Identifier) extends Exp with Idn

    /**
     * A representation of identifiers as strings.
     */
    type Identifier = String

}
