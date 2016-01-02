package haslite

import org.kiama.attribution.Attribution

object SemanticAnalysis {

    import HasliteTree._
    import org.kiama.==>
    import org.kiama.attribution.Attribution.attr
    import org.kiama.attribution.Decorators.{chain, Chain}
    import org.kiama.rewriting.Rewriter.{collect, collectall}
    import org.kiama.util.{Entity, MultipleEntity, UnknownEntity}
    import org.kiama.util.Messaging.{check, message}

    /**
     * Useful method to pretty-print types for error messages.
     */
    def prettyType (tipe : Type) : String =
        tipe match {
            case BoolType ()        => "Bool"
            case IntType ()         => "Int"
            case FunType (from, to) => s"${prettyType (from)} -> ${prettyType (to)}"
            case UnknownType (s)    => "type error: "+ s
            case RedeclaredType (i) => s"!!${i}!!"
        }

    /**
     * Collect the semantic error messages for a given tree.
     */
    val errors =
        attr (collectall {
            case e : Exp =>
                (tipe (e) match {
                    case RedeclaredType (i) => message (e, s"declared more than once")
                    case UnknownType (s) => message (e, s)
                    case _ => Nil
                    })
        })

    
    /**
     * The environment containing all bindings visible "before" a
     * particular node in the tree.  I.e., it's the environment in which that
     * node is evaluated.
     */
    val env : HasliteNode => Map[Identifier, Type] =
        attr {
            case p : Program    => Map[Identifier, Type]()
            case n : Defn       => env (n.parent[HasliteNode])
            case n              => n.parent match {
                case p @ LamExp (IdnDef (i, t),e) => env (p) +  (i -> t)
                case p @ LetExp (defns, e)        => env (p) ++ defedin (p)
                case _                            => env (n.parent[HasliteNode])
            }
        }

    val defedin : HasliteNode => Map[Identifier, Type] = 
        attr {
            case LetExp (defns, exp) => {val defs = defns.collect {case Defn (i, e) => (i -> tipe (e))}
                                         var map = Map[Identifier, Type]()
                                         defs.foreach {case (i,t) => 
                                            map.get(i) match {
                                                case Some (t) => map = map + (i -> RedeclaredType(i))
                                                case None     => map = map + (i -> t)
                                            }
                                         }
                                         map
                                        } 
            case _                   => Map[Identifier, Type]()
        }

    /**
     * What is the type of an expression?
     */
    val tipe : Exp => Type =
        attr {

            case AppExp (fn, e) if ! (e eq fn) =>
                tipe (fn) match {
                     case FunType (from, to) => if (tipe (e) == from) {to} 
                                                else                  {UnknownType (s"expected ${prettyType (from)} got ${prettyType (tipe (e))}")}
                     case t                  => UnknownType (s"application of non-function ${prettyType (t)}")
                }
            
            case LamExp (IdnDef (i,t), body) => 
                FunType (t, tipe (body))
            
            case LetExp (_, e) =>
                tipe (e)

            case BoolExp (_) =>
                BoolType ()

            case EqualExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => BoolType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case n @ IdnUse (i) =>
                env (n).get (i) match {
                    case None     => UnknownType ("variable not defined")
                    case Some (t) => t
                }

            case IfExp (c, e1, e2) => (tipe (c), tipe (e1), tipe (e2)) match {
                case (BoolType (), t1, t2) if t1 == t2 => t1
                case (BoolType (), t1 ,t2) => UnknownType (s"expected ${prettyType (t1)} got ${prettyType (t2)}")
                case (other      , _  ,_ ) => UnknownType (s"expected Bool got ${prettyType (other)}")
            }

            case IntExp (_) =>
                IntType ()

            case LessExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => BoolType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case MinusExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case PlusExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case SlashExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case StarExp (a, b) => (tipe (a), tipe (b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

        }


}
