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
            case UnknownType (s)    => "type error: " + s
        }

    /**
     * Collect the semantic error messages for a given tree.
     */
    val errors =
        attr (collectall {
            case e : Exp =>
                (tipe (e) match {
                    case UnknownType (s) => message (e, s)
                    case _ => Nil
                    })
        })
    
    /**
     * The environment containing all bindings visible "before" a
     * particular node in the tree.  I.e., it's the environment at the
     * node plus any new bindings introduced by the node.
	 *
	 * Instead of working out the Type explicitly, just give the Expression.
	 * And the Type can be worked out at a later point through other means. 
     */
    val env : HasliteNode => Map[Identifier, Exp] =
        attr {
            case p : Program => Map[Identifier, Exp]()
            case n           => n.parent match {
			    // So lambda expression cases can work correctly.
                case p @ LamExp (IdnDef (i, t), _)  => if (t == IntType())
													       env(p) + (i -> IntExp(-1))
													   else if (t == BoolType())
														   env(p) + (i -> BoolExp(false))
													   else
													       env(p) + (i -> UnknownExp("invalid type"))
                case p @ Defn (i, e) => env (p) + (i -> e)
                case p @ LetExp (defns, _)          => env (p) ++ defedin (p)
                case _                              => env (n.parent[HasliteNode])
            }
        }

    val defedin : HasliteNode => Map[Identifier, Exp] = 
        attr {
            case LetExp (defns, _) => {val defs = defns.collect {case Defn (i, e) => (i -> e)}
                                         var map = Map[Identifier, Exp]()
                                         defs.foreach {case (i, e) => 
                                            map.get (i) match {
											    case None     => map = map + (i -> e)
											    // Work around for not mapping to Types anymore.
                                                case Some (e) => map = map + (i -> UnknownExp ("declared more than once")) 
                                            }
                                         }
                                         map
                                        }	 
            case _                   => Map[Identifier, Exp]()
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
            
            case LamExp (IdnDef (i, t), body) => 
                FunType (t, tipe (body))
            
            case LetExp (_, e) =>
                tipe (e)

            case BoolExp (_) =>
                BoolType ()

            case EqualExp (a, b) => (tipe(a), tipe(b)) match {
                case (IntType (), IntType ()) => BoolType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case n @ IdnUse (i) =>
                env (n).get (i) match {
                    case None => UnknownType ("variable not defined")
					case Some (a @ IdnUse (k)) => if (env (a).get (k) == Some (n)) // Covers Defn(n, n) and Defn(a, b); Defn(b, a).
												      UnknownType ("variable type not determinable")
												  else // Else if the type of it is in some other Defn. 
													  tipe (a)
													 
					// Catch all other cases. 
					case Some (e) => tipe (e)
                }
			
            case IfExp (c, e1, e2) => (tipe (c), tipe (e1), tipe (e2)) match {
                case (BoolType (), t1, t2) if t1 == t2 => t1
                case (BoolType (), t1 ,t2) => UnknownType (s"expected ${prettyType (t1)} got ${prettyType (t2)}")
                case (other      , _  ,_ ) => UnknownType (s"expected Bool got ${prettyType (other)}")
            }

            case IntExp (_) =>
                IntType ()
				
			case UnknownExp (s) => 
				UnknownType (s)
				
            case LessExp (a, b) => (tipe(a), tipe(b)) match {
                case (IntType (), IntType ()) => BoolType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case MinusExp (a, b) => (tipe(a), tipe(b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }
			
			case PlusExp (a, b) => (tipe(a), tipe(b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

			/*
			// THIS WHOLE THING DOES NOT WORK IN CERTAIN CASES
			// E.g, let f = g + 1; g = true in f returns Int
			case PlusExp (a, b) => (a, b) match {
			    case (BoolExp (_), _) => UnknownType ("expected Int got Bool")
				case (_, BoolExp (_)) => UnknownType ("expected Int got Bool")
				// For cases such as a = a + 1, a = 1 + a + 1.
                case (IntExp (_), IntExp (_)) => IntType ()
				case (IdnUse (_), IntExp (_)) => IntType ()
				case (IntExp (_), IdnUse(_)) => IntType () 
                case (IntExp (_), other) => tipe (other)
				// YES THIS CAUSES ISSUES
				case (other1, other2) => tipe (other1)
				/*
				case (other1, other2) => if (tipe (other1) != tipe (other2)) // CONSIDER DOES THIS CASE CAUSE ISSUES? 
										     UnknownType ("Type mismatch")
										 else
											 tipe (other1)
				*/
			}
			*/
			
            case SlashExp (a, b) => (tipe(a), tipe(b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

            case StarExp (a, b) => (tipe(a), tipe(b)) match {
                case (IntType (), IntType ()) => IntType ()
                case (IntType (), other     ) => UnknownType (s"expected Int got ${prettyType (other)}")
                case (other     , _         ) => UnknownType (s"expected Int got ${prettyType (other)}")
            }

        }

}
