package edu.colorado.csci3155.project1


class RuntimeError(msg: String) extends Exception {
    override def toString(): String = {
        s"Error: $msg"
    }
}

class InterpreterError(msg: String) extends Exception{
    override def toString(): String = {
        s"Error: $msg"
    }
}


object Interpreter {



    type Environment = Map[String, Value]

    /*--
        TODO: Complete the evalExpr Function below.
        Please write refactored code and use ValueOps.plus, ValueOps.minus,..
        defined in Value.scala for evaluating the expressions.

        If you encounter error, you should throw a RuntimeError exception defined above.
        Please do not use other exception types.
     */

    def evalBinOp(e1: Expr, e2: Expr, env: Environment, fun: (Value, Value) => Value): Value = {
       fun((evalExpr(e1, env)), (evalExpr(e2,env)))
    }
    def evalExpr(e: Expr, env: Environment) : Value = e match{
        case Const(d) => NumValue(d)
        case Ident(x) => {
            if (env contains x) {
                env(x)
            } else {
               throw new InterpreterError(msg = "Unknown identifier: $x")
            }
        }
        case Plus(e1,e2) => {
            //val d1 = evalExpr(e1,env)
            //val d2 = evalExpr(e2,env)
            evalBinOp(e1,e2,env,ValueOps.plus)
        }
        case Minus(e1,e2) => {
            evalBinOp(e1,e2,env,ValueOps.minus)
        }
        case Mult(e1,e2) => {
            evalBinOp(e1,e2,env,ValueOps.mult)
        }
        case Div(e1,e2) => {
            evalBinOp(e1,e2,env,ValueOps.div)
        }
        case Geq(e1,e2) => evalBinOp(e1,e2,env, ValueOps.geq)
        case Gt(e1,e2) => evalBinOp(e1,e2,env, ValueOps.gt)
        case Eq(e1,e2) => evalBinOp(e1,e2,env, ValueOps.eq)
        case And(e1,e2) => {
            val v1 = evalExpr(e1,env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2,env)
                    if(ValueOps.isBoolean(v2)){
                    v2
                    } else {
                       throw new InterpreterError(msg= "Argument to and (&&) operator does not evaluate to boolean")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new InterpreterError( msg = "Argument to and (&&) operator does not evaluate to boolean")
            }
        }

        case Or(e1,e2) => {
            val v1 = evalExpr(e1,env)
            v1 match {
            case BoolValue(false) => {
                val v2 = evalExpr(e2, env)
                if (ValueOps.isBoolean(v2)){
                v2
                } else {
                    throw new InterpreterError(msg = "Argument to or (||) operator does not evaluate to boolean")
                }
            }
        case BoolValue(true) => BoolValue(true)
        case _ => throw new InterpreterError(msg = "Argument to or (||) operator does not evaluate to boolean")

        }
    }
        case Not(e1) => {
            val v1 = evalExpr(e1,env)
            v1 match {
                case BoolValue(false) => BoolValue(true)
                case BoolValue(true) => BoolValue(false)
                case _ => throw new InterpreterError(msg ="Argument to not (!)operator does not evaluate to boolean")
            }
        }
        case IfThenElse(cond, thExpr, elExpr) => {
            val v1 = evalExpr(cond, env)
            v1 match{
                case BoolValue(true) => evalExpr(thExpr,env)
                case BoolValue(false) => evalExpr(elExpr,env)
                case _ => throw new InterpreterError(msg = "Argument to if then else condition does not evaluate to boolean")
            }
        }

    }

    /*--
    TODO: Implement a function evalVarDefine that given a identifier x,
    expression e and environment env,
       a) evaluates e under env: let result be v
       b) yields new environment that updates env with {x -> v }
     For your convenience the RuntimeError exception has been handled for you.
     */
    def evalVarDefine(x: String, e: Expr, env: Environment): Environment = {
        try {
            val v: Value = evalExpr(e,env)
            env + (x -> v)
        } catch {
            case e: InterpreterError => {
                env
            }
            case _:RuntimeError =>  {
                env
            }
        }
    }

    /*-- TODO: Complete the evalCommand Function Below --*/
    // Function evalCommand
    // Evaluate a command under an environment.
    //  Returns the new environment as a result of executing the command.
    //  If the command is of the form Define(x, e), the environment is updated by evaluating
    //  e under the "old" environment and updating the old environment to now bind x to the result.
    // If the command is of the form Display(e), the environment returned is just the
    // same as the environment that was passed in as an argument.
    //
    def evalCommand( env: Environment, cmd: Cmd): Environment = cmd match {
        case Define(x,e) => evalVarDefine(x,e,env)
        case Display(e) => {
            val v = evalExpr(e,env)
            println("Displaying results: " + ValueOps.value2String(v))
            env
        }
    }

    /*-- TODO: Implement evalProgram function below.
       Careful: Do not use for/while loops. Instead you should be using
       pattern matching on `prog` and then using lst foldLeft function.
       A tail recursive solution is also acceptable but please try to use pattern matching.
     */
    def evalProgram(prog: CalcProgram, env0: Environment = Map.empty): Environment = prog match {
        case TopLevel(defs) => {
            defs.foldLeft[Environment] (env0) (evalCommand _)
        }
    }
}
