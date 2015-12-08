package values

import expressions._
import ui._

/**
 * @author Vaibhav
 */
class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
    
  this.typ = Type.CLOSURE;

  def apply(args: List[Value]): Value = {
    
    val localEnv = new Environment(defEnv)
    
    if (args.length != params.length) {
      
      throw new TypeException("closure argument list " + args + " must be same length as parameter list " + params)
    }
    else {
      
      localEnv.put(params, args)
      body.execute(localEnv)
    }
  }
  
  override def getType():Type ={
      this.typ
  }
 }