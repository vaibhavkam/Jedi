/**
 *
 */
package expressions

import values.Environment
import values.Value
import ui.system
import values.Closure
import ui.UndefinedException
import values.Type

/**
 * @author Vaibhav
 *
 */
case class FunCall(operator: Expression, operands: List[Expression] = Nil) extends Expression {

  def execute(env: Environment): Value = {    

    if(operator.equals(Identifier("valType"))){

       val args: List[Type] = operands.map(_.getType(env)) 
       args.head
    }
    else{
      
      val args: List[Value] = operands.map(_.execute(env))  
      
      try {
        val opexec = operator.execute(env)
            
        if (opexec.isInstanceOf[Closure]) {
          //It is a closure
          opexec.asInstanceOf[Closure].apply(args)
        }
        else {
          //Not a closure, but didn't throw UndefinedException, so just try system anyway
          system.execute(operator.asInstanceOf[Identifier], args)
        }
      }
      catch {
        case e: UndefinedException => {
          //Operator is an identifier that is undefined, try system...
          system.execute(operator.asInstanceOf[Identifier], args)
        }
      }
    }
  }
  
  def getType(env: Environment):Type ={
           
        val args: List[Type] = operands.map(_.getType(env))  
        system.getType(operator.asInstanceOf[Identifier],args)
  }
}