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
import values.FunType
import values.TupleType

/**
 * @author Vaibhav
 *
 */
case class FunCall(operator: Expression, operands: List[Expression] = Nil) extends Expression {

  def execute(env: Environment): Value = {    
      if(operator.isInstanceOf[Identifier] && operator.asInstanceOf[Identifier].name.equalsIgnoreCase("typeOf")){
        val types: List[Type] = operands.map(_.getType(env))  
        return types.head
      }
        
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
  
  def getType(env: Environment):Type ={
           
        val closure = operator.execute(env)
        if(closure.isInstanceOf[Closure]){
          val funType = closure.getType().asInstanceOf[FunType]
          val tupleType = funType.getDomain().asInstanceOf[TupleType]
          
          if(tupleType.getComponent().size==operands.size){
            
            for((m,n) <- tupleType.getComponent().zip(operands.map(_.getType(env)))){
              
              if(!m.toString().equals(n.toString()) && !n.subType(m)){
                return Type.ERROR            
              }
            }
            funType.getRange()            
          }
          else{
            Type.ERROR            
          }
        }
        else{
        var t = system.getType(operator.asInstanceOf[Identifier],operands, env)
        t
        }
  }
}