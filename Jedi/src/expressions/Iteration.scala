package expressions

import values._
import ui._

/**
 * @author Vaibhav
 * Class to represent 'while' expression in Jedi
 */
case class Iteration(operands: List[Expression]) extends SpecialForm {
	
  /**
   * Function to execute expression. Expression execution results into value
   * @param env
   */  
  def execute(env: Environment): Value = {
	  if (operands.length != 2) throw new TypeException("iteration expects exactly 2 operands")
	  
	  val operand0 = operands(0).execute(env)
	  val ok = operand0.isInstanceOf[Boole]
	  if (!ok) throw new TypeException("first iteration operand must evaluate as boole")
	  
	  while (operands(0).execute(env).asInstanceOf[Boole].value == true) {
	    operands(1).execute(env)
	  }  
	  Notification.DONE
	}
  
  /**
   * Function to get type of expression
   * @param env
   */  
  def getType(env: Environment):Type ={
    
    //Applying rule of type inference
    if(operands(0).getType(env)==Type.BOOLEAN)
      operands(1).getType(env)
    else
      Type.ERROR
  }
}