package expressions

import values._
import ui._

case class Iteration(operands: List[Expression]) extends SpecialForm {
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
  
  def getType(env: Environment):Type ={
    Type.NUMBER
  }
}