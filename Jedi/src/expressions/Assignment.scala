package expressions

import values._
import ui._

case class Assignment(id: Identifier, exp: Expression) extends SpecialForm {

  def execute(env: Environment): Value = {
	
    val (value, valueEnv) = env.findWithEnv(id)
	  
	  if (value.toString == Notification.UNKNOWN.toString) 
      throw new UndefinedException(id.name)
	  
    if (!value.isInstanceOf[Variable]) 
      throw new TypeException("Assignment cannot assign new value to non-variable")
	  
	  valueEnv.put(id, new Variable(exp.execute(env)))
	  Notification.DONE
	}
  

  def getType(env: Environment):Type ={

    if(exp.getType(env)!=id.getType(env).asInstanceOf[VarType].getContentType()){
      Type.ERROR
    }
    else{
      Type.NOTIFICATION
    }
  }
}