package expressions

import values._

/**
 * @author Vaibhav
 *
 */
case class Declaration(ident: Identifier, exp: Expression) extends SpecialForm {
	
	def execute(env: Environment): Value = {
    
			env.put(ident, exp.execute(env))
		  Notification.OK 
	}
  
  def getType(env: Environment):Type ={
    Type.NUMBER
  }
}