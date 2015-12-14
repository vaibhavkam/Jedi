package expressions

import values.Type
import values.Environment
import values.Value

/**
 * @author vkamble
 */
case class Cast(exp: Expression, typ: Expression) extends SpecialForm {
  
    def execute(env: Environment): Value = {
       exp.execute(env);
    }

  /**
   * Function to get type of expression
   * @param env
   */
  def getType(env: Environment):Type ={
    
       val t = typ.getType(env);
       if(t!=Type.TYPE)
         Type.ERROR
       typ.execute(env).asInstanceOf[Type]
 }
}