package expressions

import values.Boole
import ui.TypeException
import values.Notification
import values.Environment
import values.Value
import values.Type

/**
 * @author vkamble
 */
case class Iteration2(condition: Expression, body: Expression, testAtTop: Boolean = true) extends SpecialForm  {
  
   def execute(env: Environment) = {
    var result: Value = if (testAtTop) Notification.UNSPECIFIED else body.execute(env)
    val c = condition.execute(env)
    if (!c.isInstanceOf[Boole]) throw new TypeException("while condition must be a Boole")
    var c2 = c.asInstanceOf[Boole]
    while(c2.value) {
      result = body.execute(env)
      c2 = condition.execute(env).asInstanceOf[Boole]
    }
    result
  }
   
    def getType(env: Environment):Type ={
    
    //Applying rule of type inference
    if(condition.getType(env)==Type.BOOLEAN)
      body.getType(env)
    else
      Type.ERROR
  } 
}