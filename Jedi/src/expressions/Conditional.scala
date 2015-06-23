/**
 *
 */
package expressions

import values.Environment
import values.Value
import ui.TypeException
import values.Boole
import values.Type

/**
 * @author Vaibhav
 *
 */
case class Conditional(conditon: Expression, consequent: Expression, alternate: Expression = null) extends SpecialForm {

    def execute(env: Environment): Value = {
     
      var check = conditon.execute(env)
      if (!check.isInstanceOf[Boole]){
        throw new TypeException("Condition needs to be boolean type")
      } 
      
      var result = check.asInstanceOf[Boole]
      
      if (result.value){
        consequent.execute(env)
      } 
      else{
      
        if (alternate != null)
          alternate.execute(env)
        else 
          throw new TypeException("Null")
      } 
    }
    
  def getType(env: Environment):Type ={
        
    if(conditon.getType(env)==Type.BOOLE){
      
      if(alternate!=null && (consequent.getType(env)==alternate.getType(env)))
        consequent.getType(env)
      else
        Type.ERROR
    }
    else
      Type.ERROR    
  }
}