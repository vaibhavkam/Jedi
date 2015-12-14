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
 * Class to represent conditional expression in Jedi
 */
case class Conditional(conditon: Expression, consequent: Expression, alternate: Expression = null) extends SpecialForm {

  /**
   * Function to execute expression. Expression execution results into value
   * @param env
   */
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
          throw new TypeException("Type exception")
      } 
    }

  /**
   * Function to get type of expression
   * @param env
   */
  def getType(env: Environment):Type ={
        
    //Applying rule of type inference
    if(conditon.getType(env)==Type.BOOLEAN){
      
      if(alternate!=null){
          if(consequent.getType(env)==alternate.getType(env))
            consequent.getType(env)
          else
            Type.ERROR
      }else{
         consequent.getType(env)
      }
    }
    else
      Type.ERROR    
  }
}