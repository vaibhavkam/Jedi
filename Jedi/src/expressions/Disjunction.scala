/**
 *
 */
package expressions

import ui.TypeException
import values.Boole
import values.Value
import values.Environment
import values.Type

/**
 * @author Vaibhav
 *
 */
case class Disjunction(operands: List[Expression]) extends SpecialForm{

   def execute(env: Environment): Value = {
      var i = 0
      var result = false
     
      while (!result && i < operands.length) {
        var next = operands(i).execute(env)
        if (!next.isInstanceOf[Boole]) throw new TypeException("error5")
        var res = next.asInstanceOf[Boole]
        result = res.value
        i = i + 1 
      }
      new Boole(result)   
    }
      
    def getType(env: Environment):Type ={
      val args: List[Type] = operands.map(_.getType(env)) 
      val ok = args.filter(_ ==Type.BOOLE)
      if (ok.length == args.length)
        Type.BOOLE
      else
        Type.ERROR
    }
}