/**
 *
 */
package expressions

import ui.TypeException
import values.Boole
import values.Environment
import values.Value
import values.Type

/**
 * @author Vaibhav
 *
 */
case class Conjunction(operands:List[Expression]) extends SpecialForm{

    def execute(env: Environment): Value = {
      var index = 0
      var result = true

      while(result && index < operands.length){
        var next = operands(index).execute(env)
        if(!next.isInstanceOf[Boole]) throw new TypeException("error3")
        var res = next.asInstanceOf[Boole]
        result = res.value 
        index = index + 1
      }
      new Boole(result)   
    }
    

    def getType(env: Environment):Type ={
      val args: List[Type] = operands.map(_.getType(env)) 
      val ok = args.filter(_ ==Type.BOOLEAN)
      if (ok.length == args.length)
        Type.BOOLEAN
      else
        Type.ERROR
    }
}