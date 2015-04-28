/**
 *
 */
package expressions

import values.Value
import values.Environment
import ui.UndefinedException
import values.Type

/**
 * @author Vaibhav
 *
 */
case class Literal() extends Expression with Value with Serializable {

    this.typ = Type.BOOLE;

    def execute(env: Environment): Value = this    
    
    def getType(env: Environment):Type ={
      this.typ
    }
}