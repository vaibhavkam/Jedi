/**
 *
 */
package expressions

import values.Environment
import values.Value
import values.Notification
import ui.UndefinedException
import values.Type
import values.Dummy

/**
 * @author Vaibhav
 *
 */
case class Identifier(name: String,var typ: Type =null) extends Expression with Serializable {

  def execute(env: Environment): Value = {
    val vals = env.find(this)
    vals
  }
  
  def getType(env: Environment):Type ={
    val vals = env.find(this)
    vals.getType()
  }
}