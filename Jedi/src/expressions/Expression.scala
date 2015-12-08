/**
 *
 */
package expressions

import values.Environment
import values.Value
import values.Type

/**
 * @author Vaibhav
 * Trait to represent expression in Jedi
 */
trait Expression {
  
  /**
   * Function to execute expression. Expression execution results into value
   * @param env
   */
  def execute(env: Environment): Value

  /**
   * Function to get type of expression
   * @param env
   */
  def getType(env: Environment): Type
    
}