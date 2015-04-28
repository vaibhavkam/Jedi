/**
 *
 */
package expressions

import values.Environment
import values.Value
import values.Type

/**
 * @author Vaibhav
 *
 */
trait Expression {
  
    def execute(env: Environment): Value
    
    def getType(env: Environment): Type
    
}