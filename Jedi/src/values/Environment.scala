/**
 *
 */
package values

import scala.collection.mutable.HashMap
import expressions.Identifier

/**
 * @author Vaibhav
 *
 */
class Environment(val nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {

    def put(names: List[Identifier], vals: List[Value]){
      
        for((i,v)<-(names zip vals)){
          this.put(i,v)

        }
    }
    
    def find(id: Identifier): Value ={

      if(this.contains(id)){

        this.get(id).get

      }
      else if(nextEnv!=null){
        nextEnv.find(id)
      }
      else{
        Notification.UNKNOWN
      }
    }
    
    def next(): Environment = {
       nextEnv
    }
    
     
    //Returns both the value found, and the environment in which it was found, to aid with assignment
    def findWithEnv(id: Identifier): (Value, Environment) = {
      if (this.contains(id)) {
        (get(id).get, this)
      }
      else if (nextEnv != null) {
        nextEnv.findWithEnv(id)
      }
      else {
        (Notification.UNKNOWN, this)
      }
    }
    
    

}