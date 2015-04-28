package expressions

import values._
import expressions._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

case class Lambda(parameters: Map[Identifier,Identifier] , body: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
    
    var paramterList = new ListBuffer[Identifier]()

    for ((k,v) <- parameters) {
      paramterList+=k
    } 
	  new Closure(paramterList.toList, body, env)
	}
  
  def getType(env: Environment):Type = {

      val localEnv = new Environment(env);
      var paramterList = new ListBuffer[Identifier]()
      var typeList = new ListBuffer[Identifier]()

      for ((k,v) <- parameters) {
        paramterList+=k
        typeList+=v
      } 

      var args: List[Value] = typeList.toList.map(_.execute(localEnv))  

      localEnv.put(paramterList.toList, args)
      
      body.getType(localEnv)
  }

}