package expressions

import values._
import expressions._
import scala.collection.mutable.MutableList
import scala.collection.mutable.ListBuffer

case class Lambda(parameters: Map[Identifier,Expression] , body: Expression) extends SpecialForm {
	def execute(env: Environment): Value = {
    
    var paramterList = new ListBuffer[Identifier]()

    for ((k,v) <- parameters) {
      paramterList+=k
    } 
	  val closure = new Closure(paramterList.toList, body, env)
    closure.typ=this.getType(env);
    closure
	}
  
  def getType(env: Environment):Type = {

      val localEnv = new Environment(env);
      var paramterList = new ListBuffer[Identifier]()
      var typeList = new ListBuffer[Type]()
      var typ:Type=null;
      for ((k,v) <- parameters) {
        var value: Value = v.execute(localEnv);
        if(value.isInstanceOf[Type])
          typ = value.asInstanceOf[Type]
        else
          typ = Type.VOID
                    
        localEnv.put(k, new Dummy(typ))

        typeList+=typ
      } 
      
      var range: Type = body.getType(localEnv)

      if(typeList!=null && !typeList.isEmpty)
        new FunType(new TupleType(typeList.toList),range);
      else
        new FunType(Type.VOID,range);
  }

}