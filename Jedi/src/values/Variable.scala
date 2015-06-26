package values

class Variable(val content: Value) extends Value {
	
  this.typ = Type.VARIABLE;

  override def toString(): String = {
	  "Variable(" + content.toString + ")"
	}
  
  override def getType():Type ={
     if(content.getType()!=Type.ERROR)
       new VarType(content.getType())
     else
       Type.ERROR
  }

}