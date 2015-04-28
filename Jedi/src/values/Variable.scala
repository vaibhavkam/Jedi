package values

class Variable(val content: Value) extends Value {
	
  this.typ = Type.VARIABLE;

  override def toString(): String = {
	  "Variable(" + content.toString + ")"
	}
  
}