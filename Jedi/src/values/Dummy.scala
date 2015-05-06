package values

  class Dummy(val myType: Type) extends Value {
      override def getType():Type = { return myType }
      override def toString() = "dummy"
   }