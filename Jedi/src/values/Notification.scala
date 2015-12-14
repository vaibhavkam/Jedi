package values

class Notification(msg: String) extends Value{

    this.typ = Type.NOTIFICATION;

    override def toString(): String ={
      this.msg
    }
    
    override def getType():Type ={
       this.typ
    }

}

object Notification{
  
    def apply(msg: String): Notification = {
      
      new Notification(msg)
    }
    
    def VARIABLE_UPDATED() = apply("variable is updated")
    def BINDING_CREATED() = apply("binding is created")
    def UNKNOWN() = apply("unknown")
    def UNSPECIFIED() = apply("unspecified")
    def ERROR() = apply("error")
    def OK() = apply("OK")
    def DONE() = apply("done")

}