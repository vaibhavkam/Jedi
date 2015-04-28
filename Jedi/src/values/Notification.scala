package values

class Notification(msg: String) extends Value{

    override def toString(): String ={
      this.msg
    }
}

object Notification{
  
    def apply(msg: String): Notification = {
      
      new Notification(msg)
    }
    
    def VARIABLE_UPDATED() = apply("variable is updated")
    def BINDING_CREATED() = apply("binding is created")
    def UNKNOWN() = apply("unknown")
    def ERROR() = apply("error")
    def OK() = apply("OK")
    def DONE() = apply("done")

}