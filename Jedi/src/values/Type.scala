/**
 *
 */
package values

import ui.TypeException

/**
 * @author Vaibhav
 *
 */
class Type(str: String =null) extends Value with Comparable[Type] {

    this.typ = Type.TYPE;

    override def compareTo(obj: Type): Int ={
            
      var result:Int=0;
      
      if(!this.isInstanceOf[FunType] && !obj.isInstanceOf[FunType]){
        result = generalSize(this,obj)
      }
      else if(this.isInstanceOf[TupleType] && obj.isInstanceOf[TupleType]){
        var tupleTyp1= this.asInstanceOf[TupleType]
        var tupleTyp2= obj.asInstanceOf[TupleType]
        

        if(tupleTyp1.getComponent().zip(tupleTyp2.getComponent()).count{case (x,y) => (if(generalSize(x,y)==0)true else false)} == tupleTyp1.getComponent().size)
          result = 0
        else
          result = -1
      }      
      else if(this.isInstanceOf[FunType] && obj.isInstanceOf[FunType]){
        
        var funTyp1= this.asInstanceOf[FunType]
        var funTyp2= obj.asInstanceOf[FunType]

        if(generalSize(funTyp1.getDomain(),funTyp2.getDomain())==0 && generalSize(funTyp1.getRange(),funTyp2.getRange())==0)   
          result = 0
        else
          result = -1 
      }
      else
      {
        result = -1
      }      
      result
    }

    def generalSize(x: Type, y: Type) = (x,y) match {
      
      case (Type.INTEGER,Type.INTEGER) => 0 
      case (Type.INTEGER,Type.NUMBER) => 0 
      case (Type.NUMBER,Type.INTEGER) => 0 
      case (Type.NUMBER, Type.NUMBER) => 0

      case _ => -1
    }
    
    override def toString(): String ={
      this.str
    }
    
    def getValue(typ:Type): Value={
      
      val t = typ.toString() match{
        case "Boole" => new Boole(true)
        case "Number" => new Number(0)
        case "Integer" => new Integer(0)
      }
      
      t
    }
    
    def getDefaultValue(): Value = {
      
      val typ: Value = this match{
        case Type.BOOLE => new Boole(false)
        case Type.NUMBER => new Number(0)
      }
      typ      
    }
}

object Type{
    
    val NUMBER = new Type("Number")
    val LITERAL = new Type("Literal")
    val BOOLE = new Type("Boole")
    val VALUE = new Type("Value")
    val TYPE = new Type("Type")
    val ERROR = new Type("Error")
    val VARIABLE = new Type("Variable")
    val CLOSURE = new Type("Closure")
    val TUPLE = new Type("Tuple")
    val FUNCTION = new Type("Function")
    val INTEGER = new Type("Integer")
    val VOID = new Type("Void")
    val NOTIFICATION = new Type("Notification")



}