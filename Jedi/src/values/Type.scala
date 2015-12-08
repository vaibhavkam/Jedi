/**
 *
 */
package values

import ui.TypeException

/**
 * @author Vaibhav
 * Class to represent base type in Jedi
 */
class Type(str: String =null) extends Value {

    //Type of Type is 'Type'
    this.typ = Type.TYPE;
    
    override def toString(): String ={
      this.str
    }
    
   /**
    * Function to check whether type is sub-type of other type or not
    * @param other
    */    
    def subType(other:Type): Boolean = {
      
      //If type is equal to other type, return true
      if(this == other)
        return true;
      
      //Error type is sub-type all type
      if(this ==Type.ERROR)
        return true
       
      //Rational is sub-type of Number type  
      if(this == Type.RATIONAL && other == Type.NUMBER)
        return true
        
      return false  
    }
}

/**
 * Object Type to define primitive types in Jedi
 */
object Type{
    
    val NUMBER = new Type("Number")
    val RATIONAL = new Type("Rational")
    val LITERAL = new Type("Literal")
    val BOOLEAN = new Type("Boolean")
    val VALUE = new Type("Value")
    val TYPE = new Type("Type")
    val ERROR = new Type("Error")
    val VARIABLE = new Type("Variable")
    val CLOSURE = new Type("Closure")
    val TUPLE = new Type("Tuple")
    val FUNCTION = new Type("Function")
    val VOID = new Type("Void")
    val NOTIFICATION = new Type("Notification")
}