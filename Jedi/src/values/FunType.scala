/**
 *
 */
package values

/**
 * @author Vaibhav
 * FunType class to represent type of function
 */
class FunType(domain: Type, range:Type) extends Type{

  //Type of FunType is function
  this.typ = Type.FUNCTION;

   override def toString(): String ={
      "("+domain+" -> "+range+")";
    }
  
   /**
    * Function to return type of function domain
    */
   def getDomain(): Type ={
      domain;
    }
   
   /**
    * Function to return type of function range
    */
   def getRange(): Type ={
      range;
   }
      
   /**
    * Function to check whether type is sub-type of other type or not
    * @param other
    */
   override def subType(other:Type): Boolean ={
     
     //If other type is 'Type' i.e. base type
     if(other==Type.TYPE)
       return true
     
     //If other type is not a function type then return false  
     if(!other.isInstanceOf[FunType])
       return false;
     
     //cast other type to function type
     val funType = other.asInstanceOf[FunType]
          
     //Apply rule of function sub-typing
     return funType.getDomain().subType(domain) && range.subType(funType.getRange())
   }
}