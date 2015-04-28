/**
 *
 */
package values

/**
 * @author Vaibhav
 *
 */
class FunType(domain: Type, range:Type) extends Type{

  this.typ = Type.FUNCTION;

  override def toString(): String ={
      domain+" -> "+range;
    }
  
   def getDomain(): Type ={
      domain;
    }
   
   def getRange(): Type ={
      range;
   }
}