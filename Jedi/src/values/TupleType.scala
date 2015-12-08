/**
 *
 */
package values

import scala.collection.mutable.MutableList

/**
 * @author Vaibhav
 * TupleType class to represent type of tuple
 */
class TupleType(components: List[Type]) extends Type {
    //Type of TupleType is function
    this.typ = Type.TUPLE;

    override def toString(): String ={

      "("+components.mkString(" * ")+")"
    }

   /**
    * Function to return types of components
    */
    def getComponent(): List[Type] ={

      components
    }
         
   /**
    * Function to check whether type is sub-type of other type or not
    * @param other
    */
    override def subType(other:Type): Boolean ={
     
     //If other type is 'Type' i.e. base type
     if(other==Type.TYPE)
       return true
       
     //If other type is not a tuple type then return false  
     if(!other.isInstanceOf[TupleType])
       return false;
     
     //cast other type to tuple type
     val tupleType = other.asInstanceOf[TupleType]
     
     //Apply rule of tuple sub-typing
     if(tupleType.getComponent().size==components.size){
        
        for((m,n) <- components.zip(tupleType.getComponent())){
          if(!m.toString().equals(n.toString()) && !m.subType(n)){
            return false            
          }
        }
      }
      else{
            return false            
      }     
     return true
   }
}