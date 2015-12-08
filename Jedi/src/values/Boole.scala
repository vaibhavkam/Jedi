/**
 *
 */
package values

import expressions.Literal

/**
 * @author Vaibhav
 * Class represents boolean data type
 *
 */ 
class Boole(val value: Boolean) extends Literal with Value {

    this.typ = Type.BOOLEAN;

    def this(value: String){
      this(value.toBoolean)
    }
    
    def &&(other: Boole): Boole ={
      new Boole(this.value && other.value)
    }

    def ||(other: Boole): Boole ={
      new Boole(this.value && other.value)
    }

    def unary_!(): Boole ={
      new Boole(!this.value)
    }

    override def toString(): String ={
      this.value.toString()
    }
  
    /**
     * Function to retrieve type of data
     */
    override def getType(env: Environment):Type ={
       this.typ
    }

}

object Boole{
  
    def apply(value: Boolean){
      new Boole(value)
    }

}