/**
 *
 */
package values

import expressions.Literal

/**
 * @author Vaibhav
 *
 */
class Boole(val value: Boolean) extends Literal with Value {

    this.typ = Type.BOOLE;

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

}

object Boole{
  
    def apply(value: Boolean){
      new Boole(value)
    }
  
    def test(){
      
      val bool1 = new Boole(true);
      val bool2 = new Boole(false);
      
      println("True boole from string true string, expected true, actual:"+new Boole("true"))
      println("Flase boole from string false string, expected false, actual:"+new Boole("false"))
      println()
      println("true && false => Expect: false, Result: " + (bool1 && bool2))
      println("false && true => Expect: false, Result: " + (bool2 && bool1))
      println("true && true => Expect: true, Result: " + (bool1 && bool1))
      println("false && false => Expect: false, Result: " + (bool2 && bool2))
      println()
      println("true || false => Expect: true, Result: " + (bool1 || bool2))
      println("false || true => Expect: true, Result: " + (bool2 || bool1))
      println("true || true => Expect: true, Result: " + (bool1 || bool1))
      println("false || false => Expect: false, Result: " + (bool2 || bool2))
      println()
      println("!true => Expect: false, Result: " + (!bool1))
      println("!false => Expect: true, Result: " + (!bool2))
      println("!(!true) => Expect: true, Result: " + (!(!bool1)))
      println("!(!false) => Expect: false, Result: " + (!(!bool2)))
      println()
      println("(true && false) || ((true && true) && !(false || false)) => Expect: true, Result: " + ((bool1 && bool2) || ((bool1 && bool1) && !(bool2 || bool2))))
      println()
    }
    
    def getType(env: Environment):Type ={
      Type.BOOLE
    }
}