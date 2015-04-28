/**
 *
 */
package values

import expressions.Literal

/**
 * @author Vaibhav
 *
 */
class Number(val value:Double) extends Literal with Value {

  this.typ = Type.NUMBER;


  def this(num: String){
    this(num.toDouble)
  }
  
  def +(other: Number): Number ={
    new Number(this.value + other.value)
  }
  
  def -(other: Number): Number ={
    new Number(this.value - other.value)
  }
  
  def *(other: Number): Number ={
    new Number(this.value * other.value)
  }
  
  def /(other: Number): Number ={
    new Number(this.value / other.value)
  }

  def ==(other: Number): Boole ={
    new Boole((this.value==other.value))
  }

  def <(other: Number): Boole ={
    new Boole((this.value<other.value))
  }

  def >(other: Number): Boole ={
    new Boole((this.value>other.value))
  }
  
  def !=(other: Number): Boole ={ 
    new Boole((this.value!=other.value))
  }
 
  override def toString: String = value.toString

}

object Number {
    
    def apply(v: Double) = { 
      
      new Number(v) 
    }
    
    def test() {
      
      val number1 = new Number(100)
      val number2 = new Number(25)
    
      println("Testing Number\n---------------")
      println("number1 = " + number1)
      println("number2 = " + number1)
      println()
      println("number1 + number2 => Expect: 125, Result: " + (number1 + number2))
      println("number2 + number1 => Expect: 125, Result: " + (number2 + number1))
      println()
      println("number1 - number2 => Expect: 75, Result: " + (number1 - number2))
      println("number2 - number1 => Expect: -75, Result: " + (number2 - number1))
      println()
      println("number1 * number2 => Expect: 2500, Result: " + (number1 * number2))
      println("number2 * number1 => Expect: 2500, Result: " + (number2 * number1))
      println()
      println("number1 / number2 => Expect: 4, Result: " + (number1 / number2))
      println("number2 / number1 => Expect: 0.25, Result: " + (number2 / number1))
      println()
      println("number1 == number2 => Expect: false, Result: " + (number1 == number2))
      println("number1 == new Number(100) => Expect: true, Result: " + (number1 == new Number(100)))
      println("number2 == new Number(25) => Expect: true, Result: " + (number2 == new Number(25)))
      println()
      println("number1 < number2 => Expect: false, Result: " + (number1 < number2))
      println("number2 < number1 => Expect: true, Result: " + (number2 < number1))
      println()
      println("number1 > number2 => Expect: true, Result: " + (number1 > number2))
      println("number2 > number1 => Expect: false, Result: " + (number2 > number1))
      println()
      println("number1 != number2 => Expect: true, Result: " + (number1 != number2))
      println()

    }
    
    def getType(env: Environment):Type ={
      Type.NUMBER
    }
 }