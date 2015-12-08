package values

/**
 * @author vkamble
 */
class Rational(var num:Double, var den:Double) extends Number(num/den) {
  
  this.typ= Type.RATIONAL

  def this(numStr: String, denStr: String){  
    this(numStr.toDouble, denStr.toDouble)
    num = numStr.toDouble/gcd(numStr.toDouble, denStr.toDouble)
    den = denStr.toDouble/gcd(numStr.toDouble, denStr.toDouble)
  }
  
  override def +(other: Number): Number ={
    new Number(this.value + other.value)
  }
  
  override def -(other: Number): Number ={
    new Number(this.value - other.value)
  }
  
  override def *(other: Number): Number ={
    new Number(this.value * other.value)
  }
  
  override def /(other: Number): Number ={
    new Number(this.value / other.value)
  }

  override def ==(other: Number): Boole ={
    new Boole((this.value==other.value))
  }

  override def <(other: Number): Boole ={
    new Boole((this.value<other.value))
  }

  override def >(other: Number): Boole ={
    new Boole((this.value>other.value))
  }
  
  override def !=(other: Number): Boole ={ 
    new Boole((this.value!=other.value))
  }
 
  override def toString: String = value.toString

  override def getType():Type ={
     this.typ
  }
  
  def gcd(a: Double, b: Double): Double = if (b == 0) a.abs else gcd(b, a % b)

}