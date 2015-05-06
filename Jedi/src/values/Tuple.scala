/**
 *
 */
package values

import expressions.Literal
import scala.collection.mutable.ListBuffer
import expressions.Identifier

/**
 * @author Vaibhav
 *
 */
class Tuple(components: List[Value]) extends Literal with Value {

  typ=Type.TUPLE

  def get(index: Int):Value ={
    this.components(index)
  }
  
  override def toString(): String ={

      "("+components.mkString(" * ")+")"
  }

  def getComponent(): List[Value] ={
    components
  }

  override def getType():Type ={
    
    var typeList = new ListBuffer[Type]()
    for(vals <- components)
      typeList+= vals.getType();
    this.typ = new TupleType(typeList.toList)
    this.typ
  }
}