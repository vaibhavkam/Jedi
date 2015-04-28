/**
 *
 */
package values

import scala.collection.mutable.MutableList

/**
 * @author Vaibhav
 *
 */
class TupleType(components: List[Type]) extends Type {

    this.typ = Type.TUPLE;

    override def toString(): String ={

      components.mkString(" * ")
    }

    def getComponent(): List[Type] ={

      components
    } 
}