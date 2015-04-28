/**
 *
 */
package values

/**
 * @author Vaibhav
 *
 */
class Integer(val valu:Double) extends Number(valu){

    this.typ = Type.INTEGER;
    
   override def getType(env: Environment):Type ={
      Type.NUMBER
    }
}