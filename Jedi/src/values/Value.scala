/**
 *
 */
package values

/**
 * @author Vaibhav
 *
 */
trait Value extends Serializable {

  var typ: Type = Type.VALUE;
  
  def getType():Type = { return typ }


}