/**
 *
 */
package values

/**
 * @author Vaibhav
 * Trait to represent value in Jedi
 */
trait Value extends Serializable {

  //Type of value is 'value'
  protected var typ: Type = Type.VALUE;
  
  /**
   * Function to get type of value
   */
  def getType():Type = { return typ }
  
  def setType(typ:Type){
    this.typ = typ
  }
}