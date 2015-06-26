package values

/**
 * @author vkamble
 */
class VarType(contentType: Type) extends Type  {
  
  this.typ = Type.VARIABLE;

  override def toString(): String ={
      this.typ+"("+contentType.toString()+")";
  }

}