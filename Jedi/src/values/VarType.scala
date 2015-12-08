package values

/**
 * @author vkamble
 */
class VarType(contentType: Type) extends Type  {
  
  this.typ = Type.VARIABLE;

  override def toString(): String ={
      this.typ+"("+contentType.toString()+")";
  }

  def getContentType(): Type ={
    contentType
  }
  
  override def subType(other:Type): Boolean ={
     
     if(other==Type.TYPE)
       return true
       
     if(!other.isInstanceOf[VarType])
       return false;
     
     val varType = other.asInstanceOf[VarType]
     
     if(!varType.getContentType().toString().equalsIgnoreCase(contentType.toString()) && !contentType.subType(varType.getContentType()))
         return false            

     return true
   }
}