package expressions

import ui._
import values._

case class Block(locals: List[Expression]) extends SpecialForm {

  def execute(env: Environment): Value = {
	  val localenv = new Environment(env)
	  
	  if (locals.length > 1) {
      
      //Get reverse list so the last expression in the block can be returned by head
		  val rvlc = locals.reverse		  
      
      //Take all of the tail expressions, reverse them back, and execute in order
		  rvlc.tail.reverse.map(_.execute(localenv))
      
      //Save the last expression of the locals for last, execute it, and return it as the value
		  rvlc.head.execute(localenv) 
	  }
	  else if (locals.length == 1) {
      
      //Just execute the only expression and return it as the value
	    locals.head.execute(localenv) 
	  }
	  else {
	    throw new TypeException("block requires > 0 expressions")
	  }
	}
  

  def getType(env: Environment):Type ={
    Type.NUMBER
  }
}