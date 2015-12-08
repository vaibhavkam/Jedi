package ui
import values._
import expressions._

/**
 * @author Vaibhav
 *
 */
object console {
   
  val parsers = new Parser
  val globalEnv = new Environment()
  var verbose = true
  val boolean = Type.BOOLEAN
  boolean.typ=Type.TYPE
  val number = Type.NUMBER
  number.typ=Type.TYPE
  val rational = Type.RATIONAL
  rational.typ=Type.TYPE

  def parse(cmmd: String): Expression = {
    
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => tree.get
    }
  }

  def execute(cmmd: String): String = {
    
    val tree = parsers.parseAll(parsers.expression, cmmd)

    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => "" + tree.get.execute(globalEnv)
    }
  }


  def getType(cmmd: String): String = {
    
    val tree = parsers.parseAll(parsers.expression, cmmd)
    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _ => "" + tree.get.getType(globalEnv)
    }
  }

  def repl {
        
    globalEnv.put(new Identifier("Boolean"), boolean);
    globalEnv.put(new Identifier("Number"), number);
    globalEnv.put(new Identifier("Rational"), rational);
    
    var cmmd: String = ""
    var more = true
    while (more) {
      try {
        print("-> ")
        val cmmd = readLine()
        if (cmmd == "help") {
          println("help")
        } else if (cmmd == "quit") {
          println("bye")
          more = false
        } else if(cmmd.split(" +")(0)=="type"){
          println(getType(cmmd.split(" +",2)(1)).toString());
        } 
        else {
          
          if(!getType(cmmd).toString().equalsIgnoreCase("Error"))
            println(execute(cmmd).toString())
          else
            println("Error")
        }
      } catch {
        case e: SyntaxException => {
          println(e.msg)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        case e: TypeException =>
          { println(e.msg); if (verbose) e.printStackTrace() }
        case e: UndefinedException =>
          { println(e.msg); if (verbose) e.printStackTrace() }
        case e: JediException =>
          { println(e.msg); if (verbose) e.printStackTrace() }
        case e: Throwable =>
          { println("error: " + e); if (verbose) e.printStackTrace() }
      } finally {
        Console.flush
      }
    }
  }

  
   def test {
        
    globalEnv.put(new Identifier("Boolean"), boolean);
    globalEnv.put(new Identifier("Number"), number); 
  }

  def main(args: Array[String]): Unit = { repl }
}