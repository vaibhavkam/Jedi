package ui

import scala.util.parsing.combinator._
import expressions._
import values._

class Parser extends RegexParsers {

  def expression: Parser[Expression] =   tupleType| getTupleValue | fun | declaration | conditional | disjunction | failure("Invalid expression")
  
  def expressionTypeCheck: Parser[Expression] =  tupleType| fun|  identifier
  
  def tupleType: Parser[Expression] = "TupleType" ~ operands ^^{
    case "TupleType" ~ exp => new FunCall(Identifier("tupleType"), exp)
    case _ => throw new SyntaxException()
  }

  def fun: Parser[Expression] = "Fun" ~ operands ^^{
    case "Fun" ~ exp => new FunCall(Identifier("fun"), exp)
    case _ => throw new SyntaxException()
  }
  
//  def valType: Parser[Expression] = "type" ~ expression^^{
//    case "type" ~ exp => new FunCall(Identifier("valType"), List(exp))
//    case _ => throw new SyntaxException()
//  }
//  

  def declaration: Parser[Expression] = "def" ~ identifier ~ "=" ~ expression ^^{
    case "def" ~ id ~ "=" ~ exp => new Declaration(id, exp)
    case _ => throw new SyntaxException()
  }

  def conditional: Parser[Conditional] = "if" ~ "(" ~ expression ~ ")" ~ expression ~ opt("else" ~ expression) ^^{
    case "if" ~ "(" ~ e1 ~ ")" ~ e2 ~ None => new Conditional(e1, e2)
    case "if" ~ "(" ~ e1 ~ ")" ~ e2 ~ Some("else" ~ e3) => new Conditional(e1, e2, e3)
    case _ => throw new SyntaxException()
  }

  def disjunction: Parser[Expression] = conjunction ~ rep("||" ~> conjunction) ^^{
    case t ~ Nil => t
    case t ~ rest => new Disjunction(t :: rest)
    case _ => throw new SyntaxException()
  }

  def conjunction: Parser[Expression] = equality ~ rep("&&" ~> equality) ^^{
    case t ~ Nil => t
    case t ~ more => new Conjunction(t :: more)
    case _ => throw new SyntaxException()
  }

  def equality: Parser[Expression] = inequality ~ rep("==" ~> inequality) ^^{
    case t ~ Nil => t
    case t ~ rest => FunCall(Identifier("equals"), t :: rest)
    case _ => throw new SyntaxException()
  }

  def inequality: Parser[Expression] = sum ~ rep("<" ~> sum) ^^{
    case t ~ Nil => t
    case t ~ rest => FunCall(Identifier("less"), t :: rest)
    case _ => throw new SyntaxException()
  }

  def negate(exp: Expression): Expression = {
    val sub = Identifier("sub")
    val zero = Number(0)
    FunCall(sub, List(zero, exp))
  }

  def sum: Parser[Expression] = product ~ rep(("+" | "-­") ~ product ^^ {
    case "+" ~ s => s
    case "-" ~ s => negate(s)
  }) ^^ {
    case p ~ Nil => p
    case p ~ rest => new FunCall(Identifier("add"), p :: rest)
    case _ => throw new SyntaxException()
  }

  def inverse(exp: Expression): Expression = {
    val div = Identifier("div")
    val one = Number(1)
    FunCall(div, List(one, exp))
  }

  def product: Parser[Expression] = funcall ~ rep(("*" | "/") ~ funcall ^^ {
    case "*" ~ s => s
    case "/" ~ s => inverse(s)
  }) ^^ {
    case p ~ Nil => p
    case p ~ rest => new FunCall(Identifier("mul"), p :: rest)
    case _ => throw new SyntaxException()
  }
  
  def funcall: Parser[Expression] = term ~ opt(operands) ^^
    {
      case t ~ None => t
      case t ~ Some(ops) => FunCall(t, ops)
      case _ => throw new SyntaxException()
    }
  
  def operands: Parser[List[Expression]] = "(" ~> opt(expression ~ rep("," ~> expression)) <~ ")" ^^ {
    case None => Nil
    case Some(e ~ Nil) => List(e)
    case Some(e ~ exps) => e :: exps
    case _ => Nil
  }
  
  def term: Parser[Expression] =  typeOf | cast | variable| tuple | getTupleValue| assignment | iteration | deref | lambda | block | literal | identifier | "(" ~> expression <~ ")" | failure("Invalid term")

  def literal: Parser[Literal] = boole | numeral | rational

  def numeral: Parser[Number] = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^
    {
      case c => new Number(c.toDouble)
    }

  def boole: Parser[Boole] = """true|false""".r ^^
    {
      case e => new Boole(e.toBoolean)
    
    }

  def number = """(\+|-)?[0-9]+(\.[0-9]+)?""".r ^^ {
    case tree => new Identifier(tree)
  }

  def identifier: Parser[Identifier] = """[a-zA-Z][a-zA-Z0-9]*""".r ^^
    {
      case e => new Identifier(e)
    }
  
  def lambda: Parser[Expression] = "lambda" ~> parametersWithType ~ expression ^^ {
    case p~e => Lambda(p, e)
  }
  
  def parameters: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case None => Nil
    case Some(id1 ~ Nil) => List(id1)
    case Some(id1 ~ ids) => id1 +: ids
    case _ => Nil
  }
    
  def parametersWithType: Parser[Map[Identifier, Expression]] = "("~> repsep(member, ",") <~")" ^^ (Map() ++ _)
  
  def member: Parser[(Identifier, Expression)] = identifier~":"~expression ^^ { 
    case name~":"~value => (name, value) 
  }
  
  def block: Parser[Expression] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case exp1 ~ Nil => Block(List(exp1))
    case exp1 ~ exps => Block(exp1 +: exps)
  }

  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
    case id ~ "=" ~ exp => Assignment(id, exp)
  }
  
  def deref: Parser[Expression] = "[" ~> expression <~ "]" ^^ {
    case v => FunCall(Identifier("val"), List(v))
  }
  
  def iteration: Parser[Expression] = "while" ~ "(" ~ expression ~ ")" ~ expression ^^ {
    case "while" ~ "(" ~ exp1 ~ ")" ~ exp2 => Iteration(List(exp1, exp2))
  }

  def tuple: Parser[Expression] = "Tuple" ~> operands  ^^ {
    case p => new FunCall(Identifier("tuple"), p)
    case _ => throw new SyntaxException()

  }

  def getTupleValue: Parser[Expression] = "get" ~ operands ^^{
    case "get" ~ exp => new FunCall(Identifier("getTupleValue"), exp)
    case _ => throw new SyntaxException()
  }

  def rational: Parser[Rational] = "Rat" ~ "(" ~ numeral ~ "," ~ numeral~ ")"  ^^ {
    case "Rat" ~ "(" ~n1  ~ "," ~  n2~ ")" => new Rational(n1.toString(),n2.toString())
    case _ => throw new SyntaxException()
  }
  
  def variable: Parser[Expression] = "Variable" ~> operands  ^^ {
    case p => new FunCall(Identifier("variableType"), p)
    case _ => throw new SyntaxException()

  }
  
  def cast: Parser[Expression] = "Cast" ~ "[" ~ expression ~ "," ~ expression~ "]"  ^^ {
    case "Cast" ~ "[" ~e1  ~ "," ~  e2~ "]" => new Cast(e1,e2)
    case _ => throw new SyntaxException()
  }
  
  def typeOf: Parser[Expression] = "typeOf" ~ "(" ~ expression~ ")"  ^^ {
    case "typeOf" ~ "(" ~ exp~ ")"  => new FunCall(Identifier("typeOf"), List(exp))
    case _ => throw new SyntaxException()
  }


}