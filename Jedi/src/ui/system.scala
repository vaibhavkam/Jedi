/**
 *
 */
package ui

import values.Value
import values.Number
import expressions.Identifier
import values.Boole
import values.Variable
import values.Type
import values.TupleType
import scala.collection.mutable.MutableList
import values.FunType
import scala.collection.mutable.ListBuffer
import expressions.Expression

/**
 * @author Vaibhav
 *
 */
object system {

    def execute(opcode: Identifier, args: List[Value]): Value = {
      
      opcode.name match {        
      case "add" => add(args)
      case "sub" => sub(args)
      case "mul" => mul(args)
      case "div" => div(args)
      case "equals" => equals(args)
      case "less" => less(args)
      case "not" => not(args)
      case "var" => varFunc(args)
      case "val" => valFunc(args)
      case "valType" => valType(args)
      case "tuple" => tuple(args)
      case "fun" => fun(args)
      case "typeLess" => typeLess(args)
      case _ => throw new UndefinedException(opcode.name)
    }
  }
    
  def getType(opcode: Identifier, args: List[Type]): Type = {
      opcode.name match {
      case "add" => getAddType(args)
      case "sub" => getSubType(args)
      case "mul" => getMulType(args)
      case "div" => getDivType(args)
      case "fun" => getFunType(args)
      case "tuple" => getTupleType(args)
      case _ => throw new UndefinedException(opcode.name)
    }
  }

    private def add(vals: List[Value]): Value = {
        
      if (vals.isEmpty) throw new TypeException("addition expects > 0 inputs")
      val ok = vals.filter(_.isInstanceOf[Number])
      if (ok.length < vals.length) throw new TypeException("all addition inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
      args2.reduceLeft(_+_)
    }
    
    private def sub(vals: List[Value]): Value = {
        
      if (vals.isEmpty) throw new TypeException("Substraction expects > 0 inputs")
      val ok = vals.filter(_.isInstanceOf[Number])
      if (ok.length < vals.length) throw new TypeException("all substraction inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
      args2.reduceLeft(_-_)
    }
    
    private def mul(vals: List[Value]): Value = {
       
      if (vals.isEmpty) throw new TypeException("Multiplication expects > 0 inputs")
      val ok = vals.filter(_.isInstanceOf[Number])
      if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
      args2.reduceLeft(_*_)
    }

    private def div(vals: List[Value]): Value = {
        
      if (vals.isEmpty) throw new TypeException("Multiplication expects > 0 inputs")
      val ok = vals.filter(_.isInstanceOf[Number])
      if (ok.length < vals.length) throw new TypeException("all multiplication inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
      args2.reduceLeft(_*_)
    }
    
    private def equals(vals: List[Value]): Value = {
      if (vals.length <= 1) throw new TypeException("equality expects > 1 inputs")
      val ok = vals.filter(_.isInstanceOf[Number])
      if (ok.length < vals.length) throw new TypeException("all equality inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
      val cmp = args2(0)
      val equality = args2.tail.filterNot(_.value == cmp.value)
      if (equality.length > 0) new Boole(false) else new Boole(true)
    }
    
    private def less(vals: List[Value]): Value = {
      if (vals.length <= 1) throw new TypeException("less than comparison expects > 1 inputs")
      val ok = vals.filter(_.isInstanceOf[Number])
      if (ok.length < vals.length) throw new TypeException("all less than comparison inputs must be numbers")
      val args2 = vals.map(_.asInstanceOf[Number])
         
      def helper(cmp: Number, list: List[Number]): Value = {
        if (list.length > 0) {
          if ((cmp < list.head).value) helper(list.head, list.tail) else new Boole(false)
        }
        else {
          new Boole(true)
        }
      }
      
      helper(args2.head, args2.tail)
    }
    
    private def not(vals: List[Value]): Value = {
      if (vals.isEmpty) throw new TypeException("not expects > 0 inputs")
      val ok = vals.filter(_.isInstanceOf[Boole])
      if (ok.length < vals.length) throw new TypeException("all not inputs must be booles")
      val args2 = vals.map(_.asInstanceOf[Boole])
      new Boole(args2.filter(_.value == true).length <= 0)
    }

    private def varFunc(vals: List[Value]): Value = {
      if (vals.isEmpty || vals.length > 1) 
        throw new TypeException("var expects 1 input")
      new Variable(vals.head)
    }
  
    private def valFunc(vals: List[Value]): Value = {
      
      if (vals.isEmpty || vals.length > 1) 
        throw new TypeException("val expects 1 input")
      
      if (vals.head.isInstanceOf[Variable]) {
        vals.head.asInstanceOf[Variable].content
      }
      else {
        throw new TypeException("val expects a variable to dereference")
      }
    }
    
    private def valType(vals: List[Value]): Value = {
      
      if (vals.isEmpty || vals.length > 1) 
        throw new TypeException("type expects 1 input")
      
      vals.head.typ
    }

   private def tuple(vals: List[Value]): Value = {
      
      if (vals.isEmpty) 
        throw new TypeException("Tuple expects more than 0 input")

      var typeList = new ListBuffer[Type]()
      for( va <- vals ){
          typeList+=va.asInstanceOf[Type]
      }
      new TupleType(typeList.toList)
    }

   private def fun(vals: List[Value]): Value = {
      
      if (vals.isEmpty) 
        throw new TypeException("Function expects more than 1 input")
      
        new FunType(vals.head.asInstanceOf[Type],vals.last.asInstanceOf[Type])
    }

   private def typeLess(vals: List[Value]): Value = {
      
     if (vals.length <= 1) throw new TypeException("less than comparison expects > 1 inputs")
               
      new Number(vals.head.asInstanceOf[Type].compareTo(vals.last.asInstanceOf[Type]))
    }

//   private def getMulType(vals: List[Value]): Type = {
//     var t: Value = null;
//     for( va <- vals ){
//         if(va.isInstanceOf[Type])
//          t=va;
//         else
//          t=va.typ;
//         
//         if(!t.asInstanceOf[Type].toString().equalsIgnoreCase(Type.NUMBER.toString()))
//           return Type.ERROR
//      }
//      Type.NUMBER
//    }
//   
//   private def getDivType(vals: List[Value]): Type = {
//     var t: Value = null;
//     for( va <- vals ){
//         if(va.isInstanceOf[Type])
//          t=va;
//         else
//          t=va.typ;
//         
//         if(!t.asInstanceOf[Type].toString().equalsIgnoreCase(Type.NUMBER.toString()))
//           return Type.ERROR
//      }
//      Type.NUMBER
//    }
      
    private def getAddType(types: List[Type]): Type = {   
      
     if (types.isEmpty) 
       return  Type.ERROR
     val ok = types.filter(_.toString().equals(Type.NUMBER.toString()))
     
     if (ok.length < types.length)
        return Type.ERROR
     Type.NUMBER
    }

    private def getSubType(types: List[Type]): Type = {    
     if (types.isEmpty) 
       return  Type.ERROR
     val ok = types.filter(_.toString().equals(Type.NUMBER.toString()))
     
     if (ok.length < types.length)
        return Type.ERROR
     Type.NUMBER
    }

    private def getMulType(types: List[Type]): Type = {    
     if (types.isEmpty) 
       return  Type.ERROR
     val ok = types.filter(_.toString().equals(Type.NUMBER.toString()))
     
     if (ok.length < types.length)
        return Type.ERROR
     Type.NUMBER
    }

    private def getDivType(types: List[Type]): Type = {    
     if (types.isEmpty) 
       return  Type.ERROR
     val ok = types.filter(_.toString().equals(Type.NUMBER.toString()))
     
     if (ok.length < types.length)
        return Type.ERROR
     Type.NUMBER
    }

    private def getFunType(types: List[Type]): Type = {
      
      if (types.isEmpty) 
       return  Type.ERROR
            
       val ok = types.filter(_.toString().equals(Type.TYPE.toString()))
       
       if (ok.length < types.length)
          return Type.ERROR
       Type.FUNCTION
    }

    private def getTupleType(types: List[Type]): Type = {
      
      if (types.isEmpty) 
       return  Type.ERROR
            
       val ok = types.filter(_.toString().equals(Type.TYPE.toString()))
       
       if (ok.length < types.length)
          return Type.ERROR
       Type.TUPLE
    }

}