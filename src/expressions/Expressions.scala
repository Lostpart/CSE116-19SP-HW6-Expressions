package expressions

import java.util

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks._


object Expressions {
//  def main(args: Array[String]): Unit = {
//    evaluateArithmetic("4 + 5 * (3 + 1 - 2) - 20 / 2")
//  }

  def evaluate[A](input: String, stringToNumber :String => A, operatorToFunction:Map[String, (A, A)=> A],order:List[List[String]]):A={
    // 分割输入式子
    var pre_expression: String = input
    pre_expression = pre_expression.replace(" ", "")
    for(a <- operatorToFunction.keys.toList){

      pre_expression = pre_expression.replace(a, "_" + a + "_")

    }
    pre_expression = pre_expression.replace("(", "_(_")
    pre_expression = pre_expression.replace(")", "_)_")
    pre_expression = pre_expression.replace(" ", "")

    // 分割文本
    var expression:List[String] = pre_expression.split("_").toList
    expression = expression.filter(s => s != "")

    //初始化参数
    // SY short for Shouting Yard
    var SY_left : ListBuffer[String]= new ListBuffer[String]
    var SY_stack : util.LinkedList[String]= new util.LinkedList[String]
    var SY_rank : Map[String, Int] = calOperatorRank(operatorToFunction.keys.toList, order)


    //调度场算法 将算式转为后缀表达式
    for(i <- expression){

      if(isOperator(i,operatorToFunction.keys.toArray)){
        //是运算符 需要进行额外判断
        //println("符号"+i.toString)
        if(SY_stack.size() == 0){
          //stack为空，直接插入
          SY_stack.addFirst(i)
        } else if(SY_rank(i) > SY_rank(SY_stack.getFirst)){
          //如果i比最顶上的优先级要大的话 则直接插入
          SY_stack.addFirst(i)
        } else if(SY_rank(i) == SY_rank(SY_stack.getFirst)){
          //如果i比最顶上的优先级要相等的话 移走栈内符号 随后放入栈内
          SY_left += SY_stack.removeFirst()
          SY_stack.addFirst(i)
        } else if(SY_rank(i) < SY_rank(SY_stack.getFirst)){
          //i比最上方的运算符优先度要低，先将当前栈内括号前内容移动到左侧
          while(SY_stack.size() != 0 && SY_stack.getFirst != "(" ){
            SY_left += SY_stack.removeFirst()
          }
          SY_stack.addFirst(i)
        }
        //      } else if(isNumber(i)){
        //        //println("数字"+i.toString)
        //        //是数字，直接移动到左侧
        //        SY_left += i //此处在Bonus Object处需要修改
      } else if(i == "("){
        //左括号，移动到下侧
        SY_stack.addFirst(i)
      } else if(i == ")"){
        //右括号，在栈中找到左括号并且把在这之中的运算符全部移动到左侧
        while(SY_stack.getFirst != "("){
          SY_left += SY_stack.removeFirst()
        }
        SY_stack.removeFirst()
      } else {
        SY_left += i
      }
      //      println(SY_left.toList.toString + "  |  " + SY_stack.toString)
    }
    while(SY_stack.size() != 0){
      SY_left += SY_stack.removeFirst()
    }
    var inverse_expression:List[String]= SY_left.toList

    //println(inverse_expression.toList.toString)

    while(inverse_expression.size > 1){
      breakable{
        for(i <- inverse_expression.indices){
          if(isOperator(inverse_expression(i),operatorToFunction.keys.toArray)){
            val ans: A = operatorToFunction(inverse_expression(i))(stringToNumber(inverse_expression(i-2)),stringToNumber(inverse_expression(i-1)))
            //println(inverse_expression.toString + "| i = " + i.toString)
            inverse_expression = inverse_expression.take(i-2) ::: List(ans.toString) ::: inverse_expression.drop(i+1)
            break()
          }
        }
      }
      //println(inverse_expression.size)
    }

    stringToNumber(inverse_expression.head)
  }


  //def removeIndex[A](List:List[A], i: Int): List[A] = List.take(i) ++ List.drop(i+1)

  def calOperatorRank(operatorlist:List[String], order:List[List[String]]):Map[String, Int]={

    var result : Map[String, Int] = Map("(" -> -100)
    for(o <- operatorlist){
      var currentRank : Int = order.length + 100
      breakable{
        for(a <- order){
          currentRank = currentRank - 1
          for(b <- a){
            if(o == b){
              currentRank = currentRank - 100
              break()
            }
          }
        }
      }
      result += (o -> currentRank)
    }


    result
  }
  def isNumber(c: String):Boolean={
    (c >= "0" && c <= "9") || (c >= "a" && c <= "z" || c >= "A" && c <= "Z")
  }
  def isOperator(c: String,operators:Array[String]):Boolean={
    for(i <- operators){
      if(c == i){
        return true
      }
    }
    false
  }

  def isVariable(c: String,variables:Array[String]):Boolean={
    for(i <- variables){
      if(c == i){
        return true
      }
    }
    false
  }

  def evaluateArithmetic(expression: String): Double = {
    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b
    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "^" -> pow,
      "*" -> mul,
      "/" -> div,
      "+" -> add,
      "-" -> sub
    )
    val order = List(List("^"), List("*", "/"), List("+", "-"))
    evaluate(expression, (s: String) => s.toDouble, operatorTable, order)
  }
  def evaluateBoolean(expression: String): Boolean = {
    val and = (a: Boolean, b: Boolean) => a && b
    val or = (a: Boolean, b: Boolean) => a || b
    val xor = (a: Boolean, b: Boolean) => (a || b) && !(a && b)
    val implies = (a: Boolean, b: Boolean) => !(a && !b)
    val iff = (a: Boolean, b: Boolean) => (a && b) || (!a && !b)
    val operatorTable: Map[String, (Boolean, Boolean) => Boolean] = Map(
      "&&" -> and,
      "||" -> or,
      "xor" -> xor,
      "->" -> implies,
      "<>" -> iff
    )
    val order = List(List("&&"), List("||", "xor"), List("->", "<>"))
    evaluate(expression, (s: String) => s.toBoolean, operatorTable, order)
  }
  def SL_openfile(filename:String):Array[String]={
    val file = Source.fromFile(filename)
    file.getLines().toArray
  }

  def runScriptArithmetic(filename: String): Double = {
    val pow = (a: Double, b: Double) => Math.pow(a, b)
    val mul = (a: Double, b: Double) => a * b
    val div = (a: Double, b: Double) => a / b
    val add = (a: Double, b: Double) => a + b
    val sub = (a: Double, b: Double) => a - b
    val operatorTable: Map[String, (Double, Double) => Double] = Map(
      "^" -> pow,
      "*" -> mul,
      "/" -> div,
      "+" -> add,
      "-" -> sub
    )
    val order = List(List("^"), List("*", "/"), List("+", "-"))
    runScript(filename, (s: String) => s.toDouble, operatorTable, order)
  }

  def runScript[A](filename: String, stringToNumber :String => A, operatorToFunction:Map[String, (A, A)=> A],order:List[List[String]]):A={
    var script:Array[String] = SL_openfile(filename)
    var variableList:Map[String,String]=Map()
    var ans:String = ""
    breakable{
      for(line <- script){
        var lcl_mode:String = ""
        var lcl_variable: String =""

        var pre_expression: String = line
        pre_expression = pre_expression.replace(" ", "")

        if(pre_expression.contains("=")){
          lcl_variable = pre_expression.split("=").toList.head
          pre_expression = pre_expression.split("=").toList(1)
          if(variableList.keys.toList.contains(lcl_variable)){
            //已知变量
            lcl_mode = "Known"
          } else {
            //未知变量
            lcl_mode = "Unknown"
          }
        } else {
          //最后一行
          lcl_mode = "LastLine"
        }






        for(a <- operatorToFunction.keys.toList){

          pre_expression = pre_expression.replace(a, "_" + a + "_")

        }
        pre_expression = pre_expression.replace("(", "_(_")
        pre_expression = pre_expression.replace(")", "_)_")
        pre_expression = pre_expression.replace(" ", "")

        // 分割文本
        var expression:List[String] = pre_expression.split("_").toList
        expression = expression.filter(s => s != "")

        //初始化参数
        // SY short for Shouting Yard
        var SY_left : ListBuffer[String]= new ListBuffer[String]
        var SY_stack : util.LinkedList[String]= new util.LinkedList[String]
        var SY_rank : Map[String, Int] = calOperatorRank(operatorToFunction.keys.toList, order)


        //调度场算法 将算式转为后缀表达式
        for(i <- expression){

          if(isOperator(i,operatorToFunction.keys.toArray)){
            //是运算符 需要进行额外判断
            //println("符号"+i.toString)
            if(SY_stack.size() == 0){
              //stack为空，直接插入
              SY_stack.addFirst(i)
            } else if(SY_rank(i) > SY_rank(SY_stack.getFirst)){
              //如果i比最顶上的优先级要大的话 则直接插入
              SY_stack.addFirst(i)
            } else if(SY_rank(i) == SY_rank(SY_stack.getFirst)){
              //如果i比最顶上的优先级要相等的话 移走栈内符号 随后放入栈内
              SY_left += SY_stack.removeFirst()
              SY_stack.addFirst(i)
            } else if(SY_rank(i) < SY_rank(SY_stack.getFirst)){
              //i比最上方的运算符优先度要低，先将当前栈内括号前内容移动到左侧
              while(SY_stack.size() != 0 && SY_stack.getFirst != "(" ){
                SY_left += SY_stack.removeFirst()
              }
              SY_stack.addFirst(i)
            }
            //      } else if(isNumber(i)){
            //        //println("数字"+i.toString)
            //        //是数字，直接移动到左侧
            //        SY_left += i //此处在Bonus Object处需要修改
          } else if(i == "("){
            //左括号，移动到下侧
            SY_stack.addFirst(i)
          } else if(i == ")"){
            //右括号，在栈中找到左括号并且把在这之中的运算符全部移动到左侧
            while(SY_stack.getFirst != "("){
              SY_left += SY_stack.removeFirst()
            }
            SY_stack.removeFirst()
          } else if(isVariable(i,variableList.keys.toArray)){
            //变量
            SY_left += variableList(i)
          } else {
            //“数字”
            SY_left += i
          }
          //      println(SY_left.toList.toString + "  |  " + SY_stack.toString)
        }
        while(SY_stack.size() != 0){
          SY_left += SY_stack.removeFirst()
        }
        var inverse_expression:List[String]= SY_left.toList

        //println(inverse_expression.toList.toString)

        while(inverse_expression.size > 1){
          breakable{
            for(i <- inverse_expression.indices){
              if(isOperator(inverse_expression(i),operatorToFunction.keys.toArray)){
                val ans: A = operatorToFunction(inverse_expression(i))(stringToNumber(inverse_expression(i-2)),stringToNumber(inverse_expression(i-1)))
                //println(inverse_expression.toString + "| i = " + i.toString)
                inverse_expression = inverse_expression.take(i-2) ::: List(ans.toString) ::: inverse_expression.drop(i+1)
                break()
              }
            }
          }
          //println(inverse_expression.size)
        }


        if(lcl_mode == "Known"){
          variableList = variableList + (lcl_variable -> inverse_expression.head)
        } else if(lcl_mode == "Unknown"){
          variableList = variableList + (lcl_variable -> inverse_expression.head)
        } else if(lcl_mode == "LastLine"){
          ans = inverse_expression.head
          break()
        }




      }
    }
    stringToNumber(ans)
  }
}
