//Author Zach Baklund
//Last Modified 12/9/18

class ArithParser extends Parser[String, Int] {
  type ParserWrapper[X] = List[X]
  var idx: Int = 0

  //Takes the string from the input and returns a abstracted wrapper type 
  //to handle throwing exceptions to the top level of the method call stack
  def invokeParser(input: String): ParserWrapper[Int] = {
    var wrapper: ParserWrapper[Int] = List()
    try{
      var result: Int = myParse(input)
      wrapper = List(result)
    } catch {
      case e: Exception => wrapper = List()
    }
    return wrapper
  }

  //Unwrapping method that returns the result from the wrapper type abstraction
  def unwrap(result: ParserWrapper[Int]): Int = {
    var ret: Int = 0
    //Error was thrown from invokeParser and transformed into an empty list
    if(result.isEmpty) throw new IllegalArgumentException("Malformed Input")
    ret = result.head
    return ret
  }

  //Helper method to call my instance of parsing underneath the hood
  def myParse(str: String): Int = {
    idx = 0
    var result: Int = 0
    result = expr(str)
    //Not all of the input was consumed
    if(idx < str.length()) throw new IllegalArgumentException("Malformed Input")
    return result
  }

  //Top level non-terminal in the grammar with addition and subtraction parsing
  def expr(str: String): Int = {
    var result: Int = factor(str)
    while(idx < str.length() && (str.charAt(idx) == '+' || str.charAt(idx) == '-')){
      if(str.charAt(idx) == '+'){
        idx += 1
        result = result + factor(str)
      }else if(str.charAt(idx) == '-'){
        idx += 1
        result = result - factor(str)
      }else {
        //Not a possible place to parse into but exception just in case
        throw new IllegalArgumentException("Malformed Input")
      }
    }
    return result
  }

  //Factor in the grammar for parsing multiplication and division
  def factor(str: String): Int = {
    var result: Int = term(str)
    while(idx < str.length() && (str.charAt(idx) == '*' || str.charAt(idx) == '/')){
      if(str.charAt(idx) == '*'){
        idx += 1
        result = result * term(str)
      }else if(str.charAt(idx) == '/'){
        idx += 1
        result = result / term(str)
      }else{
        //Not a possible place to parse into but exception just in case
        throw new IllegalArgumentException("Malformed Input")
      }
    }
    return result
  }

  //Terminal grammar entry
  def term(str: String): Int = {
    var result: Int = 0
    var unaryMinus: Boolean = false
    //Loop to handle sequential unary negations
    while(idx < str.length() && str.charAt(idx) == '-'){
      unaryMinus = !unaryMinus
      idx += 1
    }
    if(Character.isDigit(str.charAt(idx))){
      var num: Int = number(str)
      result = num
    }else if(str.charAt(idx) == '('){
      idx += 1
      result = expr(str)
      if(str.charAt(idx) != ')'){
        //Missing ending parenthesis
        throw new IllegalArgumentException("Malformed Input")
      }
      idx += 1
    }else{
      //Non-digit & non-parenthesis
      throw new IllegalArgumentException("Malformed Input")
    }
    //Negates input if unaryMinus flag is raised else returns result unmodified
    return if(unaryMinus) -1 * result else result
  }

  //Number parser
  def number(str: String): Int = {
    var result: Int = 0
    while(idx < str.length() && Character.isDigit(str.charAt(idx))){
      result = 10 * result + str.charAt(idx).asDigit
      idx += 1
    }
    return result
  }
}