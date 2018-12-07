class ArithParser extends Parser[String, Int] {
  type ParserWrapper[X] = List[X]
  var idx: Int = 0

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

  def unwrap(result: ParserWrapper[Int]): Int = {
    var ret: Int = 0
    if(result.isEmpty) throw new IllegalArgumentException("Malformed Input")
    ret = result.head
    return ret
  }

  def myParse(str: String): Int = {
    idx = 0
    var result: Int = 0
    result = expr(str)
    if(idx < str.length()) throw new IllegalArgumentException("Malformed Input")
    return result
  }
  def expr(str: String): Int = {
    var result: Int = term(str)
    while(idx < str.length() && (str.charAt(idx) == '+' || str.charAt(idx) == '-')){
      if(str.charAt(idx) == '+'){
        idx += 1
        result = result + term(str)
      }else if(str.charAt(idx) == '-'){
        idx += 1
        result = result - term(str)
      }else {
        throw new IllegalArgumentException("Malformed Input")
      }
    }
    return result
  }
  def term(str: String): Int = {
    var result: Int = factor(str)
    while(idx < str.length() && (str.charAt(idx) == '*' || str.charAt(idx) == '/')){
      if(str.charAt(idx) == '*'){
        idx += 1
        result = result * factor(str)
      }else if(str.charAt(idx) == '/'){
        idx += 1
        result = result / factor(str)
      }else{
        throw new IllegalArgumentException("Malformed Input")
      }
    }
    return result
  }
  def factor(str: String): Int = {
    var result: Int = 0
    var unaryMinus: Boolean = false
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
        throw new IllegalArgumentException("Malformed Input")
      }
      idx += 1
    }else{
      throw new IllegalArgumentException("Malformed Input")
    }
    return if(unaryMinus) -1 * result else result
  }
  def number(str: String): Int = {
    var result: Int = 0
    while(idx < str.length() && Character.isDigit(str.charAt(idx))){
      result = 10 * result + str.charAt(idx).asDigit
      idx += 1
    }
    return result
  }
}