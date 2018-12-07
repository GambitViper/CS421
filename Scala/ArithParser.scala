class ArithParser {
  var idx: Int = 0
  
  def parseExpr(str: String): Int = {
    idx = 0
    return expr(str)
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
        //failure
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
        //failure
      }
    }
    return result
  }
  def factor(str: String): Int = {
    var result: Int = 0
    var unaryMinus: Boolean = false
    if(idx < str.length() && str.charAt(idx) == '-') unaryMinus = true
    if(Character.isDigit(str.charAt(idx))){
      var num: Int = number(str)
      result = num
    }else if(str.charAt(idx) == '('){
      idx += 1
      result = expr(str)
      if(str.charAt(idx) != ')'){
        //failure
      }
      idx += 1
    }else{
      //failure
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