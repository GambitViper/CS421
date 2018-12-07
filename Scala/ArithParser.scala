class ArithParser {
  var idx: Int = 0
  var str: String = null
  
  def parseExpr(input: String): Int = {
    idx = 0
    str = input
    return expr()
  }
  def expr(): Int = {
    var result: Int = term()
    while(idx < str.length() && (str.charAt(idx) == '+' || str.charAt(idx) == '-')){
      if(str.charAt(idx) == '+'){
        idx += 1
        result = result + term()
      }else if(str.charAt(idx) == '-'){
        idx += 1
        result = result - term()
      }else {
        //failure
      }
    }
    return result
  }
  def term(): Int = {
    var result: Int = factor()
    while(idx < str.length() && (str.charAt(idx) == '*' || str.charAt(idx) == '/')){
      if(str.charAt(idx) == '*'){
        idx += 1
        result = result * factor()
      }else if(str.charAt(idx) == '/'){
        idx += 1
        result = result / factor()
      }else{
        //failure
      }
    }
    return result
  }
  def factor(): Int = {
    var result: Int = 0
    if(Character.isDigit(str.charAt(idx))){
      var num: Int = number()
      result = num
    }else if(str.charAt(idx) == '('){
      idx += 1
      result = expr()
      if(str.charAt(idx) != ')'){
        //failure
      }
      idx += 1
    }else{
      //failure
    }
    return result
  }
  def number(): Int = {
    var result: Int = 0
    while(idx < str.length() && Character.isDigit(str.charAt(idx))){
      result = 10 * result + str.charAt(idx).asDigit
      idx += 1
    }
    return result
  }
}