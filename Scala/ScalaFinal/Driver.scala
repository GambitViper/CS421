class Driver {
  val parser = new ArithParser()
  checkValid("10", 10)
  checkValid("1*2*3*4", 24)
  checkValid("4-1+2+3", 8)
  checkValid("10*20*30*40", 240000)
  checkValid("20*30*40/10", 2400)
  checkValid("40/10*20*30", 2400)
  checkValid("10*(20*30)*40", 240000)
  checkValid("100-4-3-2", 91)
  checkValid("100-4-(3-2)", 95)
  checkValid("1+2+3+4", 10)
  checkValid("10+20+30+40", 100)
  checkValid("(10)", 10)
  checkValid("(10)+1", 11)
  checkValid("((10+1))", 11)
  checkValid("((10)+1)", 11)
  checkValid("(10+1)", 11)
  checkValid("10+1", 11)
  checkValid("5*(10+1)", 55)
  checkValid("(10+1)*5", 55)
  checkValid("1*5+10", 15)
  checkValid("10+1*5", 15)
  checkValid("(10+1)*(20/4)", 55)
  checkValid("(10+1)*20/4", 55)
  checkValid("10-2*20/4", 0)
  checkValid("-10", -10)
  checkValid("-10+1", -9)
  checkValid("10+-1", 9)
  checkValid("--10", 10)
  checkValid("20/-4", -5)
  checkValid("(20/-4)", -5)

  checkInvalid("")
  checkInvalid("(10")
  checkInvalid("10)")
  checkInvalid("p10")
  checkInvalid("10p")
  checkInvalid("10p20")
  checkInvalid("10++1")
  checkInvalid("((10")
  checkInvalid("+5")
  checkInvalid("*5")
  checkInvalid("/5")
  checkInvalid("5+")
  checkInvalid("5*")
  checkInvalid("5/")

  def checkValid(expr:String, value:Int):Unit = {
    println("Parsing \"" + expr + "\" should return " + value.toString())
    try {
      val result = parser.parseExpr(expr)
      if (!result.equals(value)) {
        println(" - but was " + result)
      }
    } catch {
      case _: Exception => println(" - but threw an exception")
    }
  }

  def checkInvalid(expr:String):Unit = {
    println("Parsing \"" + expr + "\" should throw an exception ")
    try {
      val result = parser.parseExpr(expr)
      println(" - but was " + result)
    } catch {
      case _: Exception => 
    }
  }
}