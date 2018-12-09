import scala.language.higherKinds

trait Parser[Input,Result] {
  def invokeParser(input:Input):ParserWrapper[Result]
  type ParserWrapper[X]
  def unwrap(result:ParserWrapper[Result]):Result
  final def parseExpr(input:Input): Result = unwrap(invokeParser(input))
}