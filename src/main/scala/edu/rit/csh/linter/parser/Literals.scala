package edu.rit.csh.linter.parser

import edu.rit.csh.linter.language.Literals._
import fastparse.WhitespaceApi
import fastparse.all._
import fastparse.core.Parser
import fastparse.parsers.Combinators.Not

object Literals {

  import Utils._

  private val nonZeroDigit = CharIn('1' to '9')
  private val digit = CharIn('0' to '9')
  private val hex = digit | CharIn('a' to 'f') | CharIn('A' to 'F')
  private val lower = CharIn('a' to 'z')
  val upper = CharIn('A' to 'Z')
  private val letter = lower | upper

  val hexDigit: Parser[Int] = hex.!.map { i => Integer.valueOf(i, 16) }

  val unicodeEscape = P("\\" ~ "u".rep(min=1) ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit).map { case (h1, h2, h3, h4) =>
    val value = h4 * math.pow(16, 0) + h3 * math.pow(16, 1) + h2 * math.pow(16, 2) + h1 * math.pow(16, 3)
    new String(java.lang.Character.toChars(value.toChar))
  }

  // 1.1 Identifiers


  private val notInOp = Seq('\u0020', '\u0009', '\u000D', '\u000A', '$', '|', '_', '(', ')', '[', ']', '{', '}', '`', ''', '"', '.', ';', ',') ++ ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')
  private val idRest = P((letter | digit).rep ~ ("_" ~ op).?).!
  private val inOp = '\u0020' to '\u007F'

  val reservedWords = Set("abstract", "case", "catch", "class", "def", "do",
    "else", "extends", "false", "final", "finally", "for", "forSome", "if",
    "implicit", "import", "lazy", "macro", "match", "new", "null", "object",
    "override", "package", "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "try", "true", "type", "val", "var", "while",
    "with", "yield", "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@")

  val op: Parser[Symbol] = P(CharComb(inOp, notInOp).rep(min = 1)).!.filter(str => !reservedWords.contains(str)).map { case str => Symbol(str) }
  val varId: Parser[Symbol] = P(lower ~ idRest).!.map { case str => Symbol(str) }
  val plainId: Parser[Symbol] = P((upper ~ idRest).!.map { case str => Symbol(str) } | varId | op)

  // 1.2 Newline Characters

  val nl = P("\n")
  val semi = P(";" | nl.rep(1))

  // 1.3 Literals

  val charNoDoubleQuoteOrNewline = P(CharNotIn(Seq('\n', '"')).!)
  val charEscapeSeq = P("\\" ~ ("b" | "t" | "n" | "f" | "r" | "\"" | "'" | "\\")).!.map {
    case "\\b" => "\u0008"
    case "\\t" => "\t"
    case "\\n" => "\n"
    case "\\f" => "\u000c"
    case "\\r" => "\u000d"
    case "\\\"" => "\""
    case "\\'" => "\u0027"
    case "\\\\" => "\\"
  }
  val stringElement = charEscapeSeq | unicodeEscape | charNoDoubleQuoteOrNewline

  val exponentPart = P(("E" | "e") ~ ("+" | "-").? ~ digit.rep(1))
  val floatType = P("f" | "F" | "d" | "D")

  val decimalNumeral: Parser[Int] = ("0" | (nonZeroDigit ~ digit.rep)).!.map { case str => Integer.valueOf(str) }
  val hexNumeral: Parser[Int] = ("0" ~ ("x" | "X") ~ hexDigit.rep(1)).!.map { case str => Integer.valueOf(str.substring(2), 16) }

  val nullLiteral = P("null").map(_ => NullLiteral())
  val symbolLiteral = P("'" ~ plainId.!).map { SymbolLiteral }
  val stringLiteral = P("\"" ~ stringElement.rep.map { lst => StringLiteral(listToString(lst)) } ~ "\"")
  val characterLiteral = P("\'" ~ stringElement.map { str => CharacterLiteral(str.charAt(0)) } ~ "\'")
  val booleanLiteral = P("true".!.map { _ => BooleanLiteral(true) } | "false".!.map { _ => BooleanLiteral(false) })
  val floatingLiteral =
     ( (digit.rep(1) ~ "." ~ digit.rep(1) ~ exponentPart.? ~ floatType.?)
     | ("." ~ digit.rep(1) ~ exponentPart.? ~ floatType.?)
     | (digit.rep(1) ~ exponentPart ~ floatType.?)
     | (digit.rep(1).! ~ exponentPart.!.?  ~ floatType)
     )
  val integerLiteral: Parser[Int] = hexNumeral | decimalNumeral


  val literal: Parser[Literal[_]] =
    (P("-".? ~ floatingLiteral).!.map { case str => FloatingLiteral(java.lang.Double.valueOf(str)) }
    | ("-".!.? ~ integerLiteral).map {
      case (Some(_), i) => IntegerLiteral(-1 * i)
      case (None, i) => IntegerLiteral(i)
    }
    | booleanLiteral | characterLiteral | stringLiteral | symbolLiteral | nullLiteral)

  val id: Parser[Symbol] = P("`" ~ stringLiteral.map { case s => Symbol(s.value) } ~ "`" | plainId)

}
