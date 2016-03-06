package edu.rit.csh.linter.parser

import fastparse.Utils.CharBitSet
import fastparse.core.Mutable.{Failure, Success}
import fastparse.core.{Parser, ParseCtx}
import fastparse.parsers.Terminals.Fail


object Utils {

  case class CharNotIn(chars: Seq[Char]) extends Parser[Unit] {
    private[this] val uberSet = CharBitSet(chars)
    def parseRec(cfg: ParseCtx, index: Int) = {
      val input = cfg.input
      if (index >= input.length) fail(cfg.failure, index)
      else if (uberSet(input(index))) fail(cfg.failure, index)
      else success(cfg.success, (), index + 1, Nil, false)
    }
  }

  case class CharComb(in: Seq[Char], notIn: Seq[Char]) extends Parser[Unit] {
    private[this] val inSet = CharBitSet(in)
    private[this] val outSet = CharBitSet(notIn)

    def parseRec(cfg: ParseCtx, index: Int) = {
      val input = cfg.input
      if (index >= input.length) fail(cfg.failure, index)
      else if (inSet(input(index)) && !outSet(input(index))) success(cfg.success, (), index + 1, Nil, false)
      else fail(cfg.failure, index)
    }
  }

  def listToString[T](input: Seq[T]): String = {
    val builder = new StringBuilder()
    var lst = input
    while (lst.nonEmpty) {
      builder.append(lst.head)
      lst = lst.tail
    }
    builder.toString
  }

}
