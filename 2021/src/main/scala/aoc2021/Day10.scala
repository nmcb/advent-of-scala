package aoc2021

import nmcb.*

object Day10 extends AoC:


  def incomplete(s: String): Option[Char] =
    def loop(todo: List[Char], stack: List[Char] = List.empty): Option[Char] =
      if todo.isEmpty && stack.isEmpty then
        None
      else
        todo match
          case Nil                                         => None
          case '(' :: cs                                   => loop(cs, '(' :: stack)
          case '[' :: cs                                   => loop(cs, '[' :: stack)
          case '{' :: cs                                   => loop(cs, '{' :: stack)
          case '<' :: cs                                   => loop(cs, '<' :: stack)
          case ')' :: cs if stack.headOption.contains('(') => loop(cs, stack.tail)
          case ']' :: cs if stack.headOption.contains('[') => loop(cs, stack.tail)
          case '}' :: cs if stack.headOption.contains('{') => loop(cs, stack.tail)
          case '>' :: cs if stack.headOption.contains('<') => loop(cs, stack.tail)
          case c :: _                                      => Some(c)
    loop(s.toList)

  def incompletes(s: String): List[Char] =
    def loop(todo: List[Char], stack: List[Char] = List.empty[Char]): List[Char] =
      if todo.isEmpty && stack.isEmpty then
        Nil
      else
        todo match
          case Nil                                         => stack
          case '(' :: cs                                   => loop(cs, '(' :: stack)
          case '[' :: cs                                   => loop(cs, '[' :: stack)
          case '{' :: cs                                   => loop(cs, '{' :: stack)
          case '<' :: cs                                   => loop(cs, '<' :: stack)
          case ')' :: cs if stack.headOption.contains('(') => loop(cs, stack.tail)
          case ']' :: cs if stack.headOption.contains('[') => loop(cs, stack.tail)
          case '}' :: cs if stack.headOption.contains('{') => loop(cs, stack.tail)
          case '>' :: cs if stack.headOption.contains('<') => loop(cs, stack.tail)
          case      _                                      => Nil
    loop(s.toList)

  lazy val answer1: Long =
    lines
      .flatMap(incomplete)
      .map:
        case ')' => 3L
        case ']' => 57L
        case '}' => 1197L
        case '>' => 25137L
        case c => sys.error(s"invalid char: $c")
      .groupMapReduce(identity)(identity)(_ + _)
      .values
      .sum

  lazy val answer2 =
    val scores =
      lines
        .map(incompletes)
        .filterNot(_.isEmpty)
        .map: stack =>
          stack.foldLeft(0L)((a,c) => (a * 5) + (c match
            case '(' => 1
            case '[' => 2
            case '{' => 3
            case '<' => 4))
        .sorted

    scores(scores.length / 2)
