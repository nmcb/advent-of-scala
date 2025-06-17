import scala.io.Source

object Day19 extends App:


  val day: String =
    getClass.getName.drop(3).init

  val molecules: String =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .filterNot(_.isBlank)
      .filterNot(_.contains("=>"))
      .next

  val replacements: Vector[(String,String)] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .collect:
        case s"$molecule => $replacement" => molecule -> replacement
      .toVector

  /** Part 1 */
  val start1: Long =
    System.currentTimeMillis

  val answer1 =
    var generations = Set.empty[String]
    for from -> to <- replacements do
      for index <- 0 until molecules.length do
        if molecules.slice(index, index + from.length) == from then
          val prefix  = molecules.substring(0, index)
          val postfix = molecules.substring(index + from.length)
          generations += (prefix + to + postfix)
    generations.size

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */
  val start2: Long   = System.currentTimeMillis

  /** Luck > Skill : replacements are already in the right order */
  val answer2 =
    var target = molecules
    var count  = 0
    while target != "e" do
      for from -> to <- replacements do
        if target.contains(to) then
          target  = target.replaceFirst(to, from)
          count = count + 1
    count

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
