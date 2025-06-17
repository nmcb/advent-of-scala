import java.security.MessageDigest

object Day05 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val id    = "ffykfhsq"

  def solve1(input: String): String =
    val md5    = MessageDigest.getInstance("MD5")
    val buffer = collection.mutable.ArrayBuffer[String]()
    var index  = 0

    while buffer.size < 8 do
      val hash = md5.digest(s"$input$index".getBytes)
      index += 1
      if hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xF0) == 0 then
        buffer += (hash(2) & 0x0F).toHexString

    buffer.mkString

  val start1  = System.currentTimeMillis
  val answer1 = solve1(id).toLowerCase
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(input: String): String =
    val md5    = MessageDigest.getInstance("MD5")
    val buffer = collection.mutable.Map[Int,String]()
    var index  = 0

    while buffer.size < 8 do
      val hash = md5.digest(s"$input$index".getBytes)
      index += 1
      if hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xF0) == 0 && hash(2) < 8 && !buffer.contains(hash(2)) then
        buffer += hash(2).toInt -> ((hash(3) >> 4) & 0x0F).toHexString

    (0 to 7).map(buffer).mkString

  val start2: Long    = System.currentTimeMillis
  val answer2: String = solve2(id).toLowerCase
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")