import scala.io.*

object Day04 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val secret: String =
    "yzbqklnj"

  import java.security.MessageDigest

  def hashMD5(s: String): String =

    val MD5 = MessageDigest.getInstance("MD5")

    def bytesToHex(bytes: Array[Byte]): String =
      val HEX_ARRAY: Array[Char] = "0123456789abcdef".toCharArray
      val hexChars: Array[Char]  = new Array[Char](bytes.length * 2)
      for j <- bytes.indices do
        val v = bytes(j) & 0xFF
        hexChars(j * 2)     = HEX_ARRAY(v >>> 4)
        hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0F)
      String(hexChars)

    bytesToHex(MD5.digest(s.getBytes))
  
  def solve(prefix: String): Int =
    def loop(i: Int = 1): Int =
      if (hashMD5(secret + i.toString).startsWith(prefix)) i else loop(i + 1)
    loop()

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = solve("00000")
  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = solve("000000")
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
