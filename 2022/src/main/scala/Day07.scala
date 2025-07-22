import scala.io.Source

object Day07 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  sealed trait Line
  case object LsLine                             extends Line
  case class  CdLine(name: String)               extends Line
  case class  FileLine(name: String, size: Long) extends Line
  case class  DirLine(name: String)              extends Line

  def parseLine(l: String): Line =
    l match
      case s"$$ cd $name"                      => CdLine(name)
      case s"$$ ls"                            => LsLine
      case s"$size $name" if size.head.isDigit => FileLine(name, size.toLong)
      case s"dir $name"                        => DirLine(name)

  val input: List[Line] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(parseLine)
      .toList

  type Path = String

  object Path:
    val empty = ""
    val root  = "/"
    val slash = "/"
    val up    = ".."

    def flatten(l: List[String]): String = l.mkString(slash)

  case class File(name: Path, size: Long)

  case class FileSystem(cur: List[String], fs: List[File] = List.empty, ds: List[Path]):

    private val path2Size: Map[String,Long] =
      ds.map(name => name -> sizeOf(name)).toMap

    def sizeOf(path: String): Long =
      fs.filter(_.name.startsWith(path)).map(_.size).sum

    val sizes: List[Long] =
      path2Size.view.values.toList

  object FileSystem:
    val empty = FileSystem(List(Path.empty), List.empty, List(Path.root))

  lazy val fileSystem =
    import Path.*
    input
      .foldLeft(FileSystem.empty)((sys,line) =>
        line match
          case DirLine(name)        => sys.copy(ds = sys.ds :+ flatten(sys.cur :+ name))
          case FileLine(name, size) => sys.copy(fs = sys.fs :+ File(flatten(sys.cur :+ name), size))
          case CdLine(Path.up)      => sys.copy(cur = sys.cur.dropRight(1))
          case CdLine(name)         => sys.copy(cur = sys.cur :+ name)
          case _                    => sys
      )

  val start1: Long =
    System.currentTimeMillis

  lazy val answer1: Long =
    fileSystem.sizes.filter(_ <= 100000L).sum

  println(s"Answer AOC 2022 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  lazy val free: Long =
    70000000L - fileSystem.sizeOf("/")

  lazy val clean: Long =
    30000000L - free

  val start2: Long =
    System.currentTimeMillis

  lazy val answer2 =
    fileSystem.sizes.filter(_ >= clean).min

  println(s"Answer AOC 2022 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
