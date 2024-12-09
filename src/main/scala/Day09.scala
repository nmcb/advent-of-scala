import scala.io.*
import scala.annotation.*

object Day09 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Block(id: Int):
    def isFileBlock: Boolean = this != Block.free
    def isFreeBlock: Boolean = this == Block.free
    def asString: String     = if id == -1 then "." else id.toString

  object Block:
    def free: Block = Block(-1)

  val disk: Vector[Block] =
    @tailrec
    def loop(todo: List[Char], id: Int = 0, result: Vector[Block] = Vector.empty): Vector[Block] =
      todo match
        case Nil =>
          result
        case size :: free :: rest =>
          loop(rest, id + 1, result ++ Vector.fill(size.asDigit)(Block(id)) ++ Vector.fill(free.asDigit)(Block.free))
        case size :: Nil =>
          loop(Nil, id + 1, result ++ Vector.fill(size.asDigit)(Block(id)))
    loop(Source.fromResource(s"input$day.txt").mkString.trim.toList)

  def compact1(disk: Vector[Block]): Vector[Block] =
    @tailrec
    def loop(d: Vector[Block], l: Int, r: Int): Vector[Block] =
      if l >= r then
        d
      else if d(l).isFileBlock then
        loop(d, l + 1, r)
      else if d(r).isFreeBlock then
        loop(d, l, r - 1)
      else
        loop(d.updated(l, d(r)).updated(r, Block.free), l + 1, r - 1)
    loop(disk, 0, disk.size - 1)

  def checksum(disk: Vector[Block]): Long =
    @tailrec
    def loop(d: Vector[Block], position: Long = 0, result: Long = 0): Long =
      if d.isEmpty then
        result
      else if d.head.isFreeBlock then
        loop(d.tail, position + 1, result)
      else
        loop(d.tail, position + 1, result + position * d.head.id)
    loop(disk)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = checksum(compact1(disk))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  case class Chunk(id: Int, size: Int):
    def isFileChunk: Boolean    = id != -1
    def isFreeChunk: Boolean    = id == -1
    def toBlocks: Vector[Block] = Vector.fill(size)(Block(id))

  object Chunk:
    def file(id: Int, size: Int): Chunk = Chunk(id, size)
    def free(size: Int): Chunk          = Chunk(-1, size)

    def convert(disk: Vector[Block]): Vector[Chunk] =
      @tailrec
      def loop(blocks: Vector[Block], result: Vector[Chunk]): Vector[Chunk] =
        if blocks.isEmpty then
          result
        else
          val block = blocks.head
          val last  = result.last
          if block.isFreeBlock then
            if last.isFreeChunk then
              loop(blocks.tail, result.init :+ last.copy(size = last.size + 1))
            else
              loop(blocks.tail, result :+ Chunk.free(1))
          else
            if last.id == block.id then
              loop(blocks.tail, result.init :+ last.copy(size = last.size + 1))
            else
              loop(blocks.tail, result :+ Chunk.file(block.id, 1))

      loop(disk, Vector(Chunk.file(disk.head.id, 0)))

  extension (chunks: Vector[Chunk]) def asString: String =
    chunks.foldLeft("")((a, c) => a + c.toBlocks.map(_.asString).mkString(""))

  def compact2(disk: Vector[Block]): Vector[Block] =
    @tailrec
    def loop(chunks: Vector[Chunk], id: Int): Vector[Chunk] =
      if id <= 0 then
        chunks
      else
        val chunksWithIndex = chunks.zipWithIndex
        val (file, fileIdx) = chunksWithIndex.find((c,_) => c.id == id).head
        chunksWithIndex
          .find((chunk, idx) => idx <= fileIdx & chunk.isFreeChunk & chunk.size >= file.size) match
            case None =>
              loop(chunks, id - 1)
            case Some(free, freeIdx) =>
              val patch =
                if free.size != file.size then
                  Vector(file, free.copy(size = free.size - file.size))
                else
                  Vector(file)

              val update = chunks.patch(fileIdx, Vector(Chunk.free(file.size)), 1).patch(freeIdx, patch, 1)
              loop(update, id - 1)

    val converted = Chunk.convert(disk)
    val start = converted.map(_.id).max
    loop(converted, start).flatMap(_.toBlocks)

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = checksum(compact2(disk))
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
