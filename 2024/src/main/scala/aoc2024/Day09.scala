package aoc2024

import nmcb.*

import scala.annotation.*

object Day09 extends AoC:

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
    loop(input.toList)

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

  lazy val answer1: Long = checksum(compact1(disk))

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
          val chunk = result.last
          if block.isFreeBlock then
            if chunk.isFreeChunk then
              loop(blocks.tail, result.init :+ chunk.copy(size = chunk.size + 1))
            else
              loop(blocks.tail, result :+ Chunk.free(1))
          else
            if chunk.id == block.id then
              loop(blocks.tail, result.init :+ chunk.copy(size = chunk.size + 1))
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
              val next = chunks.patch(fileIdx, Vector(Chunk.free(file.size)), 1)

              val patch =
                if free.size > file.size then
                  Vector(file, free.copy(size = free.size - file.size))
                else
                  Vector(file)

              loop(next.patch(freeIdx, patch, 1), id - 1)

    val converted = Chunk.convert(disk)
    val start = converted.map(_.id).max
    loop(converted, start).flatMap(_.toBlocks)

  lazy val answer2: Long = checksum(compact2(disk))

  /**
   * class Mem():
   *   def __init__(b, pos, len): b.pos = pos; b.len = len
   *   def val(b): return (2*b.pos + b.len-1) * b.len // 2
   *
   * pos, mem = 0, []
   * for len in map(int, input()):
   *   mem += [Mem(pos, len)]
   *   pos += len
   *
   * for used in mem[::-2]:
   *   for free in mem[1::2]:
   *     if (free.pos <= used.pos
   *     and free.len >= used.len):
   *       used.pos  = free.pos
   *       free.pos += used.len
   *       free.len -= used.len
   *
   * print(sum(id*m.val() for id, m in enumerate(mem[::2])))
   */

  lazy val answer2Mutable: Long =
    class Mem(var pos: Int, var len: Int):
      infix def value(id: Int, pointer: Int = pos, result: Long = 0): Long =
        if pointer > pos + len - 1 then result else value(id, pointer + 1, result + pointer * id)

    val (mem: Array[Mem], _) =
      input.map(_.asDigit).foldLeft((Array.empty[Mem], 0)):
        case ((mem, pos), len) =>
          (mem :+ Mem(pos, len), pos + len)
          
    for {
      used <- (input.length - 1 to 0 by -2).map(mem.apply)
      free <- (1 until input.length  by  2).map(mem.apply)
      if free.pos <= used.pos & free.len >= used.len
    } yield {
      used.pos  = free.pos
      free.pos += used.len
      free.len -= used.len
    }

    (0 until input.length by 2)
      .map(mem.apply)
      .zipWithIndex
      .map(_ value _)
      .sum

  def checksumJP(result: Vector[Int]): Long =
    result.zipWithIndex.foldLeft(0L):
      case (acc, (c, i)) => if c == -1 then acc else acc + i * c

  val inputJP: Vector[Char] = lines.head.toVector

  val initFilesLeft: Vector[(Int, Int)] =
    inputJP.zipWithIndex.filter((c, i) => i % 2 == 0).map((c, i) => (c.asDigit, i))
    
  lazy val answer2JP =
    input.zipWithIndex.foldLeft((Vector.empty[Int], initFilesLeft)) {
    case ((acc, filesLeft), (c, i)) if i % 2 == 1 || filesLeft.forall((d, j) => j != i) =>
      @annotation.tailrec
      def fillWithLastEligible(acc: Vector[Int], toFill: Int, filesLeft: Vector[(Int, Int)]): (Vector[Int], Vector[(Int, Int)]) = {
        val rightEligible = filesLeft.findLast((d, j) => d <= toFill && i < j)
        rightEligible match {
          case None => (acc ++ Vector.fill(toFill)(-1), filesLeft)
          case Some((d, j)) =>
            fillWithLastEligible(
              acc ++ Vector.fill(d)(j / 2),
              toFill - d,
              filesLeft.patch(filesLeft.lastIndexOf((d, j)), Nil, 1)
            )
        }
      }

      fillWithLastEligible(acc, c.asDigit, filesLeft)
    case ((acc, filesLeft), (c, i)) if i % 2 == 0 => (acc ++ Vector.fill(c.asDigit)(i / 2), filesLeft)
    case _ => ???
  }

