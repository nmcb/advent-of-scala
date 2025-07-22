object Day22 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Spell(val cost: Int, val duration: Int):
    case MagicMissile extends Spell(53, 1)
    case Drain        extends Spell(73, 1)
    case Shield       extends Spell(113, 6)
    case Poison       extends Spell(173, 6)
    case Recharge     extends Spell(229, 5)

  import Spell.*

  val Spells = Set(MagicMissile, Drain, Shield, Poison, Recharge)

  case class Game(
    player: Int,
    mana: Int,
    spent: Int,
    armor: Int,
    boss: Int,
    damage: Int,
    current: Map[Spell,Int]
  ):

    extension (active: (Spell,Int))
      def spell: Spell  = active._1
      def duration: Int = active._2

    def next: Game =
      current.foldLeft(copy(armor = 0))(_ run _)

    infix def run(active: (Spell,Int)): Game =
      val next = effect(active.spell)
      if active.duration == 1 then
        next.copy(current = current.removed(active.spell))
      else
        next.copy(current = current.updated(active.spell, active.duration - 1))

    def effect(spell: Spell): Game =
      spell match
        case MagicMissile => copy(boss = boss - 4)
        case Drain        => copy(player = player + 2, boss = boss - 2)
        case Shield       => copy(armor = 7)
        case Poison       => copy(boss = boss - 3)
        case Recharge     => copy(mana = mana + 101)

    def playerTurn(spell: Spell): Game =
      copy(
        mana    = mana - spell.cost,
        spent   = spent + spell.cost,
        current = current + (spell -> spell.duration)
      )

    def bossTurn: Game =
      copy(player = player - (damage - armor).max(1))

    def hardMode: Game =
      copy(player = player - 1)

  def solve(game: Game, hard: Boolean): Int =

    var result = Int.MaxValue

    def turns(game: Game, playersTurn: Boolean): Unit =
      val before = if hard && playersTurn then game.hardMode else game
      val after  = before.next

      if before.spent >= result || before.player <= 0 then
        ()
      else if after.boss <= 0 then
        result = result min after.spent
      else if !playersTurn then
        turns(after.bossTurn, !playersTurn)
      else Spells
        .diff(after.current.keySet)
        .filter(_.cost <= after.mana)
        .foreach(spell => turns(after.playerTurn(spell), !playersTurn))

    turns(game, true)
    result

  val start: Game = Game(50, 500, 0, 0, 71, 10, Map.empty)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve(start, hard = false)
  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(start, hard = true)
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
