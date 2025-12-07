import Day21.Outcome.Lost

object Day21 extends App:

  import scala.io.*

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  /** Modeling */

  case class Player(points: Int, damage: Int, armor: Int, gold: Int)

  object Player:
    def equip(points: Int, weapon: Weapon, armor: Option[Armor], rings: Seq[Ring]): Player =
      assert(rings.size <= 2, "max 2 rings")
      assert(rings == rings.distinct, "max one of a kind")
      Player( points = points
            , damage = weapon.damage + rings.map(_.damage).sum
            , armor  = armor.map(_.armor).getOrElse(0) + rings.map(_.armor).sum
            , gold   = weapon.cost + armor.map(_.cost).getOrElse(0) + rings.map(_.cost).sum
            )

  enum Weapon(val cost: Int, val damage: Int):
    case Dagger     extends Weapon(8, 4)
    case ShortSword extends Weapon(10,5)
    case WarHammer  extends Weapon(25, 6)
    case Longsword  extends Weapon(40, 7)
    case GreatAxe   extends Weapon(74, 8)
    
  enum Armor(val cost: Int, val armor: Int):
    case Leather    extends Armor(13,1)
    case ChainMail  extends Armor(31, 2)
    case SplitMail  extends Armor(53, 3)
    case BandedMail extends Armor(75, 4)
    case PlateMail  extends Armor(102, 5)

  enum Ring(val cost: Int, val damage: Int, val armor: Int):
    case Damage1  extends Ring(25, 1, 0)
    case Damage2  extends Ring(50, 2, 0)
    case Damage3  extends Ring(100, 3, 0)
    case Defence1 extends Ring(20, 0, 1)
    case Defence2 extends Ring(40, 0, 2)
    case Defence3 extends Ring(80, 0, 3)

  enum Outcome:
    case Won
    case Lost

  case class Game(player: Player, boss: Player):
    import Outcome.*

    def play: Outcome =
      val newPointsP = player.points - (if boss.damage - player.armor >= 1 then boss.damage - player.armor else 1)
      val newPointsB =   boss.points - (if player.damage - boss.armor >= 1 then player.damage - boss.armor else 1)

      if      newPointsB <= 0 then Won
      else if newPointsP <= 0 then Lost
      else
        val newP = player.copy(points = newPointsP)
        val newB = boss.copy(points = newPointsB)
        Game(newP, newB).play

  /** Input */

  object Game:
    def make(player: Player): Game =
      Game(player, boss = Player(points = 109, damage = 8, armor = 2, gold = -1))

  val equipped: List[Player] =
    for {
      w  <- Weapon.values.toList
      a  <- Armor.values.toList.map(Option(_)) :+ Option.empty[Armor]
      rs <- Ring.values.toList.map(List(_)) ++ Ring.values.toList.combinations(2) ++ List(List.empty[Ring])
      if rs == rs.distinct
    } yield Player.equip(100, w, a, rs)

  /** Part 1 */

  val start1: Long =
    System.currentTimeMillis

  lazy val answer1: Int =
    equipped.filter(Game.make(_).play == Outcome.Won).map(_.gold).min

  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long =
    System.currentTimeMillis

  lazy val answer2: Int =
    equipped.filter(Game.make(_).play == Outcome.Lost).map(_.gold).max

  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
