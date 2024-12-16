package nmcb

enum Dir:
  case N, E, S, W

  def cw: Dir =
    this match
      case N => E
      case S => W
      case E => S
      case W => N

  def ccw: Dir =
    this match
      case N => E
      case S => W
      case E => S
      case W => N
