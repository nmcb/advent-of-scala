def fac1(n: Int): Int =
  if n == 1 then
    1
  else
    fac1(n - 1) * n

def fac2(n: Int, acc: Int = 1): Int =
  if n == 1 then
    acc
  else
    fac2(n - 1, acc * n)

def fib1(n: Int): Int =
  n match
    case 0 => 1
    case 1 => 1
    case _ => fib1(n - 1) + fib1(n - 2)

def fib2(n: Int, a: Int = 1, b: Int = 1): Int =
  n match
    case 0 => a
    case 1 => a
    case _ => fib2(n - 1, a + b, a)

def f(m: Int, n: Int): Int =
  if m == 0 then
    println(s"[m=$m, n=$n] - exit")
    n + 1
  else if n == 0 then
    println(s"[m=$m, n=$n] - simple recursion")
    f(m - 1, 1)
  else
    println(s"[m=$m, n=$n] - complex recursion")
    f(m - 1, f(m, n - 1))


@main def exercise() =
  //  println(fac1(5))
  //  println(fac2(5))
  //  println(fib1(100))
  //  println(fib2(100))
  println(f(4, 1))