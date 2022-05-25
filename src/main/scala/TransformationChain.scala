object TransformationChain extends App {
  /**
   *  Дан набор возможных трансформаций: type Transformation[T] = T => Option[T]
   *  Написать функцию преобразования последовательности трансформаций в возможную трансформацию.
   *  Новая трансформация это результат работы всей цепочки трансформаций, которые не вернули None.
   *  Если все вернули None, то общий результат None.
   */

  type Transformation[T] = T => Option[T]

  def transformationChain[T](seq: Seq[Transformation[T]]): Transformation[T] = {
    (t: T) => {
      seq.foldLeft(Option.empty[T]) { (acc, trans) =>
        acc.flatMap { t =>
          trans(t) match {
            case value: Some[T] => value
            case None => Some(t)
          }
        }.orElse(trans(t))
      }
    }
  }

  val firstTrans: Transformation[Int] = t => Some(t + t)
  val secondTrans: Transformation[Int] = _ => None
  val thirdTrans: Transformation[Int] = t => if (t > 4) Some(t * t) else None
  val firstTransChain = transformationChain(Seq(firstTrans, secondTrans, thirdTrans))
  val secondTransChain = transformationChain(Seq(firstTrans, thirdTrans))

  println(firstTransChain(2))
  println(firstTransChain(1))
  println(secondTransChain(1))
}
