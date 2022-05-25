object CompressSeq extends App {
  /**
   *  Сжать последовательность целых чисел
   *  Seq(1, 2, 2, 3, 4, 3, 3, 3) => Seq((1, 1), (2, 2), (3, 1), (4, 1), (3, 3))
   *  Ответ выдать в виде Seq[(Int, Int)] (число из последовательности и число последовательных повторений)
   */

  def compressSeq(seq: Seq[Int]): Seq[(Int, Int)] = {
    seq.foldLeft(Seq.empty[(Int, Int)]) {
      case (Nil, el) => Seq((el, 1))
      case ((seqEl, count) :: tail, el) if seqEl == el =>
        (el, count + 1) +: tail
      case (acc, el) =>
        (el, 1) +: acc
    }
  }

  val seq: Seq[Int] = Seq(1, 2, 2, 3, 4, 3, 3, 3)
  println(s"Initial sequence: $seq")
  println(s"Compressed sequence: ${compressSeq(seq).reverse}")

  /**
   * Восстановить исходную последовательность из сжатой
   */

  def flattenSeq(seq: Seq[(Int, Int)]): Seq[Int] = {
    seq.flatMap {
      case (el, count) => Seq.fill(count)(el)
    }
  }

  println(s"Flattened sequence: ${flattenSeq(compressSeq(seq).reverse)}")

  // test
  assert(compressSeq(seq).reverse == Seq((1, 1), (2, 2), (3, 1), (4, 1), (3, 3)))
  assert(flattenSeq(compressSeq(seq).reverse) == Seq(1, 2, 2, 3, 4, 3, 3, 3))
}
