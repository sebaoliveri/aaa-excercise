import PriceList.cabinPrices

object BestGroupPriceApp {

  def main(args: Array[String]): Unit = {
    PriceList.getBestGroupPrices(
      rates = Seq(
        Rate("M1", "Military"),
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
      ),
      prices = cabinPrices
    ).foreach(println)
  }
}
