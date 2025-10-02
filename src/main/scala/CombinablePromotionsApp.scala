
object CombinablePromotionsApp {

  def main(args: Array[String]): Unit =
    Promotions.allCombinablePromotions(
      allPromotions = Promotions.allPromotions
    ).foreach(println)
}
