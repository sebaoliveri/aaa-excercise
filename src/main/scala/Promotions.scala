
case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

object Promotions {

  val allPromotions = Seq(
    Promotion("P1", Seq("P3")), // P1 is not combinable with P3
    Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
    Promotion("P3", Seq("P1")), // P3 is not combinable with P1
    Promotion("P4", Seq("P2")), // P4 is not combinable with P2
    Promotion("P5", Seq("P2")) // P5 is not combinable with P2
  )

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    def collect(current: Seq[Promotion], remaining: Seq[Promotion]): Seq[Seq[String]] =
      if (remaining.isEmpty) Seq(current.map(_.code).sorted)
      else
        remaining.flatMap { promotion =>
          val newRemaining = remaining.filter { aPromotion =>
            aPromotion.code != promotion.code &&
              !promotion.notCombinableWith.contains(aPromotion.code)
          }
          collect(current :+ promotion, newRemaining)
        }

    val collected = collect(Nil, allPromotions).distinct
    collected
      .filterNot { combo => collected.exists(other => other != combo && combo.toSet.subsetOf(other.toSet)) }
      .map(PromotionCombo)
  }

  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] =
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))
}
