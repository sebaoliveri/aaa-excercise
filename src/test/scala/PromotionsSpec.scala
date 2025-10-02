import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.contain
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PromotionsSpec extends AnyFlatSpec {

  it should "get all combinable promos" in {
    Promotions.allCombinablePromotions(
      allPromotions = Promotions.allPromotions
    ) should contain theSameElementsInOrderAs
      Seq(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5")),
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      )
  }

  it should "find combinable promos" in {
    Promotions.combinablePromotions(
      promotionCode = "P1",
      allPromotions = Promotions.allPromotions
    ) should contain theSameElementsInOrderAs
      Seq(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5"))
      )

    Promotions.combinablePromotions(
      promotionCode = "P3",
      allPromotions = Promotions.allPromotions
    ) should contain theSameElementsInOrderAs
      Seq(
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      )
  }
}
