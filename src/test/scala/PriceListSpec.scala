import PriceList.cabinPrices
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.{be, contain, convertToAnyMustWrapper}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class PriceListSpec extends AnyFlatSpec {

  it should "provide no prices when no rates are specified" in {
    PriceList.getBestGroupPrices(
      rates = Nil,
      prices = cabinPrices
    ).isEmpty shouldBe true
  }

  it should "provide no prices when there are no prices for the given rates" in {
    PriceList.getBestGroupPrices(
      rates = Seq(
        Rate("L1", "Loyalty"),
        Rate("L2", "Loyalty"),
      ),
      prices = cabinPrices
    ).isEmpty shouldBe true
  }

  it should "provide best prices for each rate group" in {
    PriceList.getBestGroupPrices(
      rates = Seq(
        Rate("M1", "Military"),
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
      ),
      prices = cabinPrices
    ) should contain theSameElementsInOrderAs
      Seq(
        BestGroupPrice("CA", "M1", 200.00, "Military"),
        BestGroupPrice("CA", "S1", 225.00, "Senior"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S1", 245.00, "Senior")
      )
  }
}
