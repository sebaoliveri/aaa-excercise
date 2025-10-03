
case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

object PriceList {

  val cabinPrices = Seq(
    CabinPrice("CA", "M1", 200.00),
    CabinPrice("CA", "M2", 250.00),
    CabinPrice("CA", "S1", 225.00),
    CabinPrice("CA", "S2", 260.00),
    CabinPrice("CB", "M1", 230.00),
    CabinPrice("CB", "M2", 260.00),
    CabinPrice("CB", "S1", 245.00),
    CabinPrice("CB", "S2", 270.00)
  )

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val pricesByRate: Map[String, Seq[CabinPrice]] = prices.groupBy(_.rateCode)
    rates
      .groupBy(_.rateGroup)
      .flatMap { case (rateGroup, groupRates) =>
        bestPricesInGroup(rateGroup, groupRates, pricesByRate)
      }
      .toSeq
      .sortBy(_.price)
  }

  private def bestPricesInGroup(
    rateGroup: String,
    rates: Seq[Rate],
    pricesByRate: Map[String, Seq[CabinPrice]]
  ): Seq[BestGroupPrice] = {
    val cabinPrices = rates.flatMap(rate => pricesByRate.getOrElse(rate.rateCode, Seq.empty))
    cabinPrices
      .groupBy(_.cabinCode)
      .flatMap { case (_, sameCabinPrices) =>
        bestCabinPriceInGroup(sameCabinPrices, rateGroup)
      }
      .toSeq
  }

  private def bestCabinPriceInGroup(prices: Seq[CabinPrice], rateGroup: String) =
    prices
      .minByOption(_.price)
      .map(cabinPrice =>
        BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rateGroup)
      )
}

