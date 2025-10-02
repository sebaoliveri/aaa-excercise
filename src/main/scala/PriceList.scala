
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

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] =
    rates
      .groupBy(_.rateGroup)
      .flatMap { case (rateGroup, rates) => bestPricesInGroup(rateGroup, rates, prices) }
      .toSeq
      .sortBy(_.price)

  private def bestPricesInGroup(rateGroup: String, rates: Seq[Rate], prices: Seq[CabinPrice]) =
    rates
      .flatMap(rate => prices.filter(_.rateCode == rate.rateCode))
      .groupBy(_.cabinCode)
      .flatMap { case (_, cabinPrices) =>  bestCabinPriceInGroup(cabinPrices, rateGroup) }

  private def bestCabinPriceInGroup(prices: Seq[CabinPrice], rateGroup: String) =
    prices
      .minByOption(_.price)
      .map(cabinPrice => BestGroupPrice(cabinPrice.cabinCode, cabinPrice.rateCode, cabinPrice.price, rateGroup))
}
