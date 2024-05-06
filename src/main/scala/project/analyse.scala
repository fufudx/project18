package project

import io.circe.generic.auto._
import sttp.client3._
import sttp.client3.circe._

import java.time.{LocalDateTime, LocalDate, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.time.temporal.WeekFields
import scala.io.StdIn
import com.github.tototoshi.csv._

import scala.util.{Failure, Success, Try}
import scala.io.Source

import java.time.format.{DateTimeFormatter, DateTimeParseException}


object analyse {
  val apiKey = "e9778964ee0a434a871a1d4fb84536c3"
  val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")

  // Data structure to hold Time Series Data
  case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)

  // Data structure for API response
  case class ApiResponse(data: List[TimeSeriesData], pagination: Pagination)

  // Data structure for pagination info
  case class Pagination(total: Int, lastPage: Int, prevPage: Option[Int], nextPage: Option[Int], perPage: Int, currentPage: Int, from: Int, to: Int)

  // Fetch data from API
  def fetchData(datasetId: Int, startDate: String, endDate: String): Try[List[TimeSeriesData]] = {
    val startTime = s"${LocalDate.parse(startDate, dateFormatter)}T00:00:00Z"
    val endTime = s"${LocalDate.parse(endDate, dateFormatter)}T23:59:59Z"
    val request = basicRequest
      .get(uri"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=$startTime&endTime=$endTime&format=json&pageSize=20000")
      .header("x-api-key", apiKey)
      .response(asJson[ApiResponse])

    implicit val backend = HttpURLConnectionBackend()

    val response = Try(request.send())

    response.flatMap { res =>
      res.body match {
        case Right(apiResponse) if apiResponse.data.nonEmpty => Success(apiResponse.data)
        case Right(_) => Failure(new Exception("No data available for the provided dates. Please enter a different date range."))
        case Left(error) => Failure(new Exception(s"Error fetching data: $error"))
      }
    }
  }

  def getValidDate(prompt: String): String = {
    println(prompt)
    val date = StdIn.readLine()
    try {
      val parsedDate = LocalDate.parse(date, dateFormatter)
      if (parsedDate.isAfter(LocalDate.now())) {
        println("Future dates are not allowed as there's no data available. Please enter a past or current date.")
        getValidDate(prompt)
      } else {
        date
      }
    } catch {
      case e: DateTimeParseException =>
        println("Invalid date format. Please use DD/MM/YYYY.")
        getValidDate(prompt)
    }
  }

  case class PowerData(id: Int, startTime: String, endTime: String, data: Double)

  def calculateMean(data: List[Double]): Double = {
    data.sum / data.length
  }

  def calculateMedian(data: List[Double]): Double = {
    val sortedData = data.sorted
    val middle = data.length / 2
    if (data.length % 2 == 0) {
      (sortedData(middle - 1) + sortedData(middle)) / 2.0
    } else {
      sortedData(middle)
    }
  }

  def calculateMode(data: List[Double]): Double = {
    val groupedData = data.groupBy(identity).view.mapValues(_.size)
    val maxFrequency = groupedData.values.max
    groupedData.filter(_._2 == maxFrequency).keys.head
  }

  def calculateRange(data: List[Double]): Double = {
    val min = data.min
    val max = data.max
    max - min
  }

  def calculateMidrange(data: List[Double]): Double = {
    val min = data.min
    val max = data.max
    (min + max) / 2
  }

  def analyzeData(data: List[PowerData]): Unit = {
    val values = data.map(_.data)
    val mean = calculateMean(values)
    val median = calculateMedian(values)
    val mode = calculateMode(values)
    val range = calculateRange(values)
    val midrange = calculateMidrange(values)

    println(s"Mean: $mean")
    println(s"Median: $median")
    println(s"Mode: $mode")
    println(s"Range: $range")
    println(s"Midrange: $midrange")
    println()
  }

  case class ProductionData(dateTime: LocalDateTime, production: Double)

  def displayProductionData(data: Seq[ProductionData]): Unit = {
    // Combine production data with the same date and hour
    val combinedData = data.groupBy(_.dateTime)
      .mapValues(_.map(_.production).sum)

    val sortedData = combinedData.toList.sortBy { case (dateTime, _) => dateTime }

    val dailyTotals = calculateDailyTotals(data)
    val sortedDailyTotals = dailyTotals.toSeq.sortBy(_._1)

    val weeklyTotals = calculateWeeklyTotals(dailyTotals)
    val sortedWeeklyTotals = weeklyTotals.toSeq.sortBy(_._1)

    val monthlyTotals = calculateMonthlyTotals(dailyTotals)
    val sortedMonthlyTotals = monthlyTotals.toSeq.sortBy(_._1)

    sortedData.foreach { case (dateTime, production) =>
      val hour = dateTime.getHour // Extract hour from the date and time
      println(s"$dateTime: $production kWh")
    }

    println("\nDaily Totals:")
    sortedDailyTotals.foreach { case (date, total) =>
      println(s"$date: $total MW")
    }

    println("\nWeekly Totals:")
    sortedWeeklyTotals.foreach { case (week, total) =>
      println(s"Week $week: $total MW")
    }

    println("\nMonthly Totals:")
    sortedMonthlyTotals.foreach { case (month, total) =>
      println(s"$month: $total MW")
    }
  }

  def calculateDailyTotals(data: Seq[ProductionData]): Map[LocalDate, Double] = {
    data.groupBy(_.dateTime.toLocalDate).view.mapValues(_.map(_.production).sum).toMap
  }

  def calculateWeeklyTotals(dailyTotals: Map[LocalDate, Double]): Map[Int, Double] = {
    dailyTotals.groupBy {
      case (date, _) =>
        val weekFields = WeekFields.of(LocalDate.now().getDayOfWeek, 1)
        date.get(weekFields.weekOfWeekBasedYear())
    }.mapValues(_.map(_._2).sum).toMap
  }

  def calculateMonthlyTotals(dailyTotals: Map[LocalDate, Double]): Map[String, Double] = {
    dailyTotals.groupBy {
      case (date, _) =>
        val yearMonth = date.getYear * 100 + date.getMonthValue
        yearMonth.toString
    }.mapValues(_.map(_._2).sum).toMap
  }

  def main(args: Array[String]): Unit = {
    var exitRequested = false
    while (!exitRequested) {
      println("\nChoose the file to analyze (solar/wind/hydropower/exit):")
      val fileType = scala.io.StdIn.readLine().toLowerCase.trim
      val (datasetId, _) = fileType match {
        case "solar" => (248, "solar")
        case "wind" => (181, "wind")
        case "hydropower" => (191, "hydropower")
        case "exit" =>
          exitRequested = true
          (0, "")
        case _ =>
          println("Invalid option. Please choose again.")
          (0, "")
      }

      if (!exitRequested && datasetId != 0) {
        val startDate = getValidDate("Enter the start date (DD/MM/YYYY):")
        val endDate = getValidDate("Enter the end date (DD/MM/YYYY):")

        val powerData = fetchData(datasetId, startDate, endDate) match {
          case Success(data) => data.map(d => PowerData(0, d.startTime, d.endTime, d.value))
          case Failure(exception) =>
            println(s"Error: ${exception.getMessage}")
            List.empty[PowerData]
        }

        if (powerData.nonEmpty) {
          println("\nData fetched successfully. Now analyzing...")
          analyzeData(powerData)
          val productionData = powerData.map(pd => ProductionData(LocalDateTime.parse(pd.startTime, DateTimeFormatter.ISO_OFFSET_DATE_TIME), pd.data))
          displayProductionData(productionData)
        } else {
          println("\nNo data fetched. Exiting...")
        }


      }
    }
  }
}
