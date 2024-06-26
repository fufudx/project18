package project
import analyse._

import io.circe.generic.auto._
import sttp.client3._
import sttp.client3.circe._

import java.io.{File, PrintWriter}
import java.time.LocalDate
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import scala.io.StdIn
import com.github.tototoshi.csv._
import org.jfree.chart.{ChartFactory, ChartPanel}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset

import javax.swing.{JFrame, WindowConstants}
import scala.util.{Failure, Success, Try, Using}

object project extends App {
  // API key for data access
  val apiKey = "e9778964ee0a434a871a1d4fb84536c3"
  // Date formatter for parsing input dates
  val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
  // Fetches time series data from the API
  def fetchData(datasetId: Int, startDate: String, endDate: String): Try[List[TimeSeriesData]] = {
    val startTime = Try(s"${LocalDate.parse(startDate, dateFormatter)}T00:00:00Z")
    val endTime = Try(s"${LocalDate.parse(endDate, dateFormatter)}T23:59:59Z")

    (startTime, endTime) match {
      case (Success(start), Success(end)) =>
        val request = basicRequest
          .get(uri"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=$start&endTime=$end&format=json&pageSize=20000")
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
      case (Failure(startError), _) =>
        Failure(new Exception(s"Error parsing start date: ${startError.getMessage}"))
      case (_, Failure(endError)) =>
        Failure(new Exception(s"Error parsing end date: ${endError.getMessage}"))
    }
  }
  // Ensures the date entered by the user is valid and not in the future
  def getValidDate(prompt: String): String = {
    println(prompt)
    val date = StdIn.readLine()
    try {
      val parsedDate = LocalDate.parse(date, dateFormatter)
      if (parsedDate.isAfter(LocalDate.now())) {
        print("Future dates are not allowed as there's no data available. Please enter a past or current date.")
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
  // Main loop for the command line interface
  def mainLoop(): Unit = {
    println("1. Collect data")
    println("2. Monitor energy data")
    println("3. View of the energy generation")
    println("4. Data process")
    println("5. Alter system")
    println("0. Exit")
    val choice = StdIn.readLine()

    choice match {
      case "1" =>// Collects energy data based on user input
        println("Collect data about:")
        println("1: solar energy")
        println("2: wind energy")
        println("3: hydropower")
        println("0: Exit")
        val subChoice = StdIn.readLine()
        subChoice match {
          case "1" | "2" | "3" =>
            val (datasetId, resourceType) = subChoice match {
              case "1" => (248, "solar")
              case "2" => (181, "wind")
              case "3" => (191, "hydropower")
            }
            val startDate = getValidDate("Enter the start date (DD/MM/YYYY):")
            val endDate = getValidDate("Enter the end date (DD/MM/YYYY):")
            if (LocalDate.parse(startDate, dateFormatter).isBefore(LocalDate.now().plusDays(1))) {
              fetchData(datasetId, startDate, endDate) match {
                case Success(data) if data.nonEmpty =>
                  storeDataAsCSV(data, s"$resourceType-power-$startDate-to-$endDate.csv", resourceType, startDate, endDate)
                case Success(_) => println("There is no data to store.")
                case Failure(exception) => println(s"Error: ${exception.getMessage}")
              }
            }
            mainLoop()  // Recursive call to continue

          case "0" =>
            println("Exiting...")

          case _ =>
            println("Invalid choice, please choose again.")
            mainLoop()
        }
      case "2" =>// Monitors energy data for specific types
        println("Monitor data of:")
        println("1: solar")
        println("2: wind")
        println("3: hydropower")
        println("0: Exit")
        val subChoice = StdIn.readLine()
        subChoice match {
          case "1" | "2" | "3" =>
            val (datasetId, resourceType) = subChoice match {
              case "1" => (248, "solar")
              case "2" => (181, "wind")
              case "3" => (191, "hydropower")
            }
            val date = getValidDate("Enter the date (DD/MM/YYYY):")
            fetchData(datasetId, date, date) match {
              case Success(data) =>
                val totalProduction = data.map(_.value).sum
                println(s"Total production for the day: $totalProduction MW")
                println("Enter a increase percentage (means that the total production now will be->total*increase percentage:")
                val adjustmentFactor = StdIn.readDouble()
                println(s"Data for $resourceType on $date after adjustment is ${totalProduction * adjustmentFactor} MW")
              case Failure(exception) =>
                println(s"Error fetching data: ${exception.getMessage}")
            }
          case "0" =>
            println("Exiting...")
            mainLoop()
          case _ =>
            println("Invalid choice, please choose again.")
        }
        mainLoop()
      case "3" =>//View data as a line chart.
        println("Choose energy type:")
        println("1: solar energy")
        println("2: wind energy")
        println("3: hydropower")
        val energyChoice = StdIn.readLine()
        energyChoice match {
          case "1" => viewStoredData("solar")
          case "2" => viewStoredData("wind")
          case "3" => viewStoredData("hydropower")
          case _ => println("Invalid choice.")
        }
        mainLoop()

      case "4" =>// Processes data through an external analysis tool
        analyse.main(Array.empty)
        mainLoop()

      case "5" =>// Alters system settings or data
        println("Welcome to renewable energy alter system:")
        println("Select the type of power generation data to fetch (solar, hydro, wind):")
        val resourceType = StdIn.readLine().toLowerCase()
        print("Please enter the date you want to analyze (DD/MM/YYYY):")
        val date = getValidDate("")
        val datasetId = resourceType match {
          case "solar" => 248
          case "wind" => 181
          case "hydro" => 191
          case _ =>
            println("Invalid resource type.")
            mainLoop()
            return
        }
        val startDate = date
        val endDate = date
        fetchData(datasetId, startDate, endDate) match {
          case Success(data) if data.exists(_.value < 0) =>
            println("Alert: Detected power generation less than 0:")
            data.filter(_.value < 0).foreach { ts =>
              println(s"Specific time of day: ${ts.startTime}")
            }
          case _ =>
            println("No issues detected with today's production.")
        }
        mainLoop() // Continue after monitoring
      case "0" =>
        println("Exiting program...")  // Print a message indicating that the program is exiting
        System.exit(0)  // Exit the program
      case _ =>
        println("Invalid choice, please choose again.")
        mainLoop()
    }
  }

  def storeDataAsCSV(data: List[TimeSeriesData], outputPath: String, resourceType: String, startDate: String, endDate: String): Unit = {
    // Construct the filename
    val filename = s"${resourceType} Power.csv"
    // Construct the full output path
    val fullOutputPath = outputPath + File.separator + filename
    // Create a file object
    val file = new File(fullOutputPath)
    // Create the directory path
    file.getParentFile.mkdirs()
    // Open the CSV writer
    val writer = CSVWriter.open(file)
    try {
      // Write the header row
      writer.writeRow(List("DatasetId", "startTime", "endTime", "Value"))
      // Iterate through the data and write each row
      data.foreach { ts =>
        writer.writeRow(List(ts.datasetId.toString, ts.startTime, ts.endTime, ts.value.toString))
      }
    } catch {
      case e: Exception =>
        println(s"Failed to write to file: $e")
    } finally {
      writer.close()
    }
    println(s"Data stored in $filename")
  }

  mainLoop()
  // Data model for time series data
  case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
  // Data model for API response
  case class ApiResponse(data: List[TimeSeriesData], pagination: Pagination)
  // Pagination details for API responses
  case class Pagination(total: Int, lastPage: Int, prevPage: Option[Int], nextPage: Option[Int], perPage: Int, currentPage: Int, from: Int, to: Int)


  def viewStoredData(resourceType: String): Unit = {
    // Date formatter for parsing user input
    val dateFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy")
    println(s"Viewing $resourceType energy data:")
    println("Enter the date (DD/MM/YYYY):")
    // Read user input for date
    val dateStr = StdIn.readLine()

    // Parse the input date
    val startDate = LocalDate.parse(dateStr, dateFormatter)
    val endDate = startDate.plusDays(1)
    // Format start and end dates for API query
    val formattedStartDate = startDate.toString + "T00:00:00Z"
    val formattedEndDate = endDate.toString + "T00:00:00Z"

    // Determine dataset ID based on resource type
    val datasetId = resourceType match {
      case "solar" => 248
      case "wind" => 181
      case "hydropower" => 191
      case _ =>
        println("Invalid energy type.")
        return
    }

    val url = uri"https://data.fingrid.fi/api/datasets/$datasetId/data?startTime=$formattedStartDate&endTime=$formattedEndDate&format=json&pageSize=20000"
    val apiKey = "e9778964ee0a434a871a1d4fb84536c3" // Replace with your API key
    // HTTP backend
    implicit val backend = HttpURLConnectionBackend()

    val response = Try {
      basicRequest
        .get(url)
        .header("x-api-key", apiKey)
        .response(asJson[ApiResponse])
        .send()
    }

    response match {
      case Success(res) =>
        res.body match {
          case Right(apiResponse) =>
            // Create dataset for chart
            val dataset = new DefaultCategoryDataset()
            apiResponse.data.foreach { ts =>
              dataset.addValue(ts.value, "Energy", ts.startTime)
            }
            // Create line chart
            val chartTitle = s"$resourceType Power Generation Data"
            val chart = ChartFactory.createLineChart(
              chartTitle,
              "Time",
              "Value",
              dataset,
              PlotOrientation.VERTICAL,
              true,
              true,
              false
            )
            // Display chart in a frame
            val panel = new ChartPanel(chart)
            val frame = new JFrame(chartTitle)
            frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
            frame.add(panel)
            frame.pack()
            frame.setVisible(true)
            // Write data to file
            Using(new PrintWriter(new File("energy_data.txt"))) { writer =>
              apiResponse.data.foreach(ts => writer.println(s"${ts.startTime},${ts.value}"))
            }.getOrElse(println("Failed to write to file"))
          // Handle error response
          case Left(error) =>
            println(s"Error fetching data: $error")
        }
      // Handle request failure
      case Failure(exception) =>
        println(s"Failed to fetch data: ${exception.getMessage}")
    }
  }
}
