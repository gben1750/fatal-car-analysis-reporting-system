#' Import traffic accident data
#'
#' Enables the importing of comma delimited text files (CSV) or tab sperated files (TSV) from the US National Highway Traffic
#' Safety Administration's Fatality Analysis Reporting System (FARS). The data provides a statistical summary of fatalities
#' (where a death occured within 30 days of the incident) suffered in motor vehicle accidents in the United States.
#'
#' @param filename A character vector giving the location of the data file to be imported.
#'
#' @details The function uses the the readr package to read and load the csv file identified by the filename parameter. The
#' tbl_def function returns a tibble otherwise execution of the function stops and a warning ' file accident_xxxx.csv.bz2
#' does not exist' is issued (where xxxx is the year of data being imported). The suppressmessages function is used to ignore
#' all simple diagnostic messages.
#'
#' @note The readr and dplyr packages must be installed in the R environment but do not have to be attached.
#'
#' @return A 50 column tibble with each row representing a fatal incident.
#'
#' @examples
#' fars_read('accident_2013.csv.bz2')
#' fars_read('accident_2014.csv.bz2')
#'
#' @export
fars_read <- function(filename) {
  filepath <- system.file("extdata", filename, package = "fatalcaranalysiisreports" , mustWork = TRUE)
  if(!file.exists(filepath))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filepath, progress = FALSE)
  })
  # dplyr::tbl_df(data)
  # Waring: tbl_df()` is deprecated as of dplyr 1.0.0...Please use `tibble::as_tibble()` instead.
  tibble::as_tibble (data)
}
#'
#' Creates filename
#'
#' Generate the name of a file containing traffic fatality statistics provided by the US National Highway Traffic Safety
#'   Administration's Fatality Analysis Reporting System (FARS).
#'
#' @details The function accepts a year argument and uses the sprintf function to return a filename containing accident data
#' for the \code{year} parameter.
#'
#' @param year A value used to represent the year. This can be in either character or numeric format as the function will
#'  try to coerce the value to an integer.
#'
#' @return A filename representing the fatal traffic incident data for a given year. The file will be of the form;
#'  accident_xxxx.csv.bz2 where xxxx is the year of data being imported.
#'
#' @examples
#' make_filename(2013)
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}
#'
#' Generates a list of fatalities
#'
#' The function generates a list summarising the number of fatal traffic accidents recorded in the US National Highway Traffic
#'   Safety Administration's Fatality Analysis Reporting System (FARS). Each file imported (representing data for a specific
#'   year) will be represented as a tibble with column headings of Month and Year.
#'
#' @param years A vector (or matrix) used to represent the years of the data files to be imported.
#'
#' @details The function iterates over each year using lapply to import the files created using the make_filename() function
#'   supplied in this package. The mutute and select functions from the dplyr package are also used to generate the
#'   required output.
#'
#' @note A package permitting the use of pipelines ( %>% ) such as magrittr must be installed and attached.
#'   The dplyr package must be installed in the R environment but does not have to be attached.
#'
#' @return A ist with each element containing the Month and Year for each dataset imported. A warning  message,
#'   "invalid year: xxxx" will be displayed if the file does not exist.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_read_years (2014)
#' fars_read_years(c(2013,2015))
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}
#'
#' Summary by year and month of the number of fatal accidents recorded in the US NHTSA FARS system
#'
#' The function generates a tibble summarising the number of fatal injuries suffered in motor vehicle accidents recorded by the
#' US National Highway Traffic Safety Administration's Fatality Analysis Reporting System (FARS). The tibble shows the number of
#' accidents grouped by month and year.
#'
#' @param years A vector (or matrix) used to represent the years of the data files to be imported.
#'
#' @details The function accepts a \code{years} argument and converts the elements from the list generated from the
#' fars_read_years() function into a tibble which summarises the number of fatalities incurred in motor vehicle
#' accidents by month and year.
#'
#' @note A package permitting the use of pipelines ( %>% ), such as magrittr must be installed and attached.
#'   The dplyr and tidyr packages must be installed in the R environment but do not have to be attached.
#'
#' @return This function returns a tibble summarising the numbers of fatal accidents by month and year.
#'
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @import dplyr
#'
#' @examples
#' fars_summarize_years (2013)
#' fars_summarize_years (c(2013,2014))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'
#' Fatal Incident Plot
#'
#' The function graphically displays the locations of fatal accidents recorded in the US National Highway Traffic
#'   Safety Administration's Fatal Analysis Reporting System (FARS). The function accepts a state number and year as parameters.
#
#' @param year A vector (or matrix) used to represent the years of the data files to be imported.
#' @param state.num State number
#'
#' @details The function uses the following previously described functions in this package;
#'   make_filename(), fars_read. The function accepts a \code{state.num} argument and \code{state.num} argument to plot
#'   incidents for a given year. Execution will halt if the state number does not exist and return the message
#'   invalid STATE number: " x (where x is the \code{state.num} argument). If there are no fatalities in a state the
#'   function returns 'no incidents to plot'.
#'
#' @note The dplyr,map and point packages must also be installed in the R environment but do not have to be attached.
#'
#' @return This function returns a plot depicting the location of fatal incidents (where a death occured within 30 days
#'   of the crash) recorded during a given year for a state.
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state (1,2013)
#'
#' \dontrun{
#' fars_read_years(c(2013,2014))
#' }
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
