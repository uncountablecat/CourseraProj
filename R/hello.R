#' Read data file and return it
#'
#' This function reads the data file 'filename' and return
#' it as a data.frame.
#'
#' @param filename the file name of the data file
#'
#' @return a data.frame.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create a file name containing the year
#'
#' This function creates a string containing the provided year, which will be used as the file name
#'
#' @param year year name
#'
#' @return  a character string
#'
#' @export
#'
#' @examples make_filename(2013)
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read the data with a specified year
#'
#' This function will first create a file name using make_filename containing the
#' given years, and then read data from those files. It will return the data with the specified years.
#' If those years cannot be found or are not valid, it raise an error.
#'
#' @param years years
#'
#' @return data with specified years and months. NULL if error is raised
#'
#' @importFrom dplyr mutate select
#'
#' @export
#'
#' @examples fars_read_years(2013)
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

#' Summarize data by years
#'
#' This function will read data from the file name that's created by fars_read_years with the provided years.
#' After that, it will summarize the data by the years
#'
#' @param years years
#'
#' @return summarized data by years and count
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export
#'
#' @examples fars_summarize_years(2013)
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' plot a map of the data in states
#'
#' This function plot a map of data in given states in the given year
#'
#' @param state.num the number of the state
#' @param year year we want to plot
#'
#' @return nothing but a plot will be generated
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export
#'
#' @examples fars_map_state(1, 2013)
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
