# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#
#' Reads data provided by MNEMOS software (SNIH - Argentina)
#'
#' @description Reads xlsx files generated with MNEMOS software.
#'
#' @param path path to the xlsx file.
#' @param by string with the time step of the series (e.g.: \code{'month', 'day', '6 hour',
#'  '3 hour', '1 hour', '15 min' }). If you set it as \code{'none'}, the function will ignore
#'  automatic gap filling.
#' @param out_name optional. String vector with user defined variable(s) column(s) name(s).
#' @param sheet optional. Sheet to read. Either a string (the name of a sheet), or an integer
#'  (the position of the sheet). If neither argument specifies the sheet, defaults to the
#'  first sheet.
#' @param skip optional. Minimum number of rows to skip before reading anything, be it column
#'  names or data. Leading empty rows are automatically skipped, so this is a lower bound.
#' @param get_sheet logical indicating whether you want to print available variables (\code{TRUE})
#'  in every file sheet or not.
#'
#' @return A data frame with the data inside the specified sheet. Gaps between dates are
#'  filled with \code{NA_real_} and duplicated rows are eliminated automatically. In case
#'  you set \code{get_sheet = TRUE} the function will return a \code{list} with the variables
#'  inside each sheet.
#'
#' @importFrom readxl read_xlsx excel_sheets
#'
#' @export
#'
#'
#' @examples
#'
#' # list mnemos files
#' list.files( system.file('extdata', package = 'hydrotoolbox'), pattern = 'mnemos' )
#'
#' # set path
#' path <- system.file('extdata', 'mnemos_guido.xlsx',  package = 'hydrotoolbox')
#'
#' # we can see which variables are inside the sheet's file
#' read_mnemos(path = path, get_sheet = TRUE)
#'
#' # now we want to read the maximum temperature
#' tmax_guido <- read_mnemos(path = path, by = 'day',
#'                           out_name = 'tmax(ºC)', sheet = '11413-016')
#'
#'
#'
read_mnemos <- function(path,
                        by = 'none',
                        out_name = NULL,
                        sheet = NULL,
                        skip = 3,
                        get_sheet = FALSE){
  #*++++++++++++++++++
  #* conditionals
  #*++++++++++++++++++
  #* path
  check_class(argument = path,
              target = 'character',
              arg_name = 'path')

  check_length(argument = path,
               max_allow = 1,
               arg_name = 'path')

  #* by
  check_class(argument = by,
              target = 'character',
              arg_name = 'by')

  check_length(argument = by,
               max_allow = 1,
               arg_name = 'by')

  #* out_name
  check_class(argument = out_name,
              target = 'character',
              arg_name = 'out_name')

  #* get_sheet
  check_class(argument = get_sheet,
              target = 'logical',
              arg_name = 'get_sheet')

  check_string(argument = as.character(get_sheet),
               target = c('TRUE', 'FALSE', 'T', 'F'),
               arg_name = 'get_sheet' )

  check_length(argument = get_sheet,
               max_allow = 1,
               arg_name = 'get_sheet')


  #*++++++++++++++++++
  #* function
  #*++++++++++++++++++
  #* wondering excel sheet names and variables inside?
  if(get_sheet == TRUE){
    #* return excel sheets names and variables
    sheet_name <- excel_sheets(path = path)

    # get list
    out <- lapply(X = sheet_name,
                  FUN = function(x){
                   table_tibble <- read_xlsx(path = path, sheet = x, range = 'A1:A2')
                   return(
                     paste0( colnames(table_tibble), ' => ', table_tibble[1, 1] )
                   )
                  }
    )

    names(out) <- sheet_name

    return(out)

  } else{
    #* read the file
    table_mnemos <-
      read_xlsx(path = path,
               sheet = sheet, skip = skip ) %>%
      as.data.frame() # because returns a tibble

    #* fit data to fill_table requirements
    hour_flag <- length( grep(pattern = 'hour', x = by) )
    min_flag  <- length( grep(pattern = 'min', x = by) )

      # if necessary coerce as.Date
    if( hour_flag == 0 & min_flag == 0 & by != 'none'){

      table_mnemos[ , 1] <- as.Date( table_mnemos[ , 1]  )

    }

    #* fill_table
    if(by == 'none'){

      table_fill <- table_mnemos

    } else {

      table_fill <- fill_table(x = table_mnemos[ , 1:2], by = by) # to avoid non-numeric issues with calificador column

    }

    #* set out_names
    if( !is.null(out_name) ){

      check_length(argument = c('date', out_name),
                   max_allow = ncol(table_fill),
                   arg_name = 'out_name')

      colnames(table_fill) <- c('date', out_name)

    }

    #* return table
    return(table_fill)

  }



}
