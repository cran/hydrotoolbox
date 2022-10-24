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
#' Reads data from AIC
#'
#' @description Reads excel files provided by the AIC.
#'
#' @param path path to the xlsx file.
#' @param by string with the time step of the series (e.g.: \code{'month', 'day', '6 hour',
#' '3 hour', '1 hour', '15 min' }). By default this argument is set to \code{'day'}. If
#' you set it as \code{'none'}, the function will ignore automatic gap filling.
#' @param out_name optional. String vector with user defined variable(s) column(s) name(s).
#' @param sheet optional. Sheet to read. Either a string (the name of a sheet), or an integer
#'  (the position of the sheet). If neither argument specifies the sheet, defaults to the
#'   first sheet.
#' @param skip optional. Minimum number of rows to skip before reading anything, be it
#' column names or data. Leading empty rows are automatically skipped, so this is a lower bound.
#' @param get_sheet logical indicating whether you want to print available sheet names
#' (\code{TRUE}) in the file or not.
#'
#' @return A data frame with the data inside the xlsx file. Gaps between dates are filled with \code{NA_real_} and duplicated rows are eliminated automatically.
#'
#' @importFrom readxl read_xls excel_sheets
#'
#' @export
#'
#' @examples
#'
#' # This files are provided by AIC under legal agreement only.
#'
read_aic <- function(path,
                     by = 'day',
                     out_name = NULL,
                     sheet = NULL,
                     skip = 12,
                     get_sheet = FALSE){
  #*++++++++++++++++++
  #* conditionals
  #*++++++++++++++++++
  #* path
  check_class(argument = path,
              target = 'character',
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
  #* wondering excel sheet names?
  if(get_sheet == TRUE){
    #* return excel sheets names
    sheet_name <- excel_sheets(path = path)

    return(sheet_name)

  }

  #* read the file
  table_aic <-
    read_xls(path = path, col_types = 'guess',
              sheet = sheet, skip = skip ) %>% # to be sure that I will work with strings
    as.data.frame() # because returns a tibble

  table_aic[ , 1]  <- as.Date( table_aic[ , 1], format = '%d/%m%/Y' )


  #* fill_table
  if(by == 'none'){

    table_fill <- table_aic

  } else {

    table_fill <- fill_table(x = table_aic, by = by)

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
