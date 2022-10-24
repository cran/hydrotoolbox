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
#' Reads data from Departamento General de Irrigación - Hydrological Division
#' (DGI - Mendoza - Argentina)
#'
#' @description Reads excel files provided by the DGI (Hydrological Division).
#'
#' @param path path to the xlsx file.
#' @param by string with the time step of the series (e.g.: \code{'month', 'day', '6 hour',
#'  '3 hour', '1 hour', '15 min' }). By default this argument is set to \code{'day'}.
#'   If you set it as \code{'none'}, the function will ignore automatic gap filling.
#' @param out_name optional. String vector with user defined variable(s) column(s) name(s).
#' @param sheet optional. Sheet to read. Either a string (the name of a sheet), or an integer
#'  (the position of the sheet). If neither argument specifies the sheet, defaults to the first sheet.
#' @param get_sheet logical indicating whether you want to print available sheet names (\code{TRUE})
#'  in the file or not.
#'
#' @return A data frame with the data inside the xlsx file. Gaps between dates are filled
#'  with \code{NA_real_} and duplicated rows are eliminated automatically.
#'
#' @importFrom readxl read_xlsx
#'
#' @export
#'
#' @examples
#'
#' # set path to file
#' path_file <- system.file('extdata', 'dgi_toscas.xlsx',
#'              package = 'hydrotoolbox')
#'
#' # because dgi files has multiple sheets we take a look
#' # on them
#' read_dgi(path = path_file, get_sheet = TRUE)
#'
#' # read swe with default column names
#' head( read_dgi(path = path_file, sheet = 'swe') )
#'
#' # assign name
#' head( read_dgi(path = path_file, sheet = 'swe', out_name = 'swe(mm)') )
#'
#' # now read relative humidity
#' head( read_dgi(path = path_file, sheet = 'hr', out_name = 'rh(%)') )
#'
#'
read_dgi <- function(path, by = 'day', out_name = NULL,
                     sheet = NULL, get_sheet = FALSE){
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
  table_dgi <-
    read_xlsx(path = path, col_types = c('text', 'numeric'),
              sheet = sheet ) %>% # to be sure that I will work with strings
    as.data.frame() # because returns a tibble

  table_dgi[ , 1] <- as.Date( table_dgi[ , 1] )
  #table_dgi[ , 2] <- as.numeric( table_dgi[ , 2] )

  #* fill_table
  if(by == 'none'){

    table_fill <- table_dgi

  } else {

    table_fill <- fill_table(x = table_dgi, by = by)

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
