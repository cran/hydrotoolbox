# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Load native data files automatically
#'
#' @description The method allows you to automatically load your native data
#' inside the \code{hydromet_station} slots.
#'
#' @param obj a valid \code{hydromet_station} class object.
#' @param path string vector with the path(s) to the \code{file_name} argument.
#' If you set a single string it will be recycled for all the files.
#' @param file_name string vector with the native file(s) name(s).
#' @param slot_name string vector with the slot(s) where to set the file(s) or sheet(s).
#' @param by string vector with the time step of the series (e.g.: 'month',
#' day', '6 hour', '3 hour', '1 hour', '15 min' ). If you set it as \code{"none"},
#' the function will ignore automatic gap filling. If you set a single string,
#' it will be recycled for all the files.
#' @param out_name optional. A list containing string vectors with user
#' defined variable(s) column(s) name(s). The list length should be
#' equal to the \code{slot_name} length.
#' @param sheet Sheet to read (only excel files). Either a string vector
#' (the name of a sheet) or an integer vector (the position of the sheet).
#' This argument just make sense for excel files.
#' @param FUN function name for reading the data (e.g.: \code{read_csv()}). The method
#' will always use the \code{path + file} as first argument(s) to \code{FUN}.
#' @param ... \code{FUN} arguments to pass.
#'
#'
#' @return A \code{hydromet_station} object with the required data loaded inside.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # you can download the data from:
#' # https://gitlab.com/ezetoum27/hydrotoolbox/-/tree/master/my_data
#'
#' # set the data path
#' my_path <- "./home/my_folder/my_data"
#'
#'#///////////////////////////////
#'# Rectangular data
#'# txt, csv, csv2 and others.
#'# See readr package.
#'#///////////////////////////////
#'
#'library(readr)
#'#* Case 1: single file - many numeric variables
#'
#'hm_create() %>%
#' hm_build_generic(path = my_path,
#'                  file_name = "ianigla_cuevas.csv",
#'                  slot_name = c("tair", "rh", "patm",
#'                               "precip", "wspd", "wdir",
#'                               "kin", "hsnow", "tsoil"),
#'                  by = c("hour"),
#'                  FUN = read_csv,
#'                  col_select = !Est & !YJday & !hh.mm.ss & !bat.Volts
#'                 ) %>%
#'  hm_show()
#'
#' #* Case 2: multiple files (one per observation)
#'hm_create() %>%
#'  hm_build_generic(path = my_path,
#'                   file_name = c("h_relativa_cuevas.csv",
#'                                 "p_atm_cuevas.csv",
#'                                 "precip_total_cuevas.csv",
#'                                 "temp_aire_cuevas.csv",
#'                                 "vel_viento_cuevas.csv"),
#'                   slot_name = c("rh", "patm", "precip",
#'                                 "tair", "wspd"),
#'                   by = c("hour", "45 min", "30 min", "1 hour", "15 min"),
#'                   FUN = read_csv  ) %>%
#'  hm_show()
#'
#'#///////////////////////////////
#'# Excel files
#'# Recommended package => readxl
#'#///////////////////////////////
#'
#'library(readxl)
#'
#'
#'#* Case 1: single file - one sheet - many numeric variables
#'
#'hm_create() %>%
#'  hm_build_generic(path = my_path,
#'                   file_name = "mnemos_guido.xlsx",
#'                   slot_name = c("qd"),
#'                   by = c("day"),
#'                   FUN = read_excel,
#'                   sheet = 1L,
#'                   skip = 3,
#'                   out_name = list("q_m3/s")
#'  ) %>% hm_show()
#'
#'
#'
#'#* Case 2: single file - multiple sheets (one per variable)
#'
#'hm_create() %>%
#'  hm_build_generic(path = my_path,
#'                   file_name = "mnemos_guido.xlsx",
#'                   slot_name = c("qd", "evap", "tair",
#'                                 "tmax", "tmin"),
#'                   by = c(q = "day", evap =  "day", tair = "6 hour",
#'                          tmax = "day", tmin = "day"),
#'                   FUN = read_excel,
#'                   sheet = c(1L:5L),
#'                   skip = 3,
#'                   out_name = list( c("q_m3/s", "flag"),
#'                                    c("evap_mm", "flag"),
#'                                    c("tair", "flag"),
#'                                    c("tmax", "flag"),
#'                                    c("tmin", "flag")
#'                   )
#'  ) %>%
#'  hm_show()
#'
#'#* Case 3: multiple files - one sheet per file
#'
#'hm_create() %>%
#'   hm_build_generic(path = my_path,
#'                    file_name = c("discharge_daily.xlsx",
#'                                  "air_teperature_subdaily.xlsx"),
#'                    slot_name = c("qd", "tair"),
#'                    by = c(q = "day", tair = "6 hour"),
#'                    FUN = read_excel,
#'                    sheet = c(1L, 1L),
#'                    skip = 3,
#'                    out_name = list( c("q_m3/s", "flag"),
#'                                     c("tair", "flag"))
#'   ) %>%
#'   hm_show()
#'
#'}
#'
setGeneric(name = 'hm_build_generic',
           def = function(obj,
                          path,
                          file_name,
                          slot_name,
                          by = "none",
                          out_name = NULL,
                          sheet = NULL,
                          FUN, ...)
           {
             standardGeneric('hm_build_generic')
           })

#' @describeIn hm_build_generic build method for hydromet station object
## station
setMethod(f = 'hm_build_generic',
          signature = 'hydromet_station',
          definition = function(obj,
                                path,
                                file_name,
                                slot_name,
                                by = "none",
                                out_name = NULL,
                                sheet = NULL,
                                FUN, ...){

            #*////////////////////////
            #* conditionals
            #*////////////////////////

            #* obj
            check_class(argument = obj,
                        target = 'hydromet_station',
                        arg_name = 'obj')

            #* file_name
            check_class(argument = file_name,
                        target = 'character',
                        arg_name = 'file_name')

            #* path (invert the order because length issue)
            check_class(argument = path,
                        target = 'character',
                        arg_name = 'path')

            check_length(argument = path,
                         max_allow = length(file_name),
                         arg_name = 'path')

            #* check the file(s) existence
            check_string(argument = file_name,
                         target = list.files(path = path),
                         arg_name = 'file_name')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = setdiff(
                           x = slotNames("hydromet_station"),
                           y = slotNames("hydromet")
                         ),
                         arg_name = 'slot_name')

            #* by
            check_class(argument = by,
                        target = 'character',
                        arg_name = 'by')

            #* out_name (now is a list - for multiple files or sheets)
            if( !is.null(out_name) ){

              check_class(argument = out_name,
                          target = "list",
                          arg_name = "out_name")

              #* check that every argument inside the list
              #* is a character
              n_list <- length(out_name)

              for(i in 1:n_list){
                check_class(argument = out_name[[i]],
                            target = "character",
                            arg_name = paste0( out_name[[i]]) )
              }

              #* check that every slot has an out_name
              check_cross(ref_arg = slot_name,
                          eval_arg = out_name,
                          arg_names = c("slot_name", "out_name"))




            }

            #* sheet
            if( !is.null(sheet) ){

              check_class(argument = sheet,
                          target = c("character", "integer"),
                          arg_name = "sheet")

              check_cross(ref_arg = slot_name,
                          eval_arg = sheet,
                          arg_names = c("slot_name", "sheet"))

            }

            #*////////////////////////
            #* function
            #*////////////////////////

            n_files    <- length(file_name)
            excel_file <- ifelse(is.null(sheet), FALSE, TRUE)
            my_station <- hm_create()


            if( excel_file == FALSE ){
              #*////////////////////////
              #* non-excel files
              #*////////////////////////
              #* note: csv, csv2, txt, delim, lsv, fwf
              #* and so on...
              if(n_files == 1){
                #* case 1: one file - many variables

                full_path <- paste0(path, "/", file_name)

                #1 load table
                tb_raw <- FUN(full_path, ...)

                #2 variable distribution
                n_slots <- length(slot_name)

                for(i in 1:n_slots){
                  tb_aux <- tb_raw[ , c(1, i+1)]

                  # assign col names
                  if( !is.null(out_name) ){
                    colnames(tb_aux) <- c("date", out_name[[i]])
                  }

                  # fill?
                  if( by != "none" ){

                    check_length(argument = by,
                                 max_allow = 1,
                                 arg_name = "by")

                    tb_aux <-
                      tb_aux %>%
                      fill_table(by = by)
                  }

                  out_txt <- paste0("hm_set(obj = my_station,",
                                    slot_name[i], '=', "tb_aux", ")")

                  my_station <- eval( parse(text = out_txt) )



                  rm(tb_aux, out_txt)


                }

                # return object
                validObject(object = my_station)
                return(my_station)


              } else if(n_files  > 1){
                #* case 2: multiple file (one per variable)

                if( !is.null(by) ){
                  check_length(argument = by,
                               max_allow = n_files,
                               arg_name = "by")

                  if( length(by) != n_files ){
                    # recycle
                    message("Recycling the first element in by argument...")
                    by <- rep(by[1], n_files)
                  }

                }

                for(i in 1:n_files){

                  full_path <- paste0(path, "/", file_name[i])

                  #1 load table
                  tb_raw <- FUN(full_path, ...) # this is the table

                  # fill?
                  if( by[i] != "none" ){
                    # con la segunda condición me aseguro
                    # que con el string vacío no rellene.

                    tb_raw <-
                      tb_raw %>%
                      fill_table(by = by[i])
                  }

                  #2 variable distribution
                    # assign col names
                  if( !is.null(out_name) ){
                    colnames(tb_raw) <- c("date", out_name[[i]])
                  }

                    # set table in slot
                  out_txt <- paste0("hm_set(obj = my_station,",
                                    slot_name[i], '=', "tb_raw", ")")

                  my_station <- eval( parse(text = out_txt) )


                  rm(full_path, tb_raw, out_txt)

                }

                # return object
                validObject(object = my_station)
                return(my_station)

                }




            } else if(excel_file == TRUE){
              #*////////////////////////
              #* excel files
              #*////////////////////////
              n_sheets <- length(sheet)

              if( n_files == 1){

                if(n_sheets == 1){
                  #* case 1 : one file - one sheet - all variables

                  full_path <- paste0(path, "/", file_name)

                  #1 load table
                  tb_raw <- FUN(full_path, sheet = sheet, ...)

                  #2 variable distribution
                  n_slots <- length(slot_name)

                  for(i in 1:n_slots){
                    tb_aux <- tb_raw[ , c(1, i+1)]

                    # assign col names
                    if( !is.null(out_name) ){
                      colnames(tb_aux) <- c("date", out_name[[i]])
                    }

                    # fill?
                    if( by != "none" ){

                      check_length(argument = by,
                                   max_allow = 1,
                                   arg_name = "by")

                      tb_aux <-
                        tb_aux %>%
                        fill_table(by = by)
                    }

                    out_txt <- paste0("hm_set(obj = my_station,",
                                      slot_name[i], '=', "tb_aux", ")")

                    my_station <- eval( parse(text = out_txt) )



                    rm(tb_aux, out_txt)


                  }

                  # return object
                  validObject(object = my_station)
                  return(my_station)

                } else {
                  #* case 2 : one file - n sheets (one per variable)

                  full_path <- paste0(path, "/", file_name)

                  #2 variable distribution
                  n_slots <- length(slot_name)

                  check_cross(ref_arg = slot_name,
                              eval_arg = sheet,
                              arg_names = "sheet")

                  if( !is.null(by) ){
                    check_length(argument = by,
                                 max_allow = n_slots,
                                 arg_name = "by")

                    if( length(by) != n_slots ){
                      # recycle
                      message("Recycling the first element in by argument...")
                      by <- rep(by[1], n_sheets)
                    }

                  }

                  for(i in 1:n_slots){

                    #1 load table
                    tb_raw <- FUN(full_path, sheet = sheet[i],
                                  ...) # this is the table

                    # fill?
                    if( by[i] != "none" ){

                      tb_raw <-
                        tb_raw %>%
                        fill_table(by = by[i])
                    }

                    #2 variable distribution
                    # assign col names
                    if( !is.null(out_name) ){
                      colnames(tb_raw) <- c("date", out_name[[i]])
                    }

                    # set table in slot
                    out_txt <- paste0("hm_set(obj = my_station,",
                                      slot_name[i], '=', "tb_raw", ")")

                    my_station <- eval( parse(text = out_txt) )


                    rm(tb_raw, out_txt)

                  }

                  # return object
                  validObject(object = my_station)
                  return(my_station)





                }





              } else {

                #* case 3 : multiple files - one sheet - one variable

                #2 variable distribution
                n_slots <- length(slot_name)

                check_cross(ref_arg = slot_name,
                            eval_arg = sheet,
                            arg_names = c("slot_name", "sheet")
                            )

                check_cross(ref_arg = file_name,
                            eval_arg = slot_name,
                            arg_names = c("file_name", "slot_name")
                            )

                full_path <- paste0(path, "/", file_name)

                if( !is.null(by) ){
                  check_length(argument = by,
                               max_allow = n_slots,
                               arg_name = "by")

                  if( length(by) != n_slots ){
                    # recycle
                    message("Recycling the first element in by argument...")
                    by <- rep(by[1], n_files)
                  }

                }

                for(i in 1:n_slots){

                  #1 load table
                  tb_raw <- FUN(full_path[i], sheet = sheet[i],
                                ...) # this is the table

                  # fill?
                  if( by[i] != "none" ){

                    tb_raw <-
                      tb_raw %>%
                      fill_table(by = by[i])
                  }

                  #2 variable distribution
                  # assign col names
                  if( !is.null(out_name) ){
                    colnames(tb_raw) <- c("date", out_name[[i]])
                  }

                  # set table in slot
                  out_txt <- paste0("hm_set(obj = my_station,",
                                    slot_name[i], '=', "tb_raw", ")")

                  my_station <- eval( parse(text = out_txt) )


                  rm(tb_raw, out_txt)

                }

                # return object
                validObject(object = my_station)
                return(my_station)


              }


            }


          } # end definition
          )# end method
