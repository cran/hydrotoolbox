# **********************************************************
# Author       : Ezequiel Toum
# Licence      : GPL V3
# Institution  : IANIGLA-CONICET
# e-mail       : etoum@mendoza-conicet.gob.ar
# **********************************************************
# hydrotoolbox package is distributed in the hope that it
# will be useful but WITHOUT ANY WARRANTY.
# **********************************************************
#' Easy access to see your data
#'
#' @description This method shows the 'head' or 'tail' of a specific slot.
#'
#' @param obj a valid \code{hydromet_XXX} class object.
#' @param slot_name string vector with the name of the slot(s) to show. Alternatively
#' you can use \code{'fill'} or \code{'empty'} to get the data frames with or without
#' data respectively.
#' @param show string with either \code{'head'} or \code{'tail'}.
#'
#' @return It prints the data inside the required slot.
#'
#' @importFrom utils head tail
#'
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # lets work with the cuevas station
#' path <- system.file('extdata', package = 'hydrotoolbox')
#'
#' # use the build method
#' hm_cuevas <-
#'   hm_create() %>%
#'   hm_build(bureau = 'ianigla', path = path,
#'            file_name = 'ianigla_cuevas.csv',
#'            slot_name = c('tair', 'rh', 'patm',
#'                          'precip', 'wspd', 'wdir',
#'                          'kin', 'hsnow', 'tsoil'),
#'            by = 'hour',
#'            out_name = c('tair(°C)', 'rh(%)', 'patm(mbar)',
#'                         'p(mm)', 'wspd(km/hr)', 'wdir(°)',
#'                         'kin(kW/m2)', 'hsnow(cm)', 'tsoil(°C)' )
#'           )
#'
#' # now we want to know which are the slots with data
#' hm_show(obj = hm_cuevas)
#'
#' # see the last values of our data
#' hm_show(obj = hm_cuevas, show = 'tail')
#'
#' # or maybe we want to know which slot have no data
#' hm_show(obj = hm_cuevas, slot_name = 'empty')
#'
#' # focus on specific slots
#' hm_show(obj = hm_cuevas, slot_name = c('kin', 'rh') )
#' hm_show(obj = hm_cuevas, slot_name = c('kin', 'rh'), show = 'tail' )
#'}
#'
setGeneric(name = 'hm_show',
def = function(obj, slot_name = 'fill', show = 'head')
{
  standardGeneric('hm_show')
})

#' @describeIn hm_show print method for hydromet class
## hydromet
setMethod(f = 'hm_show',
          signature = 'hydromet',
          definition = function(obj, slot_name = 'fill', show = 'head')
          {
            #*////////////////////
            #* conditionals
            #*////////////////////
            #* obj
            check_class(argument = obj,
                        target = 'hydromet',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = c('fill',
                                    'empty',
                                    slotNames(x = 'hydromet') ),
                         arg_name = 'slot_name')

            #* show
            check_class(argument = show,
                        target = 'character',
                        arg_name = 'show')

            check_string(argument = show,
                         target = c('head', 'tail'),
                         arg_name = 'show')

            check_length(argument = show,
                         max_allow = 1,
                         arg_name = 'show')


            #*////////////////////
            #* function
            #*////////////////////

            if(slot_name[1] == 'fill'){
              # all slots with data
              slot_nm <- slotNames(x = 'hydromet')
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if( is.null(data_out) == TRUE | !is.na(data_out) ){

                  if(show == 'head'){
                    out[[ slot_nm[i] ]] <- head( data_out )

                  } else {
                    out[[ slot_nm[i] ]] <- tail( data_out )
                  }


                }

                rm(data_out)

              } # end for

            } else if(slot_name[1] == 'empty'){
              # all empty slots
              slot_nm <- slotNames(x = 'hydromet')
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if( is.null(data_out) == FALSE | is.na(data_out)){

                  if(show == 'head'){
                    out[[ slot_nm[i] ]] <- head( data_out )

                  } else {
                    out[[ slot_nm[i] ]] <- tail( data_out )
                  }


                }

                rm(data_out)

              } # end for

            } else {
              # user specified slots
              slot_nm <- slot_name
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if(show == 'head'){
                  out[[ slot_nm[i] ]] <- head( data_out )

                } else {
                  out[[ slot_nm[i] ]] <- tail( data_out )

                }

                rm(data_out)

              } # end for


            }

            # return
            return(out)


          } )


#' @describeIn hm_show print method for station class
## station
setMethod(f = 'hm_show',
          signature = 'hydromet_station',
          definition = function(obj, slot_name = 'fill', show = 'head')
          {
            #*////////////////////
            #* conditionals
            #*////////////////////

            #* obj
            check_class(argument = obj,
                        target = 'hydromet_station',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = c('fill', 'empty',
                                    slotNames(x = 'hydromet_station') ),
                         arg_name = 'slot_name')

            #* show
            check_class(argument = show,
                        target = 'character',
                        arg_name = 'show')

            check_string(argument = show,
                         target = c('head', 'tail'),
                         arg_name = 'show')

            check_length(argument = show,
                         max_allow = 1,
                         arg_name = 'show')



            #*////////////////////
            #* function
            #*////////////////////

            if(slot_name[1] == 'fill'){
              # all slots with data
              slot_nm <- setdiff(x = slotNames("hydromet_station"),
                                 y = slotNames("hydromet")
                                 )
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
               data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

               if(  !is.null(data_out)){

                 if(show == 'head'){
                   out[[ slot_nm[i] ]] <- head( data_out )

                 } else {
                   out[[ slot_nm[i] ]] <- tail( data_out )
                 }


               }

               rm(data_out)

              } # end for

            } else if(slot_name[1] == 'empty'){
              # all empty slots
              slot_nm <- setdiff(x = slotNames("hydromet_station"),
                                 y = slotNames("hydromet")
                                 )

              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if( ncol(data_out) == 0){

                  if(show == 'head'){
                    out[[ slot_nm[i] ]] <- head( data_out )

                  } else {
                    out[[ slot_nm[i] ]] <- tail( data_out )
                  }


                }

                rm(data_out)

              } # end for

            } else {
              # user specified slots
              slot_nm <- slot_name
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if(show == 'head'){
                    out[[ slot_nm[i] ]] <- head( data_out )

                  } else {
                    out[[ slot_nm[i] ]] <- tail( data_out )

                  }

                rm(data_out)

              } # end for


            }

            # return
            return(out)


          } )


#' @describeIn hm_show print method for compact class
## compact
setMethod(f = 'hm_show',
          signature = 'hydromet_compact',
          definition = function(obj, slot_name = 'compact', show = 'head')
          {
            #*/////////////////
            #* conditionals
            #*/////////////////

            #* obj
            check_class(argument = obj,
                        target = 'hydromet_compact',
                        arg_name = 'obj')

            #* slot_name
            check_class(argument = slot_name,
                        target = 'character',
                        arg_name = 'slot_name')

            check_string(argument = slot_name,
                         target = c('fill', 'empty',
                                    slotNames(x = 'hydromet_compact') ),
                         arg_name = 'slot_name')

            #* show
            check_class(argument = show,
                        target = 'character',
                        arg_name = 'show')

            check_string(argument = show,
                         target = c('head', 'tail'),
                         arg_name = 'show')

            check_length(argument = show,
                         max_allow = 1,
                         arg_name = 'show')


            #*/////////////////
            #* function
            #*/////////////////

            if(slot_name == 'fill'){
              # all slots with data
              slot_nm <- 'compact'
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if(  !is.null(data_out)){

                  if(show == 'head'){
                    out[[ slot_nm[i] ]] <- head( data_out )

                  } else {
                    out[[ slot_nm[i] ]] <- tail( data_out )
                  }


                }

                rm(data_out)

              } # end for

            } else if(slot_name == 'empty'){
              # all empty slots
              slot_nm <- 'compact'
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if( ncol(data_out) == 0){

                  if(show == 'head'){
                    out[[ slot_nm[i] ]] <- head( data_out )

                  } else {
                    out[[ slot_nm[i] ]] <- tail( data_out )
                  }


                }

                rm(data_out)

              } # end for

            } else {
              # user specified slots
              slot_nm <- slot_name
              n_it    <- length(slot_nm)

              out <- list()
              for(i in 1:n_it){
                # get the data
                data_out <- hm_get(obj = obj, slot_name = slot_nm[i])

                if(show == 'head'){
                  out[[ slot_nm[i] ]] <- head( data_out )

                } else {
                  out[[ slot_nm[i] ]] <- tail( data_out )

                }

                rm(data_out)

              } # end for


            }

            # return
            return(out)


          } )
