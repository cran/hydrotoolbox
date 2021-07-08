#' Check argument length
#'
#' @param argument the argument to evaluate.
#' @param max_allow maximum argument length
#' @param arg_name string with argument name to print (just for error message)
#'
#' @return An error message if \option{argument} does not fit the \option{max_allow} requirements
#'
#' @keywords internal
#'
check_length <- function(argument, max_allow, arg_name){

  if( !is.null(argument) ){

    guess <- length(argument)

    if( guess > max_allow ){

      error_message <-
        paste0('Out of bounds...please check for ', arg_name, ' argument. The length should not supercede: ', paste(max_allow, collapse = ', '), '.')


      stop( error_message, call. = FALSE )
    }

  }

}
