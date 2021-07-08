#' Check class for arguments
#'
#' @param argument the argument to evaluate.
#' @param target string with the allowed class.
#' @param arg_name string with argument name to print (just for error message)
#'
#' @return An error message if \option{argument} does not fit the \option{target} requirements
#'
#' @keywords internal
#'
check_class <- function(argument, target, arg_name){

  if( !is.null(argument) ){

    class_eval <- class(argument)[1]

    guess <- match(x = class_eval, table = target)
    is_na <- sum(guess) # to get NA value

    if( is.na(is_na) ){
      error_message <-
        paste0('Oops...please check for ', arg_name, ' argument. Allowed classes are: ', paste(target, collapse = ', '), '.')

      stop( error_message, call. = FALSE )
    }

  }




}
