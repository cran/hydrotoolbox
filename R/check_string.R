#' Check string argument consistency
#'
#' @param argument the argument to evaluate.
#' @param target allowed string values.
#' @param arg_name string with argument name to print (just for error message)
#'
#' @return An error message if \option{argument} does not fit the \option{target} requirements
#'
#' @keywords internal
#'
check_string <- function(argument, target, arg_name){

  if( !is.null(argument) ){

    guess <- match(x = argument, table = target)
    is_na <- sum(guess) # to get NA value

    if( is.na(is_na) ){
      flag <- which( is.na(guess) )
      error_message <-
        paste(
          paste0('Oops...please check for ', arg_name, ' argument. Allowed strings are: ', paste(target, collapse = ', '), '.'),
          paste0('Possible ', arg_name, ' errors entries: ', paste(argument[flag], collapse = ', ') ),
          sep = '\n'
        )

      stop( error_message, call. = FALSE )
    }

  }

}
