#' Check cross for argument coherence
#'
#' @param ref_arg the argument for reference length.
#' @param eval_arg the argument to evaluate.
#' @param arg_names vector string with arguments names (\code{ref_arg} and \code{eval_arg})
#'
#' @return An error message if \option{eval_arg} does not fit the \option{ref_arg} length.
#'
#' @keywords internal
#'
check_cross <- function(ref_arg, eval_arg, arg_names){

  if( !is.null(ref_arg) & !is.null(eval_arg) ){

    reference <- length(ref_arg)
    evaluate  <- length(eval_arg)

    if( evaluate != reference ){

      error_message <-
        paste0('Out of bounds...please check for ', arg_names[2], ' argument. The length should not be different from: ', arg_names[1], '.')


      stop( error_message, call. = FALSE )
    }

  }




}
