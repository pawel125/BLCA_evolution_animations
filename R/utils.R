
#' Stringify the list of items
#'
#' @param args Named list of arguments
#'
#' @return Character
#' @export
#'
#' @examples
#' x <- lst(
#'   a = 1,
#'   b = 33,
#'   c = "sfdf"
#' )
#' args_to_string(x)
#' > "a = 1, b = 33, c = sfdf"
args_to_string <- function(args) {
  args %>%
    imap_chr(~str_c(.y, " = ", .x)) %>%
    str_c(collapse = ", ")
}
