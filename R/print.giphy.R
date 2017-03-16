#' @title Print a giphy object
#' @description Print method for giphyR
#' @param image a \code{giphy} object.
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details
#'
#' @example
#'
#' @export
print.giphy <- function(image) {

  out_table <- data.frame(slug   = image$slug,
                          id     = image$id,
                          width  = image$images$original$width,
                          height = image$images$original$height,
                          frames = image$images$original$frames)

  cat(paste0('\nA giphy object with ', nrow(image$images), ' gif images:\n'))
  print(out_table, row.names = FALSE, right = FALSE)

}
