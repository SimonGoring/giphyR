#' @title Plot a GIF
#' @description Returns a random GIF from Giphy, which may be limited using tags.
#' @param image a \code{giphy} object returned from one of the \{giphy} commands.
#' @param n When the \code{giphy} contains multiple images, the index of the image requested.
#' @param type GIF size, either \code{original} or \code{downscaled}.  Default is \code{original}.
#'
#' @importFrom magick iimage_read
#' @importFrom httr content GET
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details
#'
#' @example
#'

plot_giphy <- function(image, n = 1, type = 'original', action = TRUE) {

  if(n > nrow(image)) {
    stop('There are only ', nrow(image), ' indexed GIFs in the giphy object.')
  }

  if (type %in% 'downsized') {
    image_out <- image$images$downsized$url[n]
  } else if (type %in% 'original') {
    image_out <- image$images$original$url[n]
  }

  out_fig <- magick::image_read(image_out)

  return(out_fig)

}
