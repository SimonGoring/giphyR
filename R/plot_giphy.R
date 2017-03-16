#' @title Plot a GIF
#' @description Returns a random GIF from Giphy, which may be limited using tags.
#' @param image a \code{giphy} object returned from one of the \{giphy} commands.
#' @param n When the \code{giphy} contains multiple images, the index of the image requested.
#' @param type GIF size, either \code{original} or \code{downsized}.  Default is \code{original}.
#'
#' @importFrom magick iimage_read
#' @importFrom httr content GET
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details
#'
#' @example
#'
#' gifs <- trending('horses', rating = 'g')
#' plot_giphy(gifs, n = 1)
#'
#' # Plot a smaller version of the same gif:
#' plot_giphy(gifs, n = 2, type = 'downsized')

plot_giphy <- function(image, n = 1, type = 'original', action = TRUE) {

  if(n > nrow(image) & !is.list(image$images)) {
    stop('There are only ', nrow(image), ' indexed GIFs in the giphy object.')
  }

  if (type %in% 'downsized') {
    if ('downsized' %in% names(image$images)) {

      image_out <- image$images$downsized$url[n]

    } else if ('fixed_height_downsampled_url' %in% names(image)) {

      image_out <- image$fixed_height_downsampled_url

    } else {

      stop('There are no downscaled images in this giphy object.')

    }
  } else if (type %in% 'original') {
    if ('original' %in% names(image$images)) {
      image_out <- image$images$original$url[n]
    } else if ('image_url' %in% names(image)) {
      image_out <- image$image_url
    }
  }

  if (is.na(image_out)) {
    stop('There are no valid images in this giphy object with the current settings.')
  }

  out_fig <- magick::image_read(image_out)

  return(out_fig)

}
