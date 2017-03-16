#' @title Plot a GIF
#' @description Plots the giphy into your view-port.
#'
#' @param x a \code{giphy} object returned from one of the \code{giphy} commands.
#' @param n When the \code{giphy} contains multiple images, the index of the image requested.
#' @param type GIF size, either \code{original} or \code{downsized}.  Default is \code{original}.
#' @param ... arguments to be passed to \code{magick::image_read}.
#'
#' @details Plotting a giphy requires installation of the \code{imagemagick} program.  When a \code{giphy}
#'     object is returned from one of the calls, the user may plot the first object returned (default \code{n = 1})
#'     or, may select one of the images returned.  The limit is generally 25, but more or fewer may be returned
#'     depending on the user parameters set.
#'     A user may plot the original image, or, possibly, a downscaled version of the GIF, if such an object exists.
#'     It is possible to pass other parameters to the function, but they are unused.
#' @importFrom magick image_read
#' @importFrom httr content GET
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details
#'
#' @examples
#'
#' gifs <- trending('horses', rating = 'g')
#' plot(gifs, n = 1)
#'
#' # Plot a smaller version of the same gif:
#' plot(gifs, n = 2, type = 'downsized')
#' @export

plot.giphy <- function(x, n = 1, type = 'original', ...) {

  if(n > nrow(x) & !is.list(x$images)) {
    stop('There are only ', nrow(x), ' indexed GIFs in the giphy object.')
  }

  if (type %in% 'downsized') {
    if ('downsized' %in% names(x$images)) {

      image_out <- x$images$downsized$url[n]

    } else if ('fixed_height_downsampled_url' %in% names(x)) {

      image_out <- x$fixed_height_downsampled_url

    } else {

      stop('There are no downscaled images in this giphy object.')

    }
  } else if (type %in% 'original') {
    if ('original' %in% names(x$images)) {
      image_out <- x$images$original$url[n]
    } else if ('image_url' %in% names(x)) {
      image_out <- x$image_url
    }
  }

  if (is.na(image_out)) {
    stop('There are no valid images in this giphy object with the current settings.')
  }

  out_fig <- magick::image_read(image_out)

  return(out_fig)

}
