#' @title Return a random GIF
#' @description Returns a random GIF from Giphy, which may be limited using tags.
#' @param tag a single, or multiple text strings.
#' @param rating - A rating for age appropriate gifs.  Options include \code{"y"}, \code{"g"}, \code{"pg"}, \code{"pg-13"} and \code{"r"}.
#' @param sticker Should a GIF be returned or an animated sticker (with transparent background)?  Default to \code{FALSE}.
#' @param api_key - Giphy provides a default \code{api_key} value for public beta testing: \code{"dc6zaTOxFJmzC"}.  This is the default value.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details
#'
#' @example

random <- function(tag = NULL, rating = NULL, sticker = FALSE, api_key = 'dc6zaTOxFJmzC') {

  params <- as.list(environment())

  params$fmt <- 'json'

  if (params$sticker == TRUE) {

    base_uri <- 'http://api.giphy.com/v1/stickers/random'
    params$sticker <- NULL

  } else if (params$sticker == FALSE) {

    base_uri <- 'http://api.giphy.com/v1/gifs/random'
    params$sticker <- NULL

  }

  giphy_response <- httr::content(httr::GET(base_uri, query = params), as = "text", encoding = 'UTF-8')
  giphy_list <- jsonlite::fromJSON(giphy_response)

  if(giphy_list$meta$status %in% c(400, 401, 403)) {
    stop(paste0('giphy returned an error: ', meta$msg))
  }

  giphy_out <- data.frame(giphy_list$data, stringsAsFactors = FALSE)
  attr(giphy_out, 'meta') <- giphy_list$meta
  attr(giphy_out, 'pagination') <- giphy_list$pagination

  return(giphy_out)
}
