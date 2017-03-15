#' @title Giphy search
#' @description Search all Giphy GIFs for a word or phrase. Punctuation will be stripped and ignored.  The search can be limited to \
#' @param q Search query term or phrase
#' @param limit - The number of results to return, up to 100. The default is \code{limit=25}.
#' @param offset - An offset, to manage limited returns.  The default is \code{offset=0}, for subsequent searches, can increment \code{offset} by the size of \code{limit}
#' @param rating - A rating for age appropriate gifs.  Options include \code{"y"}, \code{"g"}, \code{"pg"}, \code{"pg-13"} and \code{"r"}.
#' @param lang - Regional content; see the allowed 2-letter ISO 639-1 country codes listed in the Details.
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
#'

search <- function(q, limit = 25, offset = 0, rating = NULL, lang = NULL, sticker = FALSE, api_key = 'dc6zaTOxFJmzC') {

  params <- as.list(environment())

  params$fmt <- 'json'

  if (params$sticker == TRUE) {

    base_uri <- 'http://api.giphy.com/v1/stickers/search'
    params$sticker <- NULL

  } else if (params$sticker == FALSE) {

    base_uri <- 'http://api.giphy.com/v1/gifs/search'
    params$sticker <- NULL

  }

  giphy_response <- httr::content(httr::GET(base_uri, query = params), as = "text")
  giphy_list <- jsonlite::fromJSON(giphy_response)

  if(giphy_list$meta$status %in% c(400, 401, 403)) {
    stop(paste0('giphy returned an error: ', meta$msg))
  }

  giphy_out <- giphy_list$data
  attr(giphy_out, 'meta') <- giphy_list$meta
  attr(giphy_out, 'pagination') <- giphy_list$pagination

  return(giphy_out)
}
