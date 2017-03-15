#' @title Trending GIFs
#' @description Fetch GIFs currently trending online. Hand curated by the Giphy editorial team. The data returned mirrors the GIFs showcased on the Giphy homepage. Returns 25 results by default.
#'
#' @param limit - The number of results to return, up to 100. The default is \code{limit=25}.
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

trending <- function(limit = 25, rating = NULL, sticker = FALSE, api_key = 'dc6zaTOxFJmzC') {

  params <- as.list(environment())

  params$fmt <- 'json'

  if (params$sticker == TRUE) {

    base_uri <- 'http://api.giphy.com/v1/stickers/trending'
    params <- as.list(environment())

    params$sticker <- NULL

  } else if (params$sticker == FALSE) {

    base_uri <- 'http://api.giphy.com/v1/gifs/trending'
    params$sticker <- NULL

  }

  giphy_response <- httr::content(httr::GET(base_uri, query = params), as = "text", encoding = 'UTF-8')
  giphy_list <- jsonlite::fromJSON(giphy_response)

  if(giphy_list$meta$status %in% c(400, 401, 403)) {
    stop(paste0('giphy returned an error: ', meta$msg))
  }

  giphy_out <- giphy_list$data
  attr(giphy_out, 'meta') <- giphy_list$meta
  attr(giphy_out, 'pagination') <- giphy_list$pagination

  return(giphy_out)

}
