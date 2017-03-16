#' @title Translate Endpoint
#' @description  The translate API draws on search, but uses the Giphy
#' "special sauce" to handle translating from one vocabulary to another.
#' In this case, words and phrases to GIFs. Example implementations of
#' translate can be found in the Giphy Slack, Hipchat, Wire, or
#' Dasher integrations. Use a plus or url encode for phrases.
#' @param s - A term or phrase to translate into a GIF
#' @param rating - A rating for age appropriate gifs.  Options include \code{"y"}, \code{"g"}, \code{"pg"}, \code{"pg-13"} and \code{"r"}.
#' @param lang - Regional content; see the allowed 2-letter ISO 639-1 country codes listed in the Details.
#' @param sticker Should a GIF be returned or an animated sticker (with transparent background)?  Default to \code{FALSE}.
#' @param api_key - Giphy provides a default \code{api_key} value for public beta testing: \code{"dc6zaTOxFJmzC"}.  This is the default value.
#' @importFrom jsonlite fromJSON
#' @importFrom httr content GET
#'
#' @author Simon J. Goring \email{simon.j.goring@@gmail.com}
#' @details
#'     Tanslates a word or phrase to a GIF using the \code{giphy} API.
#'     Be aware that failure to use the \code{rating} tag may result in gifs that are not safe for work use.
#'     The default API key used here is for testing purposes only.  More information on the use of the giphy API is available at \url{https://github.com/Giphy/GiphyAPI}
#'     Language support is documented in the giphy API documentation for Language Support: \url{https://github.com/Giphy/GiphyAPI#language-support}.
#' @example
#' gif <- translate('this machine kills facists', rating = 'g')
#' plot(gif)
#' @export

translate <- function(s, rating = NULL, lang = NULL, sticker = FALSE, api_key = 'dc6zaTOxFJmzC') {

  params <- as.list(environment())

  params$fmt <- 'json'

  if (params$sticker == TRUE) {

    base_uri <- 'http://api.giphy.com/v1/stickers/translate'
    params$sticker <- NULL

  } else if (params$sticker == FALSE) {

    base_uri <- 'http://api.giphy.com/v1/gifs/translate'
    params$sticker <- NULL

  }

  giphy_response <- httr::content(httr::GET(base_uri, query = params), as = "text", encoding = 'UTF-8')
  giphy_list <- jsonlite::fromJSON(giphy_response)

  if(giphy_list$meta$status %in% c(400, 401, 403)) {
    stop(paste0('giphy returned an error: ', giphy_list$meta$msg))
  }

  giphy_out <- giphy_list$data
  attr(giphy_out, 'meta') <- giphy_list$meta
  attr(giphy_out, 'pagination') <- giphy_list$pagination

  class(giphy_out) <- c('giphy', 'data.frame')

  return(giphy_out)

}
