#' @title Get GIF by ID Endpoint
#' @description Get GIF metadata based on the unique id.
#'
#' @param id The unique GIF identifier, or a character vector of IDs.
#' @param api_key - Giphy provides a default \code{api_key} value for public beta testing: \code{"dc6zaTOxFJmzC"}.  This is the default value.
#'

gif_id <- function(id = NULL, api_key = 'dc6zaTOxFJmzC') {

  params <- as.list(environment())

  params$fmt <- 'json'

  if (length(params$id) == 1) {

    base_uri <- paste0('http://api.giphy.com/v1/gifs/', params$id)

    params$id <- NULL

  } else if (length(params$id > 1)) {

    params$ids <- paste0(params$id, collapse = ',')

    params$id <- NULL

    base_uri <- paste0('http://api.giphy.com/v1/gifs')

  }

  giphy_response <- httr::content(httr::GET(base_uri, query = params),
                                  as = "text")

  giphy_list <- jsonlite::fromJSON(giphy_response)

  if(giphy_list$meta$status == 403) {
    stop(paste0('giphy returned an error: ', giphy_list$meta$msg))
  }

  giphy_out <- giphy_list$data
  attr(giphy_out, 'meta') <- giphy_list$meta
  attr(giphy_out, 'pagination') <- giphy_list$pagination

  class(giphy_out) <- c('giphy', 'data.frame')

  return(giphy_out)

}
