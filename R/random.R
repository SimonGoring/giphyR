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
    stop(paste0('giphy returned an error: ', giphy_list$meta$msg))
  }

  empty <- data.frame(url = NA,
                      width = NA,
                      height = NA,
                      size = NA, mp4 = NA,
                      mp4_size = NA, webp = NA,
                      webp_size = NA)

  empty_still <- data.frame(url = NA,
                            width = NA,
                            height = NA,
                            size = NA)

  empty_mp4 <- data.frame(mp4 = NA, mp4_size = NA, width = NA, height = NA)

  giphy_out <- data.frame(type = giphy_list$data$type,
                          id   = giphy_list$data$id,
                          slug = '',
                          url = giphy_list$data$url,
                          bitly_gif_url = '',
                          bitly_url = '',
                          embed_url = '',
                          username = giphy_list$data$username,
                          source = '',
                          rating = '',
                          content_url = '',
                          source_tld = '',
                          source_post_url = '',
                          is_indexable = NA,
                          import_datetime = NA,
                          trending_datetime = NA,
                          stringsAsFactors = FALSE)


  giphy_out$user <- data.frame(avatar_url = '',
                               banner_url = '',
                               profile_url = '',
                               username = giphy_list$data$username,
                               display_name = '',
                               twitter = '')

  giphy_out$images <- data.frame(a = 1)

  giphy_out$images$fixed_height             <- empty
  giphy_out$images$fixed_height_still       <- empty_still
  giphy_out$images$fixed_height_downsampled <- data.frame(url = giphy_list$data$fixed_height_downsampled_url,
                                                          width  = giphy_list$data$fixed_height_downsampled_width,
                                                          height = giphy_list$data$fixed_height_downsampled_height,
                                                          webp   = NA, webp_size = NA)

  giphy_out$images$fixed_width             <- empty
  giphy_out$images$fixed_width_still       <- empty_still
  giphy_out$images$fixed_width_downsampled <- data.frame(url    = giphy_list$data$fixed_width_downsampled_url,
                                                         width  = giphy_list$data$fixed_width_downsampled_width,
                                                         height = giphy_list$data$fixed_width_downsampled_height,
                                                         webp   = NA, webp_size = NA)
  giphy_out$images$fixed_height_small      <- data.frame(url    = giphy_list$data$fixed_height_small_url,
                                                         width  = giphy_list$data$fixed_height_small_width,
                                                         height = giphy_list$data$fixed_height_small_height,
                                                         mp4    = NA, mp4_size = NA,
                                                         webp   = NA, webp_size = NA)

  giphy_out$images$fixed_height_small_still <- data.frame(url   = giphy_list$data$fixed_height_small_still_url,
                                                          width = NA, height = NA, size = NA)
  giphy_out$images$fixed_width_small        <- data.frame(url    = giphy_list$data$fixed_height_small_url,
                                                          width  = giphy_list$data$fixed_height_small_width,
                                                          height = giphy_list$data$fixed_height_small_height,
                                                          mp4    = NA, mp4_size = NA,
                                                          webp   = NA, webp_size = NA)

  giphy_out$images$fixed_width_small_still <- data.frame(url   = giphy_list$data$fixed_width_small_still_url,
                                                         width = NA, height = NA, size = NA)

  giphy_out$images$downsized       <- empty

  giphy_out$images$downsized_still <- empty_still

  giphy_out$images$downsized_large <- empty_still

  giphy_out$images$downsized_medium <- empty_still

  giphy_out$images$original <- data.frame(url   = giphy_list$data$image_original_url,
                                          width = giphy_list$data$image_width,
                                          height = giphy_list$data$image_height,
                                          size = NA,
                                          frames = giphy_list$data$image_frames,
                                          mp4 = giphy_list$data$image_mp4_url,
                                          mp4_size = NA,
                                          webp = NA,
                                          webp_size = NA,
                                          hash = NA, stringsAsFactors = FALSE)

  giphy_out$images$original_still <- empty_still

  giphy_out$images$looping        <- data.frame(mp4 = NA, mp4_size = NA)

  giphy_out$images$preview <- empty_mp4

  giphy_out$images$downsized_small <- data.frame(mp4 = NA, mp4_size = NA)

  giphy_out$images$preview_gif <- empty_still

  giphy_out$images$original_mp4 <- empty_mp4

  giphy_out$images$hd <- empty_mp4

  giphy_out$images$`480w_still` <- empty_still

  giphy_out$images <- giphy_out$images[,!colnames(giphy_out$images) %in% 'a']

  attr(giphy_out, 'meta') <- giphy_list$meta
  attr(giphy_out, 'pagination') <- giphy_list$pagination

  class(giphy_out) <- c('giphy', 'data.frame')

  return(giphy_out)
}
