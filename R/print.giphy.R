#' @export
print.giphy <- function(x, ...) {

  out_table <- data.frame(slug   = x$slug,
                          id     = x$id,
                          width  = x$images$original$width,
                          height = x$images$original$height,
                          frames = x$images$original$frames)

  cat(paste0('\nA giphy object with ', nrow(x$images), ' gif images:\n'))
  print(out_table, row.names = FALSE, right = FALSE)

}
