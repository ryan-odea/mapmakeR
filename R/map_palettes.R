#' Complete list of pre-built color palettes
#'
#' @export
map_palettes <- list(
  Daylight = c(river = "steelblue", railway = "black", highway = "black", main_street = "black", small_street = "darkgray", background = "white", metro = "orange"),
  Nightlight = c(river = "steelblue",railway = "white", highways = "white",main_streets = "white", small_streets = "white", background = "#333333", metro = "orange")
)

#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
#' @export

print_palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  colors <- unname(x)
  color_names <- names(x)

  image(1:n, 1, as.matrix(1:n), col = colors,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)

  text(seq_along(colors), rep(1.0, length(colors)), color_names, cex = 0.8, pos = 1)
}

map_palette <- function(palette_name) {
  if (palette_name %in% names(map_palettes)) {
    return(map_palettes[[palette_name]])
  } else {
    stop("Invalid palette name. Please check the available palettes.")
  }
}
