# 209-09-16
# Samuel Lee

#' Make shades
#'
#' Given a colour make \code{n} lighter or darker shades.
#'
#'
#' @param colour The R colour name to make shades of
#' @param n The number of shades to make
#' @param lighter  Whether to make lighter (\code{TRUE}) or darker (\code{FALSE}) shades
#'
#' @details This is an essay exposing the cultural importance of the \code{make_shades()} function.
#'   It is left as an exercise for the interested reader.
#'
#' @return A vector of n hex colour codes
#' @export
#'
#' @examples
#' # Five lighter shades
#' make_shades("goldenrod", 5)
#'
#' # Five darker shades
#' \dontrun{
#' make_shades("goldenrod", 5, lighter = FALSE)
#'   }
make_shades <- function(colour, n, lighter = TRUE) {

  # Check the value of n
  # stopifnot(n < 1, "n must be at least 1") ## this is one option
  # checkmate package can be good for automating checking parameters
  # etc. assertthat can do this too
  # devtools::test_coverage is a really good way to view test coverage
  if(n < 1){
    stop("n must be at least 1")
  }

  # check that colour is a valid colour name
  if(! colour %in% grDevices::colours()){
    stop("The colour provided is not a valid name")
  }

  # Convert the colour to RGB
  colour_rgb <- grDevices::col2rgb(colour)[, 1]

  # Decide if we are heading towards white or black
  if (lighter) {
    end <- 255
  } else {
    end <- 0
  }

  # Calculate the red, green and blue for the shades
  # we calculate one extra point to avoid pure white/black
  red <- seq(colour_rgb[1], end, length.out = n + 1)[1:n]
  green <- seq(colour_rgb[2], end, length.out = n + 1)[1:n]
  blue <- seq(colour_rgb[3], end, length.out = n + 1)[1:n]

  # Convert the RGB values to hex codes
  shades <- grDevices::rgb(red, green, blue, maxColorValue = 255)

  return(shades)
}


#' Plot colours
#'
#' Plot a vector of colours to see what they look like
#'
#' @param colours Vector of colour to plot
#'
#' @return A ggplot2 object
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' shades <- make_shades("goldenrod", 5)
#' plot_colours(shades)
plot_colours <- function(colours) {
  plot_data <- data.frame(Colour = colours)

  # the more verbose .data$var syntax is required within packages
  # view the docs for the course for more info. depend on rlang to
  # drop the warning for this. have to add @importFrom to docs
  ggplot2::ggplot(plot_data,
                 ggplot2::aes(x = .data$Colour, y = 1, fill = .data$Colour,
             label = .data$Colour)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(angle = "90") +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void()
}
