
#' Palette de couleurs qualitative
#'
#' Renvoie une palette de couleur de 1 à 11 couleurs qualitative
#'
#' @param n Le nombre de couleurs désirées dans la palette.
#'
#' @return Une palette de couleur.
#'
#'
#'
#' @export

shiny_colors <- function(n) {

  #https://github.com/plotly/plotly.py/issues/1026

  # shiny_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf") %>%
  #   utils::head(n)
  shiny_colors <- c("#0011ff", "#ff9900", "#008500", "#ff0001",   "#00e7ff","#d400ff", "#80fc53","#a1a1a1", "#ff8fd0", "#f8fa02", "#000000")%>%
    utils::head(n)

  return(shiny_colors)

}

#' Round with a sum 100 (avoid round errors)
#'
#' @param x A numeric vector
#'
#' @return A round numeric vector
#'
#' @examples
#' x <- c(81.808219, 6.575342, 11.616438)
#'
#' sum(x)
#' sum(round(x))
#'
#' graphr::round_100(x)
#' sum(graphr::round_100(x))
#'
#' @export
round_100 <- function(x) {

  if (sum(round(x)) == 100) {
    return(round(x))
  }

  diff <- abs(x - round(x))
  position_max <- which(diff == max(diff))

  round_100 <- round(x)

  if (sum(round(x)) == 99) {
    round_100[position_max] <- round_100[position_max] + 1
  } else if (sum(round(x)) == 101) {
    round_100[position_max] <- round_100[position_max] - 1
  }

  return(round_100)
}

#' Return a base 100 serie
#'
#' @param x A numeric vector
#'
#' @return A base 100 vector
#'
#' @examples
#' x <- c(5096, 5077, 5278, 5352, 5437, 5387)
#'
#' graphr::base_100(x)
#'
#' @export
base_100 <- function(x) {

  base_100 <- 100 + ((x - x[1]) / x[1] * 100)

  return(base_100)
}
