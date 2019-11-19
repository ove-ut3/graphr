
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
  #   head(n)
  shiny_colors <- c("#0011ff", "#ff9900", "#008500", "#ff0001",   "#00e7ff","#d400ff", "#80fc53","#a1a1a1", "#ff8fd0", "#f8fa02", "#000000")%>%
    head(n)

  return(shiny_colors)

}
