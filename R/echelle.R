#' echelle_integer
#'
#' @param champ

echelle_integer <- function(champ, n = 5) {

  if (length(champ) == 0) return(champ)

  echelle <- 0:max(champ)

  if (length(echelle) / n > 1.5) {

    echelle <- seq(0, max(echelle), by = n)
  }

  return(echelle)
}
