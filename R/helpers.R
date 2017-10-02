#' stats_count_uni
#'
#' @param champ_quali \dots
#' @param max_modalites \dots
#' @param choix_multiple_labels \dots
#'
#' @export
#' @keywords internal
stats_count_uni <- function(champ_quali, max_modalites = NULL, choix_multiple_labels = NULL) {

  stats <- dplyr::tibble(champ_quali = champ_quali) %>%
    dplyr::filter(!is.na(champ_quali)) %>%
    dplyr::count(champ_quali)

  if (!is.factor(champ_quali)) {
    stats <- dplyr::arrange(stats, -n)
  }

  if (!is.null(max_modalites)) {
    if (nrow(stats) > max_modalites) {
      stats <- dplyr::filter(stats, row_number() <= max_modalites - 1) %>%
        dplyr::bind_rows(dplyr::tibble(champ_quali = "...",
                                n = dplyr::filter(stats, row_number() >= max_modalites) %>%
                                  dplyr::pull(n) %>%
                                  sum()))
    }
  }

  stats <- dplyr::mutate(stats, pct = paste0(trimws(format(round(n / sum(stats$n) * 100, 1))), "%"))

  if (!is.null(choix_multiple_labels)) {

    stats <- dplyr::full_join(stats,
                              dplyr::tibble(label = choix_multiple_labels),
                              by = c("champ_quali" = "label")) %>%
      dplyr::arrange(-n) %>%
      dplyr::mutate(n = ifelse(is.na(n), 0, n),
                    ordre = dplyr::row_number(champ_quali))
  }

  return(stats)
}

#' stats_count_bi
#'
#' @param champ_quali \dots
#' @param champ_x \dots
#' @param identifiant \dots
#' @param complet \dots
#'
#' @export
#' @keywords internal
stats_count_bi <- function(champ_quali, champ_x, identifiant = NULL, complet = FALSE) {

  stats <- dplyr::tibble(champ_x = champ_x, champ_quali = champ_quali) %>%
    dplyr::count(champ_x, champ_quali) %>%
    dplyr::group_by(champ_x) %>%
    dplyr::mutate(pos = cumsum(n) - 0.5 * n) %>%
    dplyr::ungroup()

  if (complet == TRUE) {

    complet <- expand.grid(factor(levels(champ_x), levels(champ_x)), factor(levels(champ_quali), levels(champ_quali)))
    names(complet) <- c("champ_x", "champ_quali")

    stats <- dplyr::mutate(stats, pct = paste0(trimws(format(round(n / length(unique(identifiant)) * 100, 1))), "%")) %>%
      dplyr::full_join(complet, by = c("champ_x", "champ_quali")) %>%
      dplyr::arrange(champ_x, champ_quali) %>%
      dplyr::mutate(n = ifelse(is.na(n), 0, n))

  }

  return(stats)

}

#' pct_repondants
#'
#' @param table \dots
#' @param stats \dots
#'
#' @export
#' @keywords internal
pct_repondants <- function(repondants, total) {

  if (repondants != total) {
    n_repondants <- paste0("Répondants : ", repondants, " (", format(round(repondants / total * 100, 1)), " %)")
  } else {
    n_repondants <- NULL
  }

  return(n_repondants)

}

#' echelle_integer
#'
#' @param champ \dots
#' @param n \dots
#'
#' @export
#' @keywords internal
echelle_integer <- function(champ, n = 5) {

  if (length(champ) == 0) return(champ)

  echelle <- 0:max(champ)

  if (length(echelle) / n > 1.5) {

    #echelle <- seq(0, max(echelle), by = n)
    echelle <- pretty(0:max(champ), n = n)
  }

  return(echelle)
}
