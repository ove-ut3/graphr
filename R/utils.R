stats_count_uni <- function(champ_quali, max_modalites = NULL, lib_modalite_autre = NULL, choix_multiple = FALSE, ...) {

  stats <- dplyr::tibble(champ_quali = champ_quali) %>%
    tidyr::drop_na(champ_quali) %>%
    dplyr::count(champ_quali)

  if (!is.null(max_modalites) | choix_multiple == TRUE) {
    stats <- stats %>%
      dplyr::mutate(champ_quali = as.character(champ_quali))
  }

  if (!is.factor(stats$champ_quali)) {
    stats <- dplyr::arrange(stats, -.data$n)
  }

  if (!is.null(max_modalites)) {

    if (nrow(stats) > max_modalites) {
      stats <- stats %>%
        dplyr::filter(dplyr::row_number() <= max_modalites - 1) %>%
        dplyr::bind_rows(
          dplyr::tibble(
            champ_quali = ifelse(!is.null(lib_modalite_autre), lib_modalite_autre, "Autres modalit\u00E9s"),
            n = dplyr::filter(stats, dplyr::row_number() >= max_modalites) %>%
              dplyr::pull(.data$n) %>%
              sum()
          )
        )
    }
  }

  stats <- dplyr::mutate(stats, pct = scales::percent(.data$n / sum(stats$n), suffix = "\u202F%", ...))

  if (choix_multiple == TRUE) {

    stats <- stats %>%
      dplyr::arrange(-.data$n) %>%
      dplyr::mutate(
        n = ifelse(is.na(.data$n), 0, .data$n),
        ordre = -dplyr::row_number()
      )
  }

  return(stats)
}

stats_count_bi <- function(champ_quali, champ_x, identifiant = NULL, complet = FALSE, ...) {

  stats <- dplyr::tibble(champ_x = champ_x, champ_quali = champ_quali) %>%
    tidyr::drop_na(champ_quali) %>%
    dplyr::count(champ_quali, champ_x) %>%
    dplyr::group_by(champ_x) %>%
    dplyr::mutate(pos = cumsum(.data$n) - 0.5 * .data$n,
                  pct = scales::percent(.data$n / sum(.data$n, na.rm = TRUE), suffix = "\u202F%", ...)) %>%
    dplyr::ungroup()

  if (complet == TRUE) {

    complet <- expand.grid(factor(levels(champ_x), levels(champ_x)), factor(levels(champ_quali), levels(champ_quali)))
    names(complet) <- c("champ_x", "champ_quali")

    stats <- dplyr::mutate(stats, pct = scales::percent(.data$n / length(unique(identifiant)), suffix = "\u202F%")) %>%
      dplyr::full_join(complet, by = c("champ_x", "champ_quali")) %>%
      dplyr::arrange(champ_x, champ_quali) %>%
      dplyr::mutate(n = ifelse(is.na(.data$n), 0, .data$n))

  }

  return(stats)

}

pct_repondants <- function(repondants, total) {

  if (repondants != total) {
    n_repondants <- paste0("R\u00E9pondants : ", repondants, " (", scales::percent(repondants / total, suffix = "\u202F%"), ")")
  } else {
    n_repondants <- NULL
  }

  return(n_repondants)

}

echelle_integer <- function(champ, n = 5) {

  if (length(champ) == 0) return(champ)

  echelle <- 0:max(champ)

  if (length(echelle) / n > 1.5) {
    echelle <- pretty(0:max(champ), n = n)
  }

  return(echelle)
}
