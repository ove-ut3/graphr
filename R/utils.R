stats_count_uni <- function(champ_quali, max_modalites = NULL, lib_modalite_autre = NULL, choix_multiple = FALSE, pct_arrondi = 1) {

  stats <- dplyr::tibble(champ_quali = champ_quali) %>%
    tidyr::drop_na(champ_quali) %>%
    dplyr::count(champ_quali)

  if (!is.null(max_modalites) | choix_multiple == TRUE) {
    stats <- stats %>%
      dplyr::mutate(champ_quali = as.character(champ_quali))
  }

  if (!is.factor(stats$champ_quali)) {
    stats <- dplyr::arrange(stats, -n)
  }

  if (!is.null(max_modalites)) {

    if (nrow(stats) > max_modalites) {
      stats <- dplyr::filter(stats, dplyr::row_number() <= max_modalites - 1) %>%
        dplyr::bind_rows(dplyr::tibble(champ_quali = ifelse(!is.null(lib_modalite_autre), lib_modalite_autre, "Autres modalités"),
                                n = dplyr::filter(stats, dplyr::row_number() >= max_modalites) %>%
                                  dplyr::pull(n) %>%
                                  sum()))
    }
  }

  stats <- dplyr::mutate(stats, pct = caractr::str_percent(n / sum(stats$n), digits = pct_arrondi))

  if (choix_multiple == TRUE) {

    stats <- stats %>%
      dplyr::arrange(-n) %>%
      dplyr::mutate(n = ifelse(is.na(n), 0, n),
                    ordre = -dplyr::row_number())
  }

  return(stats)
}

stats_count_bi <- function(champ_quali, champ_x, identifiant = NULL, complet = FALSE) {

  stats <- dplyr::tibble(champ_x = champ_x, champ_quali = champ_quali) %>%
    tidyr::drop_na(champ_quali) %>%
    dplyr::count(champ_quali, champ_x) %>%
    dplyr::group_by(champ_x) %>%
    dplyr::mutate(pos = cumsum(n) - 0.5 * n,
                  pct = caractr::str_percent(n / sum(n, na.rm = TRUE))) %>%
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

pct_repondants <- function(repondants, total) {

  if (repondants != total) {
    n_repondants <- paste0("Répondants : ", repondants, " (", format(round(repondants / total * 100, 1)), " %)")
  } else {
    n_repondants <- NULL
  }

  return(n_repondants)

}

echelle_integer <- function(champ, n = 5) {

  if (length(champ) == 0) return(champ)

  echelle <- 0:max(champ)

  if (length(echelle) / n > 1.5) {

    #echelle <- seq(0, max(echelle), by = n)
    echelle <- pretty(0:max(champ), n = n)
  }

  return(echelle)
}

stats_count_histo <- function(data, var) {

  stats <- data %>%
    dplyr::count(!!dplyr::enquo(var)) %>%
    dplyr::group_by() %>%
    dplyr::mutate(evol = ifelse(dplyr::row_number() == 1,
                                NA_real_,
                                (n - dplyr::lag(n)) / dplyr::lag(n)) %>%
                    caractr::str_percent(sign = TRUE),
                  base_100 = 100 + (100 * (n - dplyr::first(n)) / dplyr::first(n)))

  return(stats)
}
