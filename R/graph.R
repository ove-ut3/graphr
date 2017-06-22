#' quali_uni
#'
#' @param table
quali_uni <- function(table, choix_multiple = FALSE, marge_gauche = FALSE, taille_texte = 3.5) {

  if (nrow(table) == 0) {
    if (class(table$champ_quali) == "factor") {
      table <- table %>%
        dplyr::add_row(champ_quali = tail(levels(table$champ_quali), 1),
                       n = 0)
    } else {
      cat("effectif nul")
      return("")
    }
  }

  if (class(table$champ_quali) != "factor") {
    if (is.null(table[["ordre"]])) {
      table <- dplyr::mutate(table, ordre = row_number())
    }
    plot <- ggplot2::ggplot(table, ggplot2::aes(x = reorder(champ_quali, -ordre), y = n, fill = champ_quali))
  } else {
    plot <- ggplot2::ggplot(table, ggplot2::aes(x = champ_quali, y = n, fill = champ_quali))
  }

  plot <- plot +
    ggplot2::geom_col(show.legend = FALSE, width = 0.5) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = echelle_integer(table$n)) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()

  if (choix_multiple == TRUE) {
    plot <- plot + ggplot2::geom_text(stat = "identity", size = taille_texte, aes(y = 0.2, hjust = 0, label = ifelse(n >= 1, format(n, big.mark = " "), "")))
  } else if (choix_multiple == FALSE) {
    plot <- plot + ggplot2::geom_text(stat = "identity", size = taille_texte, aes(y = 0.2, hjust = 0, label = paste0(format(n, big.mark = " "), " (", trimws(format(round(n / sum(n) * 100, 1), decimal.mark = ",")), "%)")))
  }

  if (marge_gauche == TRUE) {
    plot <- plot + ggplot2::labs(x = "", y = NULL)
  } else {
    plot <- plot + ggplot2::labs(x = NULL, y = NULL)
  }

  return(plot)
}

#' quali_bi_aires
#'
#' @param table
quali_bi_aires <- function(table, levels_x, levels_quali, label_pourcentage = FALSE, position_legende = "bas", taille_texte_legende = 1, nombre_lignes_legende = NULL, palette_ordinal = FALSE) {

  if (nrow(table) == 0) {
    if (class(table$champ_x) != "factor") {
      table <- table %>%
        dplyr::add_row(champ_x = tail(levels(table$champ_x), 1),
                       n = 0)
    } else {
      cat("effectif nul")
      return("")
    }
  }

  if (position_legende == "bas") {
    plot <- ggplot2::ggplot(table, ggplot2::aes(x = champ_x, y = n, fill = factor(champ_quali, levels = rev(levels_quali)), order = rev(champ_quali)))
  } else if (position_legende == "droite") {
    plot <- ggplot2::ggplot(table, ggplot2::aes(x = champ_x, y = n, fill = champ_quali))
  }

  echelle_y <- dplyr::group_by(table, champ_x) %>%
    dplyr::summarise(n = sum(n)) %>%
    .$n %>%
    graphr::echelle_integer()

  plot <- plot +
    ggplot2::geom_area() +
    ggplot2::scale_x_continuous(breaks = table$champ_x %>% unique, labels = levels_x) +
    ggplot2::scale_y_continuous(breaks = echelle_y) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(taille_texte_legende, 'lines'),
                   legend.text = ggplot2::element_text(size = 8),
                   legend.position = dplyr::recode(position_legende, "bas" = "bottom", "droite" = "right"),
                   legend.box.spacing = ggplot2::unit(1, "mm"))

  if (palette_ordinal == TRUE) {
    plot <- plot + ggplot2::scale_fill_brewer()
  } else {
    plot <- plot + ggplot2::scale_fill_discrete(direction = -1)
  }

  if (label_pourcentage == TRUE) {
    plot <- plot + ggplot2::geom_text(data = subset(table, n != 0), stat = "identity", ggplot2::aes(label = paste0(format(n, big.mark = " "), " (", pct,")"), y = pos), size = 3)
  } else {
    plot <- plot + ggplot2::geom_text(data = subset(table, n != 0), stat = "identity", ggplot2::aes(label = format(n, big.mark = " "), y = pos), size = 3)
  }

  if (position_legende == "bas") {
    plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE, byrow = TRUE))
    if (!is.null(nombre_lignes_legende)) {
      plot <- plot + ggplot2::guides(fill = ggplot2::guide_legend(nrow = nombre_lignes_legende, byrow = TRUE, reverse = TRUE))
    }
  }

  return(plot)
}

#' quali_bi_ordinal
#'
#' @param table

  if (nrow(table) == 0) {
    if (class(table$champ_quali) != "factor") {
      table <- table %>%
        dplyr::add_row(champ_quali = tail(levels(table$champ_quali), 1),
                       n = 0)
    } else {
      cat("effectif nul")
      return("")
    }
  }

  plot <- ggplot2::ggplot(table, ggplot2::aes(x = champ_quali, y = pct, fill = factor(valeur, levels = rev(levels_valeur)))) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::scale_fill_brewer() +
    ggplot2::geom_text(position = "stack", size = 3, ggplot2::aes(y = pos, label = format(n, big.mark = " "))) +
    ggplot2::scale_x_discrete(drop = FALSE, limits = rev(levels_y)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "bottom",
                   legend.box.spacing = ggplot2::unit(1, "mm"),
                   legend.key.size = ggplot2::unit(1, 'lines'),
                   legend.text = ggplot2::element_text(size = 8)) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE))

  return(plot)
}
