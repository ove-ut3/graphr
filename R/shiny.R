#' shiny_barplot_horizontal
#'
#' @param var \dots
#' @param colors \dots
#' @param alpha \dots
#' @param font_family \dots
#'
#' @export
shiny_barplot_horizontal <- function(var, colors = NULL, alpha = 1, font_family = NULL) {

  if (is.null(colors)) {
    colors <- rev(shiny_colors(length(unique(var))))
  }

  data <- dplyr::tibble(
    var
  ) %>%
    dplyr::count(var) %>%
    dplyr::group_by() %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::ungroup()

  if (!is.factor(var)) {

    data <- data %>%
      dplyr::arrange(-.data$n) %>%
      dplyr::mutate(
        var_trunc = stringr::str_sub(var, 1, 50),
        var = dplyr::if_else(.data$var_trunc == var, var, stringr::str_c(.data$var_trunc, "..."))
      ) %>%
      dplyr::mutate_at("var", ~ factor(., levels = var))

    if ("Autre" %in% levels(data$var)) {
      data <- dplyr::mutate_at(data, "var", forcats::fct_relevel, "Autre", after = Inf)
    }

    data <- dplyr::arrange(data, var)

  }

  data %>%
    dplyr::mutate_at("var", forcats::fct_rev) %>%
    plotly::plot_ly(
      type = 'bar', x = ~pct, y = ~var,
      color = ~var,
      colors = rev(colors),
      opacity = alpha,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Valeur: ", var,
        "<br>Effectif: ", scales::number(n, accuracy = 1, big.mark = "\u202F"),
        "<br>Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")
      )
    ) %>%
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE, ticksuffix = "%"),
      yaxis = list(title = "", showgrid = FALSE),
      showlegend = FALSE,
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_barplot_horizontal
#'
#' @param var \dots
#' @param colors \dots
#' @param alpha \dots
#' @param font_family \dots
#'
#' @export
shiny_barplot_vertical <- function(var, colors = NULL, alpha = 1, font_family = NULL) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var)))
  }

  data <- dplyr::tibble(
    var
  ) %>%
    dplyr::count(var) %>%
    dplyr::group_by() %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::ungroup()

  if (!is.factor(var)) {
    data <- data %>%
      dplyr::arrange(-.data$n) %>%
      dplyr::mutate_at("var", ~ factor(., levels = var))

      if ("Autre" %in% levels(data$var)) {
        data <- dplyr::mutate_at(data, "var", forcats::fct_relevel, "Autre", after = Inf)
      }
  }

  data %>%
    plotly::plot_ly(
      type = 'bar', x = ~var, y = ~pct,
      color = ~var,
      colors = colors,
      opacity = alpha,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Valeur: ", var,
        "<br>Effectif: ", scales::number(n, accuracy = 1, big.mark = "\u202F"),
        "<br>Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")
      )
    ) %>%
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = "", showgrid = FALSE, ticksuffix = "%"),
      showlegend = FALSE
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_pie
#'
#' @param var \dots
#' @param colors \dots
#' @param alpha \dots
#' @param donut \dots
#' @param donut_title \dots
#' @param legend_position \dots
#' @param font_family \dots
#'
#' @export
shiny_pie <- function(var, colors = NULL, alpha = 1, donut = FALSE, donut_title = "", legend_position = c("right", "bottom"), font_family = NULL) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var)))
  }

  data <- dplyr::tibble(var) %>%
    dplyr::count(var) %>%
    dplyr::group_by() %>%
    dplyr::mutate(pct = .data$n / sum(.data$n)) %>%
    dplyr::ungroup()

  if (class(var) != "factor") {
    data <- data %>%
      dplyr::arrange(-.data$n) %>%
      dplyr::mutate_at("var", ~ factor(., levels = var))

    if ("Autre" %in% levels(data$var)) {
      data <- dplyr::mutate_at(data, "var", forcats::fct_relevel, "Autre", after = Inf)
    }

    data <- dplyr::arrange(data, var)

  }

  data %>%
    dplyr::mutate_at("pct", scales::percent, decimal.mark = ",", suffix = "\u202F%", accuracy = 1) %>%
    dplyr::mutate(pct = dplyr::if_else(.data$pct == "0\u202F%", "<\u202F1\u202F%", .data$pct)) %>%
    dplyr::mutate(effectif = scales::number(.data$n, accuracy = 1, big.mark = "\u202F")) %>%
    plotly::plot_ly(
      labels = ~var, values = ~n,
      sort = FALSE,
      direction = "clockwise",
      textinfo = "text",
      text = ~pct,
      hoverinfo = "text",
      hovertext = ~ paste0(
        "Valeur: ", var,
        "<br>Effectif: ", effectif,
        "<br>Pourcentage: ", pct
      ),
      marker = list(colors = colors),
      opacity = alpha
    ) %>%
    plotly::add_pie(hole = ifelse(donut, 0.6, 0)) %>%
    plotly::layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      annotations = list(text = glue::glue("<b>{donut_title}</b>"), font = list(size = 15), showarrow = FALSE),
      legend = list(
        orientation = ifelse(legend_position == "right", "v", "h"),
        y = ifelse(legend_position == "right", 0.5, -0.1)
      ),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_line_base100
#'
#' @param var_year \dots
#' @param var_value \dots
#' @param title_x \dots
#' @param title_y \dots
#' @param note_base100 \dots
#' @param color \dots
#' @param font_family \dots
#'
#' @export
shiny_line_base100 <- function(var_year, var_value, title_x = "", title_y = "", note_base100 = "", color = NULL, font_family = NULL) {

  data <- dplyr::tibble(var_year, var_value) %>%
    dplyr::mutate(base_100 = graphr::base_100(var_value))

  data %>%
    plotly::plot_ly(
      x = ~var_year,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Valeur: ", var_value,
        "<br>Base 100: ", format(round(base_100, digits = 1), decimal.mark = ",")
      )
    ) %>%
    plotly::add_lines(y = ~base_100, name = "linear", line = list(shape = "linear", color = color)) %>%
    plotly::layout(
      xaxis = list(title = title_x),
      yaxis = list(title = title_y),
      margin = list(r = 50, b = 50),
      annotations = list(
        text = note_base100, xref = 'paper', yref = 'paper',
        x = 1.08, y = -0.16, xanchor = 'right', yanchor = 'auto',
        showarrow = FALSE
      ),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_line_percent
#'
#' @param var_year \dots
#' @param var_percent \dots
#' @param title_x \dots
#' @param title_y \dots
#' @param hovertext \dots
#' @param color \dots
#' @param font_family \dots
#'
#' @export
shiny_line_percent <- function(var_year, var_percent, title_x = "", title_y = "", hovertext = NULL, color = NULL, font_family = NULL) {

  dplyr::tibble(
    var_year,
    var_percent
  ) %>%
    dplyr::mutate_at("var_percent", ~ . * 100) %>%
    plotly::plot_ly(
      x = ~var_year,
      hoverinfo = "text",
      hovertext = hovertext
    ) %>%
    plotly::add_lines(y = ~var_percent, name = "linear", line = list(shape = "linear", color = color)) %>%
    plotly::layout(
      xaxis = list(title = title_x),
      yaxis = list(title = title_y, rangemode = "tozero", ticksuffix = "\u202F%"),
      margin = list(r = 50),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_line_percent
#'
#' @param var_year \dots
#' @param var_line \dots
#' @param var_percent \dots
#' @param title_x \dots
#' @param title_y \dots
#' @param colors \dots
#' @param font_family \dots
#'
#' @export
shiny_line_percent_multi <- function(var_year, var_line, var_percent, title_x = "", title_y = "", colors = NULL, font_family = NULL) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_line)))
  }

  data <- dplyr::tibble(
    var_year,
    var_line,
    var_percent
  ) %>%
    dplyr::mutate_at("var_percent", graphr::round_100) %>%
    tidyr::nest_legacy(-var_line)

  plot <- data %>%
    plotly::plot_ly(
      x = ~var_year
    )

  for (num_line in 1:nrow(data)) {
    plot <- plot %>%
      plotly::add_lines(y = ~var_percent, data = data$data[[num_line]], name = data$var_line[num_line], line = list(shape = "linear", color = colors[num_line]))
  }

  plot <- plot %>%
    plotly::layout(
      hovermode = 'x',
      xaxis = list(title = title_x),
      yaxis = list(title = title_y, ticksuffix = "%", rangemode = "tozero"),
      legend = list(y = 0.5),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

  plot

}

#' shiny_areas_evolution
#'
#' @param var_x \dots
#' @param var_y \dots
#' @param colors \dots
#' @param title_x \dots
#' @param title_y \dots
#' @param font_family \dots
#'
#' @export
shiny_areas_evolution <- function(var_x, var_y, colors = NULL, title_x = "", title_y = "", font_family = NULL) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_y)))
  }

  data <- dplyr::tibble(
    var_x,
    var_y,
  ) %>%
    dplyr::count(var_x, var_y) %>%
    tidyr::spread(var_y, .data$n, fill = 0) %>%
    tidyr::gather("var_y", "n", -var_x)

  if (class(var_y) == "factor") {

    levels <- levels(var_y)

  } else {

    levels <- data %>%
      dplyr::group_by(var_y) %>%
      dplyr::summarise(n_total = sum(.data$n)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(.data$n_total)) %>%
      dplyr::pull(var_y)

  }

  data %>%
    dplyr::mutate_at("var_y", factor, levels) %>%
    dplyr::arrange(var_x, var_y) %>%
    dplyr::group_by(var_x) %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::mutate(cumsum = cumsum(.data$pct)) %>%
    dplyr::ungroup() %>%
    plotly::plot_ly(
      type = 'scatter', x = ~var_x, y = ~cumsum, color = ~var_y, colors = colors,
      mode = 'none', fill = 'tonexty',
      hoverinfo = "text",
      hovertext = ~ paste(
        stringr::str_c(dplyr::na_if(title_x, ""), ": ", var_x),
        var_y,
        paste("Effectif: ", scales::number(.data$n, accuracy = 1, big.mark = "\u202F")),
        paste("Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")),
        sep = "<br>"
      )
    ) %>%
    plotly::layout(
      xaxis = list(title = title_x, showgrid = FALSE),
      yaxis = list(title = title_y, showgrid = FALSE, ticksuffix = "%"),
      legend = list(y = 0.5),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_barplot_vertical_multi
#'
#' @param var_x \dots
#' @param var_y \dots
#' @param colors \dots
#' @param alpha \dots
#' @param title_x \dots
#' @param title_y \dots
#' @param font_family \dots
#'
#' @export
shiny_barplot_vertical_multi <- function(var_x, var_y, colors = NULL, alpha = 1, title_x = "", title_y = "", font_family = NULL) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_y)))
  }

  dplyr::tibble(
    var_x,
    var_y,
  ) %>%
    dplyr::count(var_x, var_y) %>%
    dplyr::group_by(var_x) %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::ungroup() %>%
    plotly::plot_ly(
      type = 'bar', x = ~var_x, y = ~pct, color = ~var_y,
      colors = colors,
      opacity = alpha,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Valeur: ", var_y,
        "<br>Effectif: ", scales::number(n, accuracy = 1, big.mark = "\u202F"),
        "<br>Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")
      )
    ) %>%
    plotly::layout(
      barmode = 'stack',
      xaxis = list(title = title_x, showgrid = FALSE),
      yaxis = list(title = title_y, showgrid = FALSE, ticksuffix = "%"),
      legend = list(y = 0.5),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_barplot_horizontal_multi
#'
#' @param var_x \dots
#' @param var_y \dots
#' @param colors \dots
#' @param alpha \dots
#' @param title_x \dots
#' @param title_y \dots
#' @param font_family \dots
#'
#' @export
shiny_barplot_horizontal_multi <- function(var_x, var_y, colors = NULL, alpha = 1, title_x = "", title_y = "", font_family = NULL) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_y)))
  }

  dplyr::tibble(
    var_x,
    var_y,
  ) %>%
    dplyr::count(var_x, var_y) %>%
    dplyr::group_by(var_x) %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("var_x", factor, levels = rev(levels(.$var_x))) %>%
    plotly::plot_ly(
      type = 'bar', x = ~pct, y = ~var_x, color = ~var_y,
      colors = colors,
      opacity = alpha,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Valeur: ", var_y,
        "<br>Effectif: ", scales::number(n, accuracy = 1, big.mark = "\u202F"),
        "<br>Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")
      )
    ) %>%
    plotly::layout(
      barmode = 'stack',
      xaxis = list(title = title_x, showgrid = FALSE, ticksuffix = "%"),
      yaxis = list(title = title_y, showgrid = FALSE),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        traceorder = "normal"
      ),
      font = list(family = font_family)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_treemap
#'
#' @param var_x \dots
#' @param colors \dots
#' @param alpha \dots
#' @param font_family \dots
#'
#' @export
shiny_treemap <- function(var_x, colors = NULL, alpha = 1, font_family = NULL) {

  if (is.null(colors)) {
    colors <- graphr::shiny_colors(length(unique(var_x)))
  }

  dplyr::tibble(
    labels = var_x
  ) %>%
    dplyr::mutate(parents = character(nrow(.))) %>%
    dplyr::count(labels, .data$parents) %>%
    dplyr::group_by() %>%
    dplyr::mutate(pct = .data$n / sum(.data$n) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("pct", graphr::round_100) %>%
    dplyr::mutate_at("pct", ~ . / 100) %>%
    dplyr::mutate_at("pct", ~ dplyr::if_else(. == 0, "< 1\u202F%", scales::percent(., accuracy = 1, decimal.mark = ",", suffix = "\u202F%"))) %>%
    dplyr::mutate(effectif = scales::number(.data$n, accuracy = 1, big.mark = "\u202F")) %>%
    dplyr::mutate(labels_pct = glue::glue("{labels} ({pct})")) %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      type = "treemap",
      labels = ~labels_pct,
      parents = ~parents,
      values = ~n,
      hoverinfo = "text",
      hovertext = ~glue::glue("Valeur: {labels}\nEffectif: {effectif}\nPourcentage: {pct}"),
      marker = list(colors = colors),
      opacity = alpha
    ) %>%
    plotly::layout(
      uniformtext = list(
        minsize = 15,
        mode = "hide",
        family = font_family
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_treemap_bi
#'
#' @param parents \dots
#' @param labels \dots
#' @param colors \dots
#' @param alpha \dots
#' @param font_family \dots
#'
#' @export
shiny_treemap_bi <- function(parents, labels, colors = NULL, alpha = 1, font_family = NULL) {

  data <- dplyr::tibble(
    parents,
    labels
  )

  if (is.null(colors)) {
    colors <- graphr::shiny_colors(length(unique(parents)))
  } else {
    colors <- utils::head(colors, length(unique(parents)))
  }

  data_parents <- data %>%
    dplyr::count(parents, sort = TRUE) %>%
    dplyr::rename(labels = parents) %>%
    dplyr::mutate(
      parents = "",
      color = colors
    )

  data_labels <- data %>%
    dplyr::count(parents, labels) %>%
    dplyr::filter(parents != labels) %>%
    dplyr::left_join(
      data_parents %>%
        dplyr::select(labels, .data$color),
      by = c("parents" = "labels")
    )

  data_plot <- dplyr::bind_rows(
    data_labels,
    data_parents
  ) %>%
    dplyr::mutate(pct = .data$n / nrow(data) * 100) %>%
    dplyr::mutate_at("pct", graphr::round_100) %>%
    dplyr::mutate_at("pct", ~ . / 100) %>%
    dplyr::mutate_at("pct", ~ dplyr::if_else(. == 0, "< 1\u202F%", scales::percent(., decimal.mark = ",", accuracy = 1, suffix = "\u202F%"))) %>%
    dplyr::mutate(effectif = scales::number(.data$n, accuracy = 1, big.mark = "\u202F"))

  data_plot %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      type = "treemap",
      labels = ~labels,
      parents = ~parents,
      values = ~n,
      branchvalues = "total",
      hoverinfo = "text",
      hovertext = ~glue::glue("Valeur: {labels}\nEffectif: {effectif}\nPourcentage: {pct}"),
      marker = list(colors = data_plot$color),
      opacity = alpha
    ) %>%
    plotly::layout(
      uniformtext = list(
        minsize = 10,
        mode = "hide",
        family = font_family
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)

}
