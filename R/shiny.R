#' shiny_donut
#'
#' @param var \dots
#' @param title \dots
#' @param colors \dots
#' @param alpha \dots
#'
#' @export
shiny_donut <- function(var, title = "", colors = NULL, alpha = 1) {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var)))
  }

  data <- dplyr::tibble(var) %>%
    dplyr::count(var)

  if (class(var) != "factor") {
    data <- dplyr::arrange(data, dplyr::desc(n))
  }

  data %>%
    dplyr::group_by() %>%
    dplyr::mutate(text = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("text", scales::percent, decimal.mark = ",", suffix = "\u202F%", accuracy = 1) %>%
    dplyr::mutate_at("text", dplyr::recode, "0\u202F%" = "<\u202F1\u202F%") %>%
    dplyr::mutate(effectif = scales::number(n, big.mark = "\u202F")) %>%
    plotly::plot_ly(
      labels = ~var, values = ~n,
      sort = FALSE,
      direction = "clockwise",
      textinfo = "text",
      text = ~text,
      hoverinfo = "text",
      hovertext = ~glue::glue("Effectif: {effectif}"),
      marker = list(colors = colors),
      opacity = alpha
    ) %>%
    plotly::add_pie(hole = 0.6) %>%
    plotly::layout(
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      annotations = list(text = glue::glue("<b>{title}</b>"), font = list(size = 15), showarrow = FALSE),
      legend = list(y = 0.5)
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
#'
#' @export
shiny_line_base100 <- function(var_year, var_value, title_x = "", title_y = "", note_base100 = "") {

  data <- dplyr::tibble(var_year, var_value) %>%
    dplyr::mutate(base_100 = graphr::base_100(var_value))

  data %>%
    plotly::plot_ly(
      x = ~var_year,
      hoverinfo = "text",
      hovertext = ~paste0("Valeur: ", var_value,
                          "<br>Base 100: ", round(base_100, digits = 1))
    ) %>%
    plotly::add_lines(y = ~base_100, name = "linear", line = list(shape = "linear")) %>%
    plotly::layout(
      xaxis = list(title = title_x),
      yaxis = list(title = title_y),
      margin = list(r = 50, b = 50),
      annotations = list(text = note_base100, xref = 'paper', yref = 'paper',
                         x = 1.08, y = -0.16, xanchor = 'right', yanchor = 'auto',
                         showarrow = FALSE)
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
#'
#' @export
shiny_line_percent <- function(var_year, var_percent, title_x = "", title_y = "", hovertext = NULL) {

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
    plotly::add_lines(y = ~var_percent, name = "linear", line = list(shape = "linear")) %>%
    plotly::layout(
      xaxis = list(title = title_x),
      yaxis = list(title = title_y, rangemode = "tozero", ticksuffix = "\u202F%"),
      margin = list(r = 50)
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_line_percent
#'
#' @param var_year \dots
#' @param var_percent \dots
#' @param title_x \dots
#' @param title_y \dots
#'
#' @export
shiny_line_percent_multi <- function(var_year, var_line, var_percent, title_x = "", title_y = "") {

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
      plotly::add_lines(y = ~var_percent, data = data$data[[num_line]], name = data$var_line[num_line], line = list(shape = "linear"))
  }

  plot <- plot %>%
    plotly::layout(
      hovermode = 'x',
      xaxis = list(title = title_x),
      yaxis = list(title = title_y, ticksuffix = "%", rangemode = "tozero"),
      legend = list(y = 0.5)
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
#'
#' @export
shiny_areas_evolution <- function(var_x, var_y, colors = NULL, title_x = "", title_y = "") {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_y)))
  }

  data <- dplyr::tibble(
    var_x,
    var_y,
  ) %>%
    dplyr::count(var_x, var_y) %>%
    tidyr::spread(var_y, n, fill = 0) %>%
    tidyr::gather("var_y", "n", -var_x)

  if (class(var_y) == "factor") {

    levels <- levels(var_y)

  } else {

    levels <- data %>%
      dplyr::group_by(var_y) %>%
      dplyr::summarise(n_total = sum(n)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(n_total)) %>%
      dplyr::pull(var_y)

  }

  data %>%
    dplyr::mutate_at("var_y", factor, levels) %>%
    dplyr::arrange(var_x, var_y) %>%
    dplyr::group_by(var_x) %>%
    dplyr::mutate(pct = n / sum(n) * 100) %>%
    dplyr::mutate(cumsum = cumsum(pct)) %>%
    dplyr::ungroup() %>%
    plotly::plot_ly(
      type = 'scatter', x = ~var_x, y = ~cumsum, color = ~var_y, colors = colors,
      mode = 'none', fill = 'tonexty',
      hoverinfo = "text",
      hovertext = ~ paste(
        stringr::str_c(dplyr::na_if(title_x, ""), ": ", var_x),
        paste("Effectif: ", scales::number(n, big.mark = "\u202F")),
        paste("Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")),
        sep = "<br>")
    ) %>%
    plotly::layout(
      xaxis = list(title = title_x, showgrid = FALSE),
      yaxis = list(title = title_y, showgrid = FALSE, ticksuffix = "%"),
      legend = list(y = 0.5)
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
#'
#' @export
shiny_barplot_vertical_multi <- function(var_x, var_y, colors = NULL, alpha = 1, title_x = "", title_y = "") {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_y)))
  }

  dplyr::tibble(
    var_x,
    var_y,
  ) %>%
    dplyr::count(var_x, var_y) %>%
    dplyr::group_by(var_x) %>%
    dplyr::mutate(pct = n / sum(n) * 100) %>%
    dplyr::ungroup() %>%
    plotly::plot_ly(
      type = 'bar', x = ~var_x, y = ~pct, color = ~var_y,
      colors = colors,
      opacity = alpha,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Effectif: ", scales::number(n, big.mark = "\u202F"),
        "<br>Pourcentage: ", scales::percent(pct / 100, accuracy = 0.1, decimal.mark = ",", suffix = "\u202F%")
      )
    ) %>%
    plotly::layout(
      barmode = 'stack',
      xaxis = list(title = title_x, showgrid = FALSE),
      yaxis = list(title = title_y, showgrid = FALSE, ticksuffix = "%"),
      legend = list(y = 0.5)
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
#'
#' @export
shiny_barplot_horizontal_multi <- function(var_x, var_y, colors = NULL, alpha = 1, title_x = "", title_y = "") {

  if (is.null(colors)) {
    colors <- shiny_colors(length(unique(var_y)))
  }

  dplyr::tibble(
    var_x,
    var_y,
  ) %>%
    dplyr::count(var_x, var_y) %>%
    dplyr::group_by(var_x) %>%
    dplyr::mutate(pct = n / sum(n) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("var_x", factor, levels = rev(levels(.$var_x))) %>%
    plotly::plot_ly(
      type = 'bar', x = ~pct, y = ~var_x, color = ~var_y,
      colors = colors,
      opacity = alpha,
      hoverinfo = "text",
      hovertext = ~paste0(
        "Effectif: ", scales::number(n, big.mark = "\u202F"),
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
      )
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_treemap
#'
#' @param var_x \dots
#' @param colors \dots
#' @param alpha \dots
#'
#' @export
shiny_treemap <- function(var_x, colors = NULL, alpha = 1) {

  dplyr::tibble(
    labels = var_x
  ) %>%
    dplyr::mutate(parents = character(nrow(.))) %>%
    dplyr::count(labels, parents) %>%
    dplyr::group_by() %>%
    dplyr::mutate(pct = n / sum(n) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("pct", graphr::round_100) %>%
    dplyr::mutate_at("pct", ~ . / 100) %>%
    dplyr::mutate_at("pct", ~ dplyr::if_else(. == 0, "< 1\U202F%", scales::percent(., decimal.mark = ",", suffix = "\u202F%"))) %>%
    dplyr::mutate(effectif = scales::number(n, big.mark = "\u202F")) %>%
    dplyr::mutate(labels = glue::glue("{labels} ({pct})")) %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      type = "treemap",
      labels = ~labels,
      parents = ~parents,
      values = ~n,
      hoverinfo = "text",
      hovertext = ~glue::glue("Effectif: {effectif}"),
      marker = list(colors = graphr::shiny_colors(length(unique(var_x)))),
      opacity = alpha
    ) %>%
    plotly::config(displayModeBar = FALSE)

}

#' shiny_treemap_bi
#'
#' @param parents \dots
#' @param labels \dots
#' @param colors \dots
#' @param alpha \dots
#'
#' @export
shiny_treemap_bi <- function(parents, labels, colors = NULL, alpha = 1) {

  data <- dplyr::tibble(
    parents,
    labels
  ) %>%
    dplyr::count(parents, labels) %>%
    dplyr::mutate(parents = dplyr::if_else(parents == labels, "", parents))

  data %>%
    dplyr::bind_rows(
      data %>%
        dplyr::filter(parents != "") %>%
        dplyr::group_by(parents) %>%
        dplyr::summarise_at("n", sum) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          labels = parents,
          parents = ""
        )
    ) %>%
    dplyr::group_by(labels) %>%
    dplyr::mutate(pct = n / sum(data$n) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_at("pct", graphr::round_100) %>%
    dplyr::mutate_at("pct", ~ . / 100) %>%
    dplyr::mutate_at("pct", ~ dplyr::if_else(. == 0, "< 1\U202F%", scales::percent(., decimal.mark = ",", suffix = "\u202F%"))) %>%
    dplyr::mutate(effectif = scales::number(n, big.mark = "\u202F")) %>%
    plotly::plot_ly() %>%
    plotly::add_trace(
      type = "treemap",
      labels = ~labels,
      parents = ~parents,
      values = ~n,
      branchvalues = "total",
      hoverinfo = "text",
      hovertext = ~glue::glue("{labels}\nEffectif: {effectif}\nPourcentage: {pct}"),
      marker = list(colors = graphr::shiny_colors(length(unique(parents)))),
      opacity = alpha
    ) %>%
    plotly::config(displayModeBar = FALSE)

}