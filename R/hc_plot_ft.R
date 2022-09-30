#' Easy plot highcharter personal theme
#'
#' @param hc highchart object
#' @param title title to the plot
#' @param subtitle subtitle to the plot
#' @param source source of data
#' @param range logical, display range options?
#' @param navigator logical, display navigator zoom?
#' @import highcharter
#'
#' @return plot
#' @export
#'
hc_plot_ft <- function(
  hc,
  title,
  subtitle,
  source,
  range     = TRUE,
  navigator = TRUE
  )

  {
  # create plot object
  dyh_plot <- hc %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_credits(
      enabled = TRUE,
      text    = paste0("Source: ", source),
      style   = list(color ="white",fontSize = "12px")
      ) %>%
    hc_xAxis(title = FALSE,
      labels    = list(color ="white",style = list(fontSize = "12px"))
      )%>%
    hc_yAxis(
      labels    = list(color ="white",format = "{value}%",style = list(fontSize = "14px")),
      title     = FALSE,
      plotLines = list(
        list(
          value = 0,
          color = "#1a1a1a",
          width = 2
          )
        )
      ) %>%
    hc_tooltip(shared = TRUE) %>%
    hc_add_theme(theme_DYH) %>%
    hc_navigator(enabled = navigator)


    if (range) {
      dyh_plot <- dyh_plot %>%
      hc_rangeSelector(
        selected          = 4,
        enabled           = TRUE,
        allButtonsEnabled = TRUE,
        inputEnabled      = FALSE,
        dropdown          = "always"
      )
    } else dyh_plot

  dyh_plot

}



