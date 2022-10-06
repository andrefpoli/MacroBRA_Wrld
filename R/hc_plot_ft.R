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
      style   = list(color ="black",fontSize = "12px")
      ) %>%
    hc_xAxis(title = FALSE,
      labels    = list(color ="black",style = list(fontSize = "12px"))
      )%>%
    hc_yAxis(
      labels    = list(color ="black",format = "{value}%",style = list(fontSize = "14px")),
      title     = FALSE,
      plotLines = list(
        list(
          value = 0,
          color = "#0D0808",
          width = 1
          )
        )
      ) %>%
    hc_tooltip(shared = TRUE) %>%
    hc_add_theme(theme_DYH_2) %>%
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


#1a1a1a
