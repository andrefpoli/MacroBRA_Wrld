# create theme
theme_fortietwo <- highcharter::hc_theme_merge(
  highcharter::hc_theme_elementary(),
  highcharter::hc_theme(
    chart = list(style = list(fontFamily = "Open Sans", color = "#333")),
    title = list(
      style = list(fontFamily = "Open Sans", color = "black", fontWeight = "bold"),
      align = "center"
    ),
    subtitle = list(
      style = list(fontFamily = "Open Sans", fontWeight = "bold"),
      align = "center"
    ),
    legend = list(align = "center", verticalAlign = "bottom"),
    xAxis = list(
      gridLineWidth      = 1,
      gridLineColor      = "#F3F3F3",
      lineColor          = "#F3F3F3",
      minorGridLineColor = "#F3F3F3",
      tickColor          = "#F3F3F3",
      tickWidth          = 1
    ),
    yAxis = list(
      gridLineColor      = "#F3F3F3",
      lineColor          = "#F3F3F3",
      minorGridLineColor = "#F3F3F3",
      tickColor          = "#F3F3F3",
      tickWidth          = 1
    )
  )
)

theme_DYH <- hc_theme_merge(
  hc_theme_darkunica(),
  hc_theme(
    chart = list(
      backgroundColor = 'transparent',
      divBackgroundImage = "http://www.wired.com/images_blogs/underwire/2013/02/xwing-bg.gif"
    ),
    title = list(
      style = list(color = "white",fontFamily = "Open Sans",fontSize = "25px")
    ),
    subtitle = list(
      style = list(color = "white",fontFamily = "Open Sans",fontSize = "15px"),
      align = "center"
    ),
    legend = list(
      itemStyle = list(color = "white",fontFamily = "Open Sans",fontSize = "15px")
    )
  )
)


theme_DYH_2 <- hc_theme_merge(
  hc_theme_ft(),
  hc_theme(
    chart = list(style = list(fontFamily = "Droid Serif", color = "#333")),
    title = list(
      style = list(fontFamily = "Droid Serif", color = "black", fontWeight = "bold"),
      align = "center"
    ),
    subtitle = list(
      style = list(fontFamily = "Droid Serif", fontWeight = "bold"),
      align = "center"
    ),
    legend = list(align = "center", verticalAlign = "bottom"),
    xAxis = list(
      align = "center",
      gridLineDashStyle = "Dot",
      gridLineWidth      = 1,
      gridLineColor      = "##CEC6B9",
      lineColor          = "#CEC6B9",
      minorGridLineColor = "#CEC6B9",
      tickColor          = "#CEC6B9",
      tickWidth          = 1
    ),
    yAxis = list(
      align = "center",
      gridLineDashStyle = "Dot",
      gridLineColor      = "#CEC6B9",
      lineColor          = "#CEC6B9",
      minorGridLineColor = "#CEC6B9",
      tickColor          = "#CEC6B9",
      tickWidth          = 1
    )
  )
)
