plotly_piechart = function(df, variable, colors, title = "") {
  df = df %>%
    group_by(get(variable)) %>%
    summarise(count = n()) %>%
    rename("variable" = "get(variable)")

  annot = list(
    x = 0,
    y = 0 ,
    text = gsub(" ", "\n", title),
    xref = "x",
    yref = "y",
    showarrow = FALSE
  )

  m = list(
    l = 2,
    r = 2,
    b = 2,
    t = 2
  )

  fig = plotly::plot_ly(
    df,
    labels = ~ variable,
    values = ~ count,
    name = title,
    type = 'pie',
    textposition = 'inside',
    textinfo = 'label+percent',
    insidetextfont = list(color = "#272c30"),
    hoverinfo = 'text',
    hole = 0.6,
    text = ~ paste(variable, count),
    marker = list(
      colors = colorspace::lighten( colors[1:nrow(df)], 0.5),
      line = list(color =  colors[1:nrow(df)], width = 1)
    ),
    showlegend = FALSE
  ) %>%
    plotly::layout(
      annotations = annot,
      margin = m,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      )
    )
  fig
}


plotly_barplot = function(df, variable, color, title = "") {
  df = df %>%
    group_by(get(variable)) %>%
    summarise(count = n()) %>%
    rename("variable" = "get(variable)") %>%
    ungroup() %>%
    arrange(desc(count))
  df$variable = factor(df$variable, levels = unique(df$variable)[order(df$count, decreasing = F)])


  fig = plotly::plot_ly(
    df,
    x = ~ count,
    y = ~ variable,
    type = 'bar',
    name = variable,
    orientation = "h",
    marker = list(
      color = colorspace::lighten(color, 0.5),
      line = list(color = color,
                  width = 1.5)
    )
  )
  fig = fig %>% plotly::layout(
    title = title,
    yaxis = list(title = ''),
    xaxis = list(title = 'Count')
  )

  return(fig)
}
