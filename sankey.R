

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  domain = list(
    x = c(0,1),
    y = c(0,1)
  ),
  orientation = "h",
  valueformat = ".0f",
  valuesuffix = "",
  node = list(
    label = c("(S0)", "Hospital","Censored","Institution","Out","Death"),
    color = c("blue", "blue","red","red","orange","orange"),
    pad =15,
    thickness =20,
    line = list(
      color = "black",
      width = 1,
      height = 0.5
    )
  ),
  link = list(
    source = c(0,0,0,1,1,1,1,3,3,4,4,4),
    target = c(1,2,5,2,3,4,5,2,5,2,1,5),
    value = c(18355, 865, 20,29,6522,35194,5501, 270, 2719, 1488, 25455, 8238)
  )
)



fig <- fig %>% layout(
  title = "Transitions and events between the different states ",
  font = list(
    size = 12
  ),
  xaxis = list(showgrid = F, zeroline = F),
  yaxis = list(showgrid = F, zeroline = F)
)
