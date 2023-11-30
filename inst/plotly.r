# based on https://plotly.com/r/3d-scatter-plots/

library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))

fig



fig2 <- 
  temps_xtra_nice %>%
  filter( is.na(Rate) == FALSE) %>%
  plot_ly( x = ~id, y = ~tavg, z = ~Rate, color = ~day_in_wint)

fig2
