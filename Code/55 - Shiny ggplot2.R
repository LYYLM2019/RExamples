#==============================================================================
# purpose: answer http://stackoverflow.com/questions/37226473/shiny-r-plotting-outputs-onto-a-scatterplot-alongside-existing-data-from-a-csv
# author: tirthankar chakrvarty
# created: 14th may 2016
# revised: 
# comments: 
#==============================================================================

library(dplyr)
library(shiny)
library(tidyr)
library(ggplot2)

# simulate some data 
df_foo = data_frame(
  percentile = seq.int(99),
  ATI = sort(rnorm(99)),
  BTI = sort(rnorm(99))
)

# UI
ui_foo = shinyUI(
  plotOutput("plot_foo")
)

# server
server_foo = shinyServer(function(input, output) {
  output$plot_foo = renderPlot({
    df_foo %>% 
    gather(key = var_name, value = value, -percentile) %>% 
    ggplot(aes(x = value, y = percentile, group = var_name, color = var_name)) + 
    geom_line() + 
    theme_bw()
  })
})

# run the server
shinyApp(ui = ui_foo, server = server_foo)