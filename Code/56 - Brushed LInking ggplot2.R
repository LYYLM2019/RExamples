# purpose: http://stackoverflow.com/questions/37226473/shiny-r-plotting-outputs-onto-a-scatterplot-alongside-existing-data-from-a-csv
# author: tirthankar chakravarty
# created: 14th may 2016
# revised: 
# comments: 

library(dplyr)
library(shiny)
library(ggplot2)

# simulate some data
df_foo = data_frame(
  percentile = seq.int(99),
  BTI = sort(rnorm(99))
)

# ui
ui_foo = shinyUI(
  plotOutput(
    "plot_foo"
  )
)

# change the data when the button changes
df_foo = eventReactive(input$button_click, {
  ecdf_income = ecdf(df_foo()$BTI)
  return(rbind(df_foo(), 
               c(percentile = ecdf_income(input$income), 
                 BTI = input$income)
  ))
})

# render the plot --> this will change whenever the data changes
output$plot_foo = renderPlot({
  df_foo() %>% 
    ggplot(aes(x = percentile, y = BTI)) + 
    geom_point() + 
    geom_line() + 
    theme_bw()
})