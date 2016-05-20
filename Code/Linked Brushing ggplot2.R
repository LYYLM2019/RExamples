# purpose: http://stackoverflow.com/questions/37226473/shiny-r-plotting-outputs-onto-a-scatterplot-alongside-existing-data-from-a-csv
# author: tirthankar chakravarty
# created: 14th may 2016
# revised: 
# comments: 

library(dplyr)
library(shiny)
library(ggplot2)

# ui
ui_foo = fluidPage(
  plotOutput(
    "plot_foo"
  ),
  numericInput(inputId = "income", label = "Income: ", value = NULL),
  actionButton("button_click", "Go!")
)

# server
server_foo = shinyServer(function(input, output) {
  react_vals = reactiveValues(
    # simulate some data --> initialize the reactive dataset
    df_foo = data_frame(
      percentile = seq.int(99),
      BTI = sort(rnorm(99))
    )
  )
  
  # change the data when the button changes
  observeEvent(input$button_click, {
    ecdf_income = ecdf(react_vals$df_foo$BTI)
    react_vals$df_foo = rbind(react_vals$df_foo, 
                 c(percentile = ecdf_income(input$income)*100, 
                   BTI = input$income))
  })
  
  # make the plot respond to changes in the dataset
  output$plot_foo = renderPlot({
    react_vals$df_foo %>% 
      ggplot(aes(x = percentile, y = BTI)) + 
      geom_point() + 
      geom_line() + 
      theme_bw()
  })
})

# run the app
options(shiny.reactlog = TRUE)
shinyApp(ui = ui_foo, server = server_foo)