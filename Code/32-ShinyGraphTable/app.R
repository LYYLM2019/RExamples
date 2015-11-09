library(shiny)
library(igraph)

gtest = data.frame(cbind(Article = c(1:10), from = c(11:20), to = c(21:30)))
runApp(list(
  ui = shinyUI(
    fluidPage(    
      titlePanel("Articles by similarities"),
      sidebarLayout(      
        sidebarPanel(
          selectInput("article", "Article:", choice = gtest$Article)
        ),
        mainPanel(
          plotOutput("g3plot")  
        )
      )
    )),
    
    # Define a server for the Shiny app
    server = function(input, output) {
      g3 = reactive({
        g2 = gtest[gtest$Article==input$article,]
        g2 = g2[order(g2[[3]],decreasing = TRUE), ]
        graph.data.frame(g2[1:5,2:3], directed=TRUE)
      })
      
      
      # Fill in the spot we created for a plot
      
      output$g3plot = renderPlot({
        print(class(g3()))
        plot.igraph(g3(), layout=layout.mds)
      })
    })
)


