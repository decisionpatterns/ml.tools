#' Decision Point
#'
#' 
#' This Shiny app/gadget to select a decision point (cutoff) used
#' by binomial classification model.
#'
#' You can run the application by clicking the 'Run App' button above.
#' At present it requires a model.
#' 
#' @param actual character; actual/observed class
#' @param preds numeric; predicted value
#' 
#' @examples
#' n = 1000
#' decisionpt( runif(n,0,1:100)>50 %>% as.character, runif(n,0,1:100)/100 )
#'
#' @export

  library(shiny)
  library(ROCR)
  library(ggplot2)


decisionpt <- function(actual,preds ) {

  # preds <- predict(fit,dat, type="prob")

  # Define UI for application that draws a histogram
  ui <- fluidPage(

     # Application title
     titlePanel("Decision Point"),

     # Sidebar with a slider input for number of bins
     sidebarLayout(
        sidebarPanel(
           sliderInput("cutoff",
                       "Cutoff:",
                       min = 0,
                       max = 1,
                       value = 1.0,
                       animate=TRUE),
           tableOutput("table")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
     )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

     output$distPlot <- renderPlot({
       qplot(preds, fill=actual) +
         geom_vline( xintercept = input$cutoff, color="red", linetype="dashed" ) +
         scale_x_continuous("cutoff", limits=c(0,1)) +
         scale_y_sqrt("Number of Contracts")

     })


    output$table <- renderTable( rownames = TRUE,
      {

       tab <- table( preds >= input$cutoff, actual ) %>% as.data.frame.matrix()

       # browser()
       if(nrow(tab)==1) {
           tab[2,] <- c(0L,0L)
           rownames(tab)[2] <- "TRUE"
       }

       tab$Predicted <- rowSums(tab) %>% as.integer
       tab <- rbind(tab,colSums(tab) %>% as.integer)
         # tab[,-1] <- tab[,-1] %>% as.integer
       rownames(tab)[nrow(tab)] <- "Observed:"
       tab
      }
    )

  }


  runGadget(ui,server)
}
