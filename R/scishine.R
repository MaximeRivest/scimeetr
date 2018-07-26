#' Start shiny app to use scimeetr interactively
#' 
#' This function starts an interactive web app
#' 
#' @usage scishine(files_directory)
#' @details No details for now.
#' @return A shiny app
#' @author Maxime Rivest
#' @examples 
#' scishine()
#' @seealso \code{\link{scimeetr}}.
#' @export
#' @import shiny
scishine <- function(){
  # Define UI for app that draws a histogram ----
  ui <- fluidPage(
    
    # App title ----
    titlePanel("Scimeetr"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
      # Sidebar panel for inputs ----
      sidebarPanel(
        
        # Input: Slider for the number of bins ----
        shiny::fileInput(inputId = 'file_directory',label = NULL,multiple = T,buttonLabel = 'Browse',placeholder = "Text file"), width = 3 
        
      ),
      
      # Main panel for displaying outputs ----
      mainPanel(
        h2('Summary'),
        fluidRow(column(width=8,
                        p(strong('Number of papers:'))),
                 column(width = 4,
                        textOutput("nb_papers"))),
        fluidRow(column(width=8,
                        p(strong('Number of different reference:'))),
                 column(width = 4,
                        textOutput("nb_ref"))),
        fluidRow(column(width=8,
                        p(strong('Average number of reference per paper:'))),
                 column(width = 4,
                        textOutput("avg_nb_ref"))),
        fluidRow(column(width=8,
                        p(strong('Quantiles of total citation per paper:')))),
        fluidRow(column(width = 8),
                 column(width = 4,
                        tableOutput("quant_nb_citation"))),
        fluidRow(column(width=8,
                        p(strong('Mean number of citation per paper:'))),
                 column(width = 4,
                        textOutput("mean_nb_citation"))),
        fluidRow(column(width=8,
                        p(strong(' Average number of citation per paper per year:'))),
                 column(width = 4,
                        textOutput("avg_nb_citation_yr"))),
        h3("Keyword table"),
        fluidRow(column(width = 12,DT::dataTableOutput("kw")))
      )
    )
  )
  
  
  # Define server logic required to draw a histogram ----
  server <- function(input, output) {
    r <- NULL
    lsci <- NULL
    lsci <- reactive({
      
      if(length(input$file_directory$datapath)>= 1){
        sci <- import_wos_files_shine(input$file_directory$datapath)
      } else {
        sci <- NULL
      }
    })
    r <-  reactive({summary(lsci())})

    output$nb_papers <- renderText({r()$nb_papers})
    output$nb_ref <- renderText({r()$nb_ref})
    output$avg_nb_ref <- renderText({r()$avg_nb_ref})
    output$quant_nb_citation <- renderTable({
      my_df <- data.frame(quantile = names(r()$quant_nb_citation),
                          citations = r()$quant_nb_citation)
      })
    output$mean_nb_citation <- renderText({r()$mean_nb_citation})
    output$avg_nb_citation_yr <- renderText({r()$avg_nb_citation_yr})
    
    # Show the first "n" observations ----
    output$kw <- DT::renderDataTable(scimeetr::characterize_kw(lsci())$com1,
                                 options = list(
                                   pageLength = 8))
  }
  
  return(shinyApp(ui, server))
}
