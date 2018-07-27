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
#' @import shiny shinydashboard
scishine <- function(){
  # Define UI for app that draws a histogram ----
  ui <- dashboardPage(
    dashboardHeader(title = "Scimeetr"),
    dashboardSidebar(      
      sidebarMenu(
        menuItem("Summary", tabName = "summary", icon = icon("home")),
        shiny::fileInput(
          inputId = 'file_directory',
          label = NULL,multiple = T,buttonLabel = 'Browse',
          placeholder = "Text file")
      )),
    dashboardBody(
      tabItems(
        tabItem("summary",
                # Boxes need to be put in a row (or column)
                fluidRow(
                  valueBoxOutput("nb_papers"),
                  valueBoxOutput("nb_ref"),
                  valueBoxOutput("avg_nb_ref"),
                  valueBoxOutput("mean_nb_citation")
                  ),
                fluidRow(
                  tabBox(
                    title = "Plots",
                    id = "tabset1", height = "700px", width = "400px",
                    tabPanel("Source", plotOutput("plot_sum_com_so")),
                    tabPanel("Keyword", sankeyNetworkOutput("plot_sum_com_tag")),
                    tabPanel("Title", plotOutput("plot_sum_com_ti")),
                    tabPanel("Abstract", plotOutput("plot_sum_com_ab")),
                    tabPanel("Author", plotOutput("plot_sum_com_au")),
                    tabPanel("Size", plotOutput("plot_sum_com_size")),
                    tabPanel("ID", plotOutput("plot_sum_com_id"))
                  )
                ),
                fluidRow(
                  shinydashboard::box(DT::dataTableOutput("kw"), 
                                      width = 12)
                )
        )
      )
    )
  )
  
  # Define server logic required to draw a histogram ----
  server <- function(input, output) {
    lsci <- reactive({
      if(!is.null(input$file_directory)){
        sci <- import_wos_files_shine(input$file_directory$datapath)
      } else {
        sci <- NULL
      }
    })
    lsci1 <-  reactive({
      if(is.null(lsci())){
        lsci1 <- NULL
      } else {
        scimap(lsci())
      }
    })
    r <-  reactive({
      if(is.null(lsci1())){
        r <- NULL
      } else {
        summary(lsci1())
      }
    })
    
    output$nb_papers <- renderValueBox({
      valueBox(
        r()$nb_papers, "papers", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$nb_papers <- renderValueBox({
      valueBox(
        r()$nb_papers, "papers", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$nb_ref <- renderValueBox({
      valueBox(
        r()$nb_ref, "unique references", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$avg_nb_ref <- renderValueBox({
      valueBox(
        r()$avg_nb_ref, "reference per paper on average", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$mean_nb_citation <- renderValueBox({
      valueBox(
        round(r()$mean_nb_citation), "citation per paper on average", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    output$plot_sum_com_tag <- renderSankeyNetwork(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        compare_scimap(list(lsci(), lsci1()))
      })
    
    output$plot_sum_com_so <- renderPlot(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot(r(), "so")
      })
    
    output$plot_sum_com_ab <- renderPlot(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot(r(), "ab", node_size = 1)
      })
    
    output$plot_sum_com_ti <- renderPlot(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot(r(), "ti")
      })
    
    output$plot_sum_com_size <- renderPlot(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot(r(), "size")
      })
    
    output$plot_sum_com_au <- renderPlot(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot(r(), "au")
      })
    
    output$plot_sum_com_id <- renderPlot(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot(r(), "id")
      })
    
    output$kw <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Keywords" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_kw(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
  }
  
  return(shinyApp(ui, server))
}
