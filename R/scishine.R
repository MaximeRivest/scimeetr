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
        menuItem("In depth characterization", tabName = "characterize", icon = icon("star")),
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
                  box(visNetwork::visNetworkOutput("subcomplot"), 
                                      width = 4),
                  box(DT::dataTableOutput("comlegend"), width = 8)
                ),
                fluidRow(
                  valueBoxOutput("nb_papers"),
                  valueBoxOutput("nb_ref"),
                  valueBoxOutput("avg_nb_ref"),
                  valueBoxOutput("mean_nb_citation"),
                  valueBoxOutput("med_nb_citation"),
                  valueBoxOutput("nb_subcom")
                  ),
                fluidRow(
                  box(networkD3::sankeyNetworkOutput("plot_sum_com_tag"),
                      width = 12)
                )
        ),
        tabItem("characterize",
                fluidRow(
                  tabBox(
                    title = "Frequency tables",
                    id = "tabset2", height = "700px", width = "400px",
                    tabPanel("Keywords", 
                             box(
                               DT::dataTableOutput("kw"), 
                               width = 12)),
                    tabPanel("Abstract words", 
                             box(
                               DT::dataTableOutput("ab"), 
                               width = 12)),
                    tabPanel("Title words", 
                             box(
                               DT::dataTableOutput("ti"), 
                               width = 12)),
                    tabPanel("Journals", 
                             box(
                               DT::dataTableOutput("jo"), 
                               width = 12)),
                    tabPanel("Authors",
                             box(
                               DT::dataTableOutput("au"), 
                               width = 12)),
                    tabPanel("Universities",
                             box(
                               DT::dataTableOutput("un"), 
                               width = 12)),
                    tabPanel("Countries",
                             box(
                               DT::dataTableOutput("co"), 
                               width = 12))
                  )
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
        round(r()$mean_nb_citation[1]), "citation per paper on average", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$med_nb_citation <- renderValueBox({
      valueBox(
        median(r()$quant_nb_citation[3]), "citation per paper on median", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$nb_subcom <- renderValueBox({
      valueBox(
        length(lsci1())-1, "communities", icon = icon("article"),
        color = "light-blue", width = 1
      )
    })
    
    output$subcomplot <- visNetwork::renderVisNetwork(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        plot_subcommunities(lsci1())
      })
    
    output$comlegend <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame(X = 0, Y = 0)
      } else {
        r()$ltag
      },
      options = list(
        pageLength = 7
      ))
    
    output$plot_sum_com_tag <- networkD3::renderSankeyNetwork(
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
    
    output$au <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Keywords" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_au(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
    
    output$jo <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Journals" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_jo(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
    
    output$ti <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Title words" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_ti(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
    
    output$ab <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Abstractwords" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_ab(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
    
    output$co <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Countries" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_co(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
    
    output$un <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Universities" = NA,
                   "Frequency" = NA)
      } else {
        scimeetr::characterize_un(lsci())$com1
      },
      options = list(
        pageLength = 6
      ))
  }
  
  return(shinyApp(ui, server))
}
