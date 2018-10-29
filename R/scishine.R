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
        menuItem("Reading list", tabName = "reading_list", icon = icon("book")),
        shiny::fileInput(
          inputId = 'file_directory',
          label = NULL,multiple = T,buttonLabel = 'Browse',
          placeholder = "Text file"),
        shiny::selectInput("couple_by", "Coupling by:",
                           c("References" = "bic",
                             "Keywords" = "kec",
                             "Title words" = "tic",
                             "Abstract words" = "abc",
                             "Journals" = "joc",
                             "Authors" = "auc",
                             "All words" = "woc",
                             "References & Keywords" = "bickec",
                             "References & Keywords & Titles & Journals" = "bickecticjoc")),
        shiny::radioButtons(
          inputId = 'coms',
          label = 'Select Community',
          choices = list('com1' = 1))
      )),
    dashboardBody(
      tabItems(
        tabItem("summary",
                # Boxes need to be put in a row (or column)
                fluidRow(
                  valueBoxOutput("nb_papers"),
                  valueBoxOutput("nb_ref"),
                  valueBoxOutput("avg_nb_ref"),
                  valueBoxOutput("mean_nb_citation"),
                  valueBoxOutput("med_nb_citation"),
                  valueBoxOutput("nb_subcom"),
                  valueBoxOutput("modularity")
                ),
                fluidRow(
                  box(networkD3::sankeyNetworkOutput("plot_sum_com_tag"),
                      width = 12)
                ),
                fluidRow(
                  box(visNetwork::visNetworkOutput("subcomplot"), 
                      width = 4),
                  box(DT::dataTableOutput("comlegend"), width = 8)
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
        ),
        tabItem("reading_list",
                fluidRow(
                  tabBox(
                    title = "Reading Lists",
                    id = "tabset3", height = "700px", width = "400px",
                    tabPanel("Core papers", 
                             box(
                               DT::dataTableOutput("rl_core_papers"), 
                               width = 12)),
                    tabPanel("Core year", 
                             box(
                               sliderInput(inputId = "nb_paper_yr",
                                           label = "Number of papers per year:",
                                           min = 1,
                                           max = 5,
                                           value = 1)
                             ),
                             box(
                               DT::dataTableOutput("rl_core_yr"), 
                               width = 12)),
                    tabPanel("Old classics", 
                             box(
                               DT::dataTableOutput("rl_classics"), 
                               width = 12)),
                    tabPanel("Reviews", 
                             box(
                               DT::dataTableOutput("rl_reviews"), 
                               width = 12))
                  )
                )
        )
      ), skin = "blue"
    ),
    skin = "blue"
  )
  
  # Define server logic required to draw a histogram ----
  server <- function(input, output, session) {
    
    super_join <- function(rl_cp = rl_cp, lsciPP = lsciPP, spltcr = spltcr){
      return(dplyr::left_join(
        dplyr::left_join(rl_cp, spltcr, by = c('publication' = 'RECID')),
        lsciPP$com1$dfsci, by = c('publication' = 'RECID')
      ))
    }
    
    lsci <- reactive({
      if(!is.null(input$file_directory)){
        sci <- scimeetr:::import_wos_files_shine(input$file_directory$datapath)
      } else {
        sci <- NULL
      }
    })
    
    spltcr <- reactive({split_cr(lsci())})
    
    lsci1 <-  reactive({
      if(is.null(lsci())){
        lsci1 <- NULL
      } else {
        scimap(lsci(), coupling_by = input$couple_by)
      }
    })
    
    r <-  reactive({
      if(is.null(lsci1())){
        r <- NULL
      } else {
        summary(lsci1())
      }
    })
    
    kw <-  reactive({scimeetr::characterize_kw(lsci1())})
    so <-  reactive({scimeetr::characterize_so(lsci1())})
    jo <-  reactive({scimeetr::characterize_jo(lsci1())})
    au <-  reactive({scimeetr::characterize_au(lsci1())})
    co <-  reactive({scimeetr::characterize_co(lsci1())})
    un <-  reactive({scimeetr::characterize_un(lsci1())})
    ti <-  reactive({scimeetr::characterize_ti(lsci1())})
    ab <-  reactive({scimeetr::characterize_ab(lsci1())})
    cp <- reactive({
      rl_cp <- scimeetr::scilist(lsci1(),
                                 reading_list = 'core_papers',
                                 k = 100)
      purrr::map(rl_cp, super_join, lsciPP = lsci1(), spltcr = spltcr())
    })
    cr <- reactive({purrr::map(
      scimeetr::scilist(lsci1(), 
                        reading_list = 'core_residual',
                        k = 20),
      super_join, lsciPP = lsci1(), spltcr = spltcr())
    })
    cy <- reactive({
      purrr::map(
        scimeetr::scilist(lsci1(), 
                          reading_list = 'core_yr',
                          k = input$nb_paper_yr),
        super_join, lsciPP = lsci1(), spltcr = spltcr())
    })
    cmo <- reactive({
      purrr::map(
        scimeetr::scilist(lsci1(), 
                          reading_list = 'cite_most_others',
                          k = 20),
        super_join, lsciPP = lsci1(), spltcr = spltcr())
    })
    
    observe({
      choicelist <- as.list(1:length(names(lsci1())))
      names(choicelist) <- paste0( '* ',
                                   purrr::map_chr(
                                     purrr::map(purrr::map(
                                       lsci1(),
                                       'tag'), function(x){x[1:3]}),
                                     paste,
                                     collapse = ' * ',
                                     sep = ' * '),
                                   ' (',
                                   toupper(names(lsci1())), ')')
      # Can also set the label and select items
      updateRadioButtons(session, "coms",
                         choices = choicelist,
                         selected = 1
      )
    })
    
    output$tst <- renderValueBox({
      valueBox(
        input$coms, "papers", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$nb_papers <- renderValueBox({
      valueBox(
        r()$nb_papers, "papers", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$nb_ref <- renderValueBox({
      valueBox(
        r()$nb_ref, "unique references", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$avg_nb_ref <- renderValueBox({
      valueBox(
        r()$avg_nb_ref, "reference per paper on average", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$mean_nb_citation <- renderValueBox({
      valueBox(
        round(r()$mean_nb_citation[1]), "citation per paper on average", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$med_nb_citation <- renderValueBox({
      valueBox(
        median(r()$quant_nb_citation[3]), "citation per paper on median", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$nb_subcom <- renderValueBox({
      valueBox(
        length(lsci1())-1, "communities", icon = icon("article"),
        color = "blue", width = 1
      )
    })
    
    output$modularity <- renderValueBox({
      valueBox(
        round(igraph::modularity(lsci1()$com1$coms), digits = 2), "modularity", icon = icon("article"),
        color = "blue", width = 1
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
        pageLength = 100
      ))
    
    output$plot_sum_com_tag <- networkD3::renderSankeyNetwork(
      if(is.null(lsci())){
        plot(1:3,1:3)
      } else {
        compare_scimap(list(lsci(), lsci1()))
      })
    
    output$kw <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Keywords" = NA,
                   "Frequency" = NA)
      } else {
        kw()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$au <- DT::renderDataTable(
      
      if(is.null(lsci())){
        data.frame("Keywords" = NA,
                   "Frequency" = NA)
      } else {
        au()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$jo <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Journals" = NA,
                   "Frequency" = NA)
      } else {
        jo()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$ti <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Title words" = NA,
                   "Frequency" = NA)
      } else {
        ti()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$ab <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Abstractwords" = NA,
                   "Frequency" = NA)
      } else {
        ab()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$co <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Countries" = NA,
                   "Frequency" = NA)
      } else {
        co()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$un <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Universities" = NA,
                   "Frequency" = NA)
      } else {
        un()[[as.numeric(input$coms)]]
      },
      options = list(
        pageLength = 25
      ))
    
    output$rl_core_papers <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Paper" = NA,
                   "Citation number" = NA)
      } else {
        dplyr::select(cp()[[as.numeric(input$coms)]], 'publication','metric', 'doi','TI')
      },
      options = list(
        pageLength = 20
      ),escape = FALSE)
    
    output$rl_core_yr <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Paper" = NA,
                   "Citation number" = NA)
      } else {
        dplyr::select(cy()[[as.numeric(input$coms)]], 'publication','metric', 'doi','TI')
      },
      options = list(
        pageLength = 20
      ),escape = FALSE)
    
    output$rl_classics <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Paper" = NA,
                   "Citation number" = NA)
      } else {
        dplyr::select(cr()[[as.numeric(input$coms)]], 'publication','metric', 'doi','TI')
      },
      options = list(
        pageLength = 20
      ),escape = FALSE)
    
    output$rl_reviews <- DT::renderDataTable(
      if(is.null(lsci())){
        data.frame("Paper" = NA,
                   "Citation number" = NA)
      } else {
        dplyr::select(cmo()[[as.numeric(input$coms)]], 'publication','metric', 'doi','TI')
      },
      options = list(
        pageLength = 20
      ),escape = FALSE)
  }
  
  return(shinyApp(ui, server))
}
