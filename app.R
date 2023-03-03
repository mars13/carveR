library(shiny)
library(shinyjs)
library(bs4Dash)
library(ggplot2)
library(shinyWidgets)
library(Hmisc)
library(plotly)
library(ggpubr)
library(shinycssloaders)
library(dplyr)
library(DT)
library(fresh)
library(shinyalert)

options(shiny.maxRequestSize = 2000*1024^2)

#---UI------------------
ui = bs4DashPage(
    title = "carver",
    header = dashboardHeader( title ="carver"),

     sidebar =  bs4DashSidebar(

        #---SIDEBAR MENU-----------------
        sidebarMenu(
        id = "sidebarmenu",
        menuItem(
          "Home",
          tabName = "home",
          icon = icon("home")
        ),
        menuItem(
          "Uploads",
          tabName = "uploads",
          icon = icon("upload")
        ),
        menuItem(
          "Filters",
          tabName = "filters",
          icon = icon("filter")
        ),
        menuItem(
          "Dashboard",
          tabName = "page3",
          icon = icon("chart-simple")
        ),
        menuItem(
          "User guide",
          tabName = "page4",
          icon = icon("info")
        )
      )
      ),

      #---DASHBOARD BODY-----------------
      body =bs4DashBody(
        use_theme(mytheme),
        useShinyjs(),
        tabItems(
          tabItem(
            tabName = "home",
            homePage_ui("home")
          ),

          #---UI UPLOADS-----------------
          tabItem(
              tabName = "uploads",
              uploadInput_ui("upload_input"),
          ),

          #---UI FILTERS-----------------
          tabItem(
              tabName = "filters",
              geneFilters_ui("gene_filters")
            ),

          #---PAGE3 DASHBOARD-----------------
          tabItem(
            tabName = "page3",
            tags$style(HTML(".radio-inline {margin-right: 20px;}")),
            tabsetPanel(
              id = "tabdash",
              type = "pills",
              tabPanel(
                title = "Frequency distribution",
                  frequencyDistribution_ui("frequency_distribution")
                ),
              tabPanel(
                title = "Gene view",
                selectizeInput('geneChoice',
                               label = "",
                               choices = "",
                               options = list(maxOptions = 5, maxItems = 10)),
                tableOutput('gene_options')
              )
            ),
          ),

          #---PAGE4 INFO-----------------
          tabItem(
            tabName = "page4",
            h3("Input files"),
            br(),
            h4("Required fields"),
            mod_test_ui(id = "test1"),
            textOutput("test2")


          )
        )
      )
    )


#---SERVER------------------
server <- function(input, output, session) {

  #---UPLOADS PAGE SERVER-----------------
  # Track the number of input boxes to render
  datasets = reactiveValues(n = 1, datarows = 0, df = NULL)
  #Store files
  files = reactiveValues(files = list(), n = 0)
  uploadInput_server(id = "upload_input", datasets = datasets, files = files)


  #---FILTER PAGE SERVER-------------
  gene_whitelist = reactiveValues(genes = NULL, disabled = F, clear = F)
  gene_blacklist = reactiveValues(genes = NULL, disabled = F, clear = F)
  geneFilters_server("gene_filters", gene_whitelist, gene_blacklist)


  #---DASHBOARD PAGE FREQ TAB SERVER -----------------

  #Set a max number of genes to select for the viz or 0 (same to NA)
  top_value = reactiveValues(n = NA)
  #Set columns to sort by and the respective order
  columns = reactiveValues(col_names = NULL, order = NULL)

  frequencyDistribution_server("frequency_distribution",
                               datasets,
                               gene_whitelist,
                               gene_blacklist,
                               top_value,
                               columns)




  #---DASHBOARD PAGE GENE TAB SERVER -----------------
  gene = reactiveValues(name = NULL, data = NULL, all = NULL)

  observeEvent(c(input$files1, input$files2, input$files3),{
      gene$all = c("GENE1", "GENE2")
  })


  observe({
    updateSelectizeInput(session = session,
                      inputId = 'geneChoice',
                      choices = unlist(gene$all), server = T)
  })

  output$data = DT::renderDT(server = FALSE, {
    datatable(data_filt(),
              filter = "top",
              extensions = "Buttons",
              options = list(paging = TRUE,
                              scrollX=TRUE,
                              searching = TRUE,
                              ordering = TRUE,
                              dom = 'Bfrtip',
                              buttons = c('copy', 'csv', 'excel'),
                              pageLength=5,
                              lengthMenu=c(3,5,10)))
    })

  mod_test_server(id = "test1")

}


shinyApp(ui, server)
