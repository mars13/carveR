library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyBS)
library(shinyWidgets)
library(Hmisc)
library(plotly)
library(ggplot2)
library(shinycssloaders)
library(dplyr)
library(DT)
library(fresh)
library(shinyalert)
library(colorspace)
library(fresh)


options(shiny.maxRequestSize = 2000*1024^2)

#---UI------------------
ui = bs4DashPage(
    title = "carver",
    header = dashboardHeader(title ="carver"),

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
                geneView_ui("gene_view"),
              )
            ),
          ),

          #---PAGE4 INFO-----------------
          tabItem(
            tabName = "page4",
           userGuide_ui("user_guide")
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
  gene_whitelist = reactiveValues(genes = NULL, row_selected = NULL, clear = F)
  gene_blacklist = reactiveValues(genes = NULL, row_selected = NULL, clear = F)
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
  geneView_server("gene_view", datasets)

}


shinyApp(ui, server)
