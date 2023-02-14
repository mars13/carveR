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
     header = dashboardHeader(title = "carveR"),
     title = "carveR",

     sidebar =  bs4DashSidebar(

        #---SIDEBAR MENU-----------------
        sidebarMenu(
        id = "sidebarmenu",
        #sidebarHeader("Header 1"),
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

          #---PAGE1 UPLOADS-----------------
          tabItem(
              tabName = "uploads",
              mod_uploads_ui("file_uploads")
          ),

          #---PAGE2 FILTERS-----------------
          tabItem(
            tabName = "filters",

            fluidRow(
              column(11, align="center",
                      box(
                        title = "  Format requirements",
                        closable = FALSE,
                        width = 8,
                        solidHeader = F,
                        collapsible = T,
                        icon = icon("circle-info", lib = "font-awesome"),
                        h6("Format should be one gene per row."),
                        h6("No header should be present - first row should already contain a gene name."),
                        h6("Press the remove button to reset both text and file fields.")
                      )
              )
            ),


            fluidRow(
              column(6,
                     div(
                       id = "wlform",
                       uiOutput('whitelist'),
                       br(),
                       uiOutput('genewlInfo')
                      )
                     ),
                column(6,
                       div(
                         id = "blform",
                         uiOutput('blacklist'),
                         br(),
                         uiOutput('geneblInfo'))
                )
            )
            ),


          #---PAGE3 DASHBOARD-----------------
          tabItem(
            tabName = "page3",
            tags$style(HTML(".radio-inline {margin-right: 20px;}")),
            sidebarLayout(
              sidebarPanel(
                uiOutput("extraCheckbox"),
                uiOutput("columnsFilter"),
                uiOutput("moreControls"),
                actionButton("resetcols", "Reset fields"),
                textOutput("sortColumns"),
                width = 3
              ),
              mainPanel(
                tabsetPanel(
                  id = "tabdash",type = "pills",

                  tabPanel(
                    title = "Frequency distribution",
                    br(),
                    box(
                      title = "Gene ranking",
                      status = "primary",
                      width = 12,
                      shinycssloaders::withSpinner(
                        plotlyOutput("distPlot"),
                        type = 8)
                      ),
                        br(),
                    box(
                      title = "Frequency table",
                      status = "primary",
                      width = 12,
                      shinycssloaders::withSpinner(DT::DTOutput("rank"),
                                                   type = 8)
                    )

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

                )
            )
          ),

          #---PAGE4 INFO-----------------
          tabItem(
            tabName = "page4",
            h3("Input files"),
            br(),
            h4("Required fields"),
            mod_test_ui(id = "test1")


          )
        )
      )
    )


#---SERVER------------------
server <- function(input, output, session) {

  #---UPLOADS PAGE SERVER-----------------
  # Track the number of input boxes to render
  datasets = reactiveValues(n = 1, datarows = 0)
  #Store files
  files = reactiveValues(files = list(), n = 0)

  #Observe first dataset events
  observeEvent(c(input$files1,input$dataset1), {
    if(!is.null(input$files1) ){
      files$files[["files1"]] = cbind(dataset = input$dataset1, input$files1)
      files$n = nrow(do.call('rbind', files$files))
      datasets$datarows = nrow(data())

    }
  })

  #Observe second dataset events
  observeEvent(c(input$files2,input$dataset2), {
    if(!is.null(input$files2)){
      files$files[["files2"]] = cbind(dataset = input$dataset2, input$files2)
      files$n = nrow(do.call('rbind', files$files))
      datasets$datarows = nrow(data())

    }
  })

  #Observe third dataset events
  observeEvent(c(input$files3,input$dataset3), {
    if(!is.null(input$files3) ){
      files$files[["files3"]] = cbind(dataset = input$dataset3, input$files3)
      files$n = nrow(do.call('rbind', files$files))
      datasets$datarows = nrow(data())

    }
  })

  #Unlist stored files
  files_df = reactive({ do.call('rbind', files$files) })

  #render table of uploaded files
  output$contents <- renderTable({
    files_df()[,c("dataset", "name")]
  })

  #triggered when add_btn is pressed
  observeEvent(input$add_btn, {
    datasets$n = datasets$n + 1
    insertUI(selector = "#anchor",
             where = "beforeBegin",
             ui = add_box(datasets$n)
    )
  })

  #triggered when rm_btn is pressed
  observeEvent(input$rm_btn, {
    if (datasets$n > 1) {
      if(!is.null(files$files[[paste0("files", datasets$n)]])) {
        files$files = files$files[-which(names(files$files) == paste0("files", datasets$n))]
        files$n = nrow(do.call('rbind', files$files))
      }
      removeUI(selector = paste0("#new_box", datasets$n))
      datasets$n = datasets$n - 1
    }
  })

  #outputs for first page cards
  output$counter =  reactive({datasets$n})
  output$filenum = reactive({files$n})

  data = reactive({
    # Empty list to hold results
    ll <- list()
    for(f in c(1:nrow(files_df())) ){
      # Read from file
      temp = read.table(files_df()[f,"datapath"], header = T, sep = "\t", stringsAsFactors = F)
      # Save modified filename as field value
      temp$dataset =files_df()[f,"dataset"]
      # Save within a list
      ll[[f]] <- temp
    }
    data = do.call(plyr::rbind.fill, ll)
    data = edit_fusions(data)

    data$variant_type = ""
    if("fusion" %in% colnames(data)) {
      data[!is.na(data$fusion),]$variant_type = "SV"
    }
    if("avecopynumber" %in% colnames(data)) {
      data[!is.na(data$avecopynumber),]$variant_type = "CNA"
    }
    if("ref" %in% colnames(data)) {
      data[!is.na(data$ref),]$variant_type = "SNV"
    }
    data = data[data$variant_type != "",]
    data = data[data$gene != "",]
    return(data)
  })





  output$datarows = renderText({datasets$datarows})

  #---FILTER PAGE SERVER-------------

  #Whitelist

  gene_whitelist = reactiveValues(genes = NULL, disabled = F, clear = F)

  #Observe event, store info and set disabled=T if file exists
  observeEvent(input$genewlArea, {
   if(!is.null(input$genewlArea)) {
      genes = strsplit(input$genewlArea, "\n")
      names(genes) = "genes"
      gene_whitelist$genes = genes
    }
  })

  observeEvent(input$genewlFile, {
    if(!is.null(input$genewlFile)){
      wldata = read.delim(input$genewlFile$datapath, header = F)
      if(ncol(wldata)>1)
      {
        shinyalert::shinyalert("Column Error",
                               "Uploaded Data has more than 1 column",
                                type="error")
        shinyjs::reset(id = 'wlform')
      } else {
        colnames(wldata) = "genes"
        gene_whitelist$genes = wldata
        gene_whitelist$disabled = T
        gene_whitelist$clear = F
      }
    }
  })

  observeEvent(input$genewlRemove, {
    shinyjs::reset(id = 'wlform')
    gene_whitelist$genes = NULL
    gene_whitelist$clear = TRUE
    gene_whitelist$disabled = F
  })

  output$whitelist = renderUI({
    add_input_list(id = "genewl",
                   title = "Gene whitelist",
                   description = "Genes in the whitelist can be used in prioritisation or to filter out other genes.")
  })

  output$genewlInfo= renderUI({
    req(
      shiny::isTruthy(length(unlist(gene_whitelist$genes)) != 0)
      )
    if(!is.null(gene_whitelist$genes)){
      x = length(unlist(gene_whitelist$genes))
    } else {
      x = 0
    }
      bs4InfoBox(title = "Genes in whitelist",
                 value = h5(x),
                 color = "primary",
                 icon = icon("check"))
  })


  #Blacklist

  gene_blacklist = reactiveValues(genes = NULL, disabled = F, clear = F)

  #Observe event, store info and set disabled=T if file exists
  observeEvent(input$geneblArea, {
    if(!is.null(input$geneblArea)) {
      genes = strsplit(input$geneblArea, "\n")
      names(genes) = "genes"
      gene_blacklist$genes = genes
    }
  })

  observeEvent(input$geneblFile, {
    if(!is.null(input$geneblFile)){
      gene_blacklist$genes = read.delim(input$geneblFile$datapath, header = F, col.names = "genes")
      gene_blacklist$disabled = T
      gene_blacklist$clear = F
    }
  })

  observeEvent(input$geneblRemove, {
    shinyjs::reset(id = 'blform')
    gene_blacklist$genes = NULL
    gene_blacklist$clear = TRUE
    gene_blacklist$disabled = F
  })

  output$blacklist = renderUI({
    add_input_list(id = "genebl",
                   title = "Gene blacklist",
                   description = "Genes in the blacklist will automatically be filtered out.",
                   status  = "danger")
  })


  output$geneblInfo= renderUI({
    req(
      shiny::isTruthy(length(unlist(gene_blacklist$genes)) != 0)
      )
    if(!is.null(gene_blacklist$genes)){
      x = length(unlist(gene_blacklist$genes))
    } else {
      x = 0
    }
    bs4InfoBox(title = "Genes in blacklist",
               value = h5(x),
               color = "danger",
               icon = icon("xmark"))
  })



  #---DASHBOARD PAGE FREQ TAB SERVER -----------------
  output$extraCheckbox = renderUI({
    req(input$files1)
    tagList(checkboxGroupInput("source", label = h5("Filter datasets"),
                               choices = unique(data()$dataset),
                               selected = unique(data()$dataset)),
            checkboxGroupInput("variant", label = h5("Filter variant type"),
                               choices = unique(data()$variant_type),
                               selected =unique(data()$variant_type)),
            radioButtons(
              inputId = "wlMethod",
              label = h5("Whitelist method"),
              choices = c("Show only", "Show first"),
              selected = c("Show only")),
            materialSwitch(inputId = "set_max", label = h5("Set max gene number"), status = "primary", value = length(unique(data()$gene)) > 500),
            conditionalPanel("input.set_max",
                             numericInput("ntop", label = "Max genes", min = 0, max = length(unique(data()$gene)), value = 200, step = 10),
            ),
    )
  })


  output$columnsFilter = renderUI({
    req(input$files1)
    selectInput(inputId = 'columns',
                   label = h5("Prioritization fields"),
                   choices = colnames(data_filt()),
                   selected = NULL,
                   multiple = TRUE)
  })

  output$moreControls <- renderUI({
    req(input$columns)
    lapply(1:length(input$columns), function(i) {
      add_radiobtn(input$columns[[i]])
    })
  })

  #Set columns to sort by and the respective order
  columns = reactiveValues(col_names = NULL, order = NULL)

  observeEvent(input$columns, {
    if (!is.null(input$columns)) {
      if(all(input$columns != character(0))) {
        columns$col_names = input$columns
      } else {
        columns$col_names = NULL
      }
    } else {
      columns$col_names = NULL
    }
  })

  observeEvent(input$resetcols, {
    updateSelectInput(session, "columns", selected = character(0))
    columns$col_names = NULL
    columns$order = NULL
  })

  observe({
    if (!is.null(columns$col_names)) {
      columns$order = unlist(lapply(columns$col_names, function(x) input[[paste0("radio_", x)]]))
    }
  })

  output$sortColumns = renderText(
    if (!is.null(columns$col_names)) {
      paste(paste0(columns$col_names,"_",columns$order), collapse = "--")
    }
  )

  #Filter by dataset and variant type
  data_filt = reactive({
      new_data = data()[data()$dataset %in% unlist(input$source) &
                    data()$variant_type %in% unlist(input$variant),]
      if (length(unlist(gene_blacklist$genes)) > 0) {
        new_data = new_data[!new_data$gene %in% unlist(gene_blacklist$genes),]
      }
      if (length(unlist(gene_whitelist$genes)) > 0 & input$wlMethod == "Show only"){
        new_data = new_data[new_data$gene %in% unlist(gene_whitelist$genes),]
      }
      return(new_data)

  })

  #Set a max number of genes to select for the viz or 0 (same to NA)
  top_value = reactiveValues(n = NA)

  observeEvent(c(input$set_max, input$ntop), {
    if (input$set_max){
      top_value$n = input$ntop
    } else {
      top_value$n = NA
    }
  })

  output$topn = reactive({top_value$n})


  output$distPlot <- renderPlotly({
    shiny::validate(need(input$files1, message = "Plot error: Input at least one file to proceed."),
                    need(input$source, message = "Warning: Select at least one dataset."),
                    need(input$variant, message = "Warning: Select at least one variant type."))
    plot_ly(data = modify_plot_data(data_filt(),
                                    ntop = top_value$n,
                                    sort_cols = unlist(columns$col_names),
                                    order_vec = unlist(columns$order)),
            x = ~gene,
            y = ~value,
            type = 'bar',
            color = ~dataset,
            colors=get_palette('locuszoom', 5)[c(1,2,4)])  %>%
      config(toImageButtonOptions = list(format = "svg",
                                         filename = "custom_plot",
                                         width = 1000,
                                         height = 600))  %>%
      layout(xaxis = list(rangeslider = list(), title = ''),
             yaxis = list(title = '% samples', autotypenumbers = 'strict'))

  })

  output$rank = DT::renderDT(server = T, {
    req(input$files1)
    rank =  modify_plot_data(data_filt(),
                             ntop = NA,
                             sort_cols = unlist(columns$col_names),
                             order_vec = unlist(columns$order))
    datatable(rank,
              filter = "top",
              extensions = "Buttons",
              options = list(paging = TRUE,
                             scrollX=TRUE,
                             searching = TRUE,
                             ordering = TRUE,
                             dom = 'Bfrtip',
                             buttons = c('copy', 'csv', 'excel'),
                             pageLength=10))
  })



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
