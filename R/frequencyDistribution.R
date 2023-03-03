frequencyDistribution_ui <- function(id){
  ns = NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        br(),
        conditionalPanel("output.sidebar_status === 'OK'", ns = ns,
                         box(
                           title = "Sub-setting",
                           solidHeader = T,
                           width = 12,
                           status = "primary",
                           uiOutput(ns("extraCheckbox"))
                         ),
                         box(
                           title = "Prioritisation",
                           solidHeader = T,
                           width = 12,
                           status = "primary",
                           uiOutput(ns("whitelistFilter")),
                           uiOutput(ns("columnsFilter")),
                           uiOutput(ns("moreControls"))
                        )
        ),
        conditionalPanel("output.sidebar_status === 'WARNING'", ns = ns,
                         box(
                           title = NULL,
                           solidHeader = T,
                           collapsible = F,
                           icon = icon("circle-exclamation", class = "solid"),
                           width = 12,
                           status = "primary",
                           background = "primary",gradient = T,

                           p("The dashboard will only become active once at least one file has been uploaded.")
                         )
        ),
        width = 3),
      mainPanel(
        br(),
        box(
          title = "Gene ranking",
          status = "primary",
          width = 12,
          shinycssloaders::withSpinner(
            plotlyOutput(ns("distPlot")),
            type = 8)
        ),
        br(),
        box(
          title = "Frequency table",
          status = "primary",
          width = 12,
          shinycssloaders::withSpinner(DT::DTOutput(ns("rank")),
                                       type = 8)
        )

      )
    )
  )
}


frequencyDistribution_server = function(id,
                                        datasets,
                                        gene_whitelist,
                                        gene_blacklist,
                                        top_value,
                                        columns) {

  moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      #Set sidebar display status to T if there is data loaded
      output$sidebar_status = renderText(
        if(as.numeric(datasets$datarows) != 0) {
          "OK"
        } else {
          "WARNING"
        }
      )
      outputOptions(output, "sidebar_status", suspendWhenHidden = FALSE)


      output$extraCheckbox = renderUI({
        if(as.numeric(datasets$datarows) != 0) {
          dat = as.data.frame(datasets$df)
          tagList(checkboxGroupInput(inputId = ns("source"),
                                     label = h5("Filter datasets"),
                                     choices = unique(dat$dataset),
                                     selected = unique(dat$dataset)),
                  checkboxGroupInput(inputId = ns("variant"),
                                     label = h5("Filter variant type"),
                                     choices = unique(dat$variant_type),
                                     selected =unique(dat$variant_type)),
                  materialSwitch(inputId = ns("set_max"),
                                 label = h5("Set max gene number"),
                                 status = "primary",
                                 value = length(unique(dat$gene)) > 500),
                  conditionalPanel("input.set_max", ns=ns,
                                   numericInput(inputId = ns("ntop"),
                                                label = "Max genes",
                                                min = 0,
                                                max = length(unique(dat$gene)),
                                                value = 200,
                                                step = 10),
                  ),
          )
        } else {
          bs4Callout(title = "Dashboard will display after uploading the first dataset.", status = "danger", elevation = 1)
        }
      })

      observeEvent(c(input$set_max, input$ntop), {
        if (input$set_max){
          top_value$n = input$ntop
        } else {
          top_value$n = NA
        }
      })

      output$whitelistFilter = renderUI({
        if(length(unlist(gene_whitelist$genes)) > 0 ) {
          tagList(radioButtons(
            inputId = ns("wlMethod"),
            label = h5("Whitelist method"),
            choices = c("Show only", "Show first"),
            selected = c("Show only")))
        }
      })

      output$columnsFilter = renderUI({
        tagList(
          selectInput(inputId = ns('columns'),
                    label = h5("Prioritization fields"),
                    choices = colnames(data_filt()),
                    selected = NULL,
                    multiple = TRUE),
        actionButton(ns("resetcols"), "Reset fields")
        )

      })

      output$moreControls <- renderUI({
        req(input$columns)
        lapply(1:length(input$columns), function(i) {
          add_radiobtn(id = id,
                       ns_id = ns(input$columns[[i]]),
                       item =input$columns[[i]] )
        })
      })


      output$topn = reactive({top_value$n})


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
        updateSelectInput(session,inputId =  "columns", selected = character(0))
        columns$col_names = NULL
        columns$order = NULL
      })

      observe({
        if (!is.null(columns$col_names)) {
          columns$order = unlist(lapply(columns$col_names, function(x) input[[paste0(x, "_radio")]]))
        }
      })

      output$sortColumns = renderText(
        if (!is.null(columns$col_names)) {
          paste(names(input), collapse = ";")
          #paste(paste0(columns$col_names,"_",columns$order), collapse = "--")
        }
      )

      #Filter by dataset and variant type
      data_filt = reactive({
        data = as.data.frame(datasets$df)
        new_data = data[data$dataset %in% unlist(input$source) &
                          data$variant_type %in% unlist(input$variant),]
        if (length(unlist(gene_blacklist$genes)) > 0) {
          new_data = new_data[!new_data$gene %in% unlist(gene_blacklist$genes),]
        }
        if (length(unlist(gene_whitelist$genes)) > 0){
          if(input$wlMethod == "Show only") {
            new_data = new_data[new_data$gene %in% unlist(gene_whitelist$genes),]
          }
        }
        return(new_data)

      })




      output$distPlot <- renderPlotly({
        shiny::isTruthy(nrow(datasets$df) != 0)
        shiny::validate(need(input$source, message = "Warning: Select at least one dataset."),
                        need(input$variant, message = "Warning: Select at least one variant type."))
        plot_ly(data = modify_plot_data(data_filt(),
                                        whitelist = gene_whitelist$genes,
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
        if (as.numeric(datasets$datarows) != 0) {
          rank =  modify_plot_data(data_filt(),
                                   whitelist = gene_whitelist$genes,
                                   ntop = NA,
                                   sort_cols = unlist(columns$col_names),
                                   order_vec = unlist(columns$order))
        } else {
          rank = data.frame()
        }

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


    })
}
