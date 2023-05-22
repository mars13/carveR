uploadInput_ui = function(id){
  ns = NS(id)

  tagList(
        sidebarLayout(

          sidebarPanel(
            add_box(id = id,
                    name = 1,
                    dataset_id = ns("dataset1"),
                    file_id = ns("files1")),
            hr(id = "anchor"),
            fluidRow(
              column(1,
                     conditionalPanel("output.counter <= 2 & output.filenum !== 0", ns = ns,
                                      actionButton(ns("add_btn"), icon("plus", class = NULL, lib = "font-awesome")))
              ),
              column(1,
                     conditionalPanel("output.counter > 1", ns = ns,
                                      actionButton(ns("rm_btn"), icon("minus", class = NULL, lib = "font-awesome")))
              )
            )
          ),
          mainPanel(
            fluidRow(
              bs4Dash::valueBox(value = h4(textOutput(ns("counter"))),
                                "DATASETS",
                                gradient = T,
                                icon = icon("book", lib = "font-awesome"),
                                color = "primary"),
              bs4Dash::valueBox(value = h4(textOutput(ns("filenum"))),
                                "FILES",
                                icon = icon("file", lib = "font-awesome"),
                                color = "primary"),
              bs4Dash::valueBox(value = h4(textOutput(ns("datarows"))),
                                "DATA ROWS",
                                icon = icon("table", lib = "font-awesome"),
                                color = "primary")

            ),
            bs4Dash::box(width = 9,
                         title = "Uploaded files",
                         status = "primary",
                         id = "uploadsbox",
                         collapsible = TRUE,
                         closable = FALSE,
                         conditionalPanel("output.filenum === 0", ns = ns, p("No files uploaded yet.")),
                         DTOutput(ns("contents"))
            )
          )

        )
  )

}


uploadInput_server = function(id, files, datasets) {

  moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      #Observe first dataset events
      observeEvent(c(input$files1,input$dataset1), {
        if(!is.null(input$files1) ){
          files$files[["files1"]] = cbind(dataset = input$dataset1, input$files1)
          files$n = nrow(do.call('rbind', files$files))
          datasets$datarows = nrow(data())
          datasets$df = data()

        }
      })

      #Observe second dataset events
      observeEvent(c(input$files2,input$dataset2), {
        if(!is.null(input$files2)){
          files$files[["files2"]] = cbind(dataset = input$dataset2, input$files2)
          files$n = nrow(do.call('rbind', files$files))
          datasets$datarows = nrow(data())
          datasets$df = data()

        }
      })

      #Observe third dataset events
      observeEvent(c(input$files3,input$dataset3), {
        if(!is.null(input$files3) ){
          files$files[["files3"]] = cbind(dataset = input$dataset3, input$files3)
          files$n = nrow(do.call('rbind', files$files))
          datasets$datarows = nrow(data())
          datasets$df = data()
        }
      })


      #triggered when add_btn is pressed
      observeEvent(input$add_btn, {
        datasets$n = datasets$n + 1
        insertUI(selector = "#anchor",
                 where = "beforeBegin",
                 ui = add_box(id = id,
                              name =datasets$n,
                              dataset_id = ns(paste0("dataset", datasets$n)),
                              file_id = ns(paste0("files", datasets$n)))
        )
      })

      #triggered when rm_btn is pressed
      observeEvent(input$rm_btn, {
        if (datasets$n > 1) {
          if(!is.null(files$files[[paste0("files", datasets$n)]])) {
            files$files = files$files[-which(names(files$files) == paste0("files", datasets$n))]
            files$n = nrow(do.call('rbind', files$files))
            datasets$datarows = nrow(data())
            datasets$df = data()
          }
          removeUI(selector = paste0("#new_box_", datasets$n))
          datasets$n = datasets$n - 1
        }
      })

      #Unlist stored files
      files_df = reactive({ do.call('rbind', files$files) })

      #render table of uploaded files
      output$contents <- renderDT({
        req(shiny::isTruthy(length(files_df()) != 0))
        df = files_df()[,c("dataset", "name", "size")]
        df$size = unlist(lapply(df$size, function(x) utils:::format.object_size(x, "auto")))
        add_custom_DT(df, lineHeight = '80%', scrollY = '450')
      })

      data = reactive({
        # Empty list to hold results
        ll <- list()
        for(f in c(1:nrow(files_df())) ){
          # Read from file
          temp = read_input_files(files_df()[f,"datapath"])
          # Save modified filename as field value
          temp$dataset = files_df()[f,"dataset"]
          # Save within a list
          ll[[f]] <- temp
        }
        data = do.call(plyr::rbind.fill, ll)
        data = format_data(data)

        return(data)
      })


      #outputs for first page cards
      output$counter =  reactive({datasets$n})
      output$filenum = reactive({files$n})
      output$datarows = renderText({datasets$datarows})



    })
}
