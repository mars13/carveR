geneFilters_ui = function(id) {
  ns = NS(id)
  tagList(
    shinyjs::useShinyjs(),
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
               uiOutput(ns('whitelist')),
               br(),
               uiOutput(ns('genewlInfo'))
             )
      ),
      column(6,
             div(
               id = "blform",
               uiOutput(ns('blacklist')),
               br(),
               uiOutput(ns('geneblInfo')))
      )
    )
  )
}



geneFilters_server = function(id, gene_whitelist, gene_blacklist) {

  moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      #Observe event, store info and set disabled=T if file exists
      observeEvent(input$genewlArea, {
        if(!is.null(input$genewlArea)) {
          genes = strsplit(input$genewlArea, "\n")
          names(genes) = "genes"
          genes[["genes"]] = genes[["genes"]][genes[["genes"]] != ""]
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
            shinyjs::reset(id = 'genewlFile')
          } else {
            colnames(wldata) = "genes"
            gene_whitelist$genes = wldata
            gene_whitelist$disabled = T
            gene_whitelist$clear = F
          }
        }
      })

      observeEvent(input$genewlRemove, {
        shinyjs::reset(id = 'genewlArea')
        shinyjs::reset(id = 'genewlFile')
        gene_whitelist$genes = NULL
        gene_whitelist$clear = TRUE
        gene_whitelist$disabled = F
      })

      output$whitelist = renderUI({
        add_input_list(id = ns("genewl"),
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
      #Observe event, store info and set disabled=T if file exists
      observeEvent(input$geneblArea, {
        if(!is.null(input$geneblArea)) {
          genes = strsplit(input$geneblArea, "\n")
          names(genes) = "genes"
          genes[["genes"]] = genes[["genes"]][genes[["genes"]] != ""]
          gene_blacklist$genes = genes
        }
      })

      observeEvent(input$geneblFile, {
        if(!is.null(input$geneblFile)){
          bldata = read.delim(input$geneblFile$datapath, header = F)
          if(ncol(bldata)>1)
          {
            shinyalert::shinyalert("Column Error",
                                   "Uploaded Data has more than 1 column",
                                   type="error")
            shinyjs::reset(id = 'geneblFile')
          } else {
            gene_blacklist$genes = read.delim(input$geneblFile$datapath, header = F, col.names = "genes")
            gene_blacklist$disabled = T
            gene_blacklist$clear = F
          }
        }
      })

      observeEvent(input$geneblRemove, {
        shinyjs::reset(id = 'geneblArea')
        shinyjs::reset(id = 'geneblFile')
        gene_blacklist$genes = NULL
        gene_blacklist$clear = TRUE
        gene_blacklist$disabled = F
      })

      output$blacklist = renderUI({
        add_input_list(id = ns("genebl"),
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


    })

}
