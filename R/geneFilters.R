wlDescription = tagList(p(
    strong("WHITELIST USE: "),
    "Genes in the whitelist can be used in prioritisation or to filter out other genes. This can be selected from the 'Dashboard'."
  )
)

blDescription =  tagList(p(
    strong("BLACKLIST USE: "),
    "Genes in the blacklist will automatically be filtered out."))

generalInfo = tagList(
  p(strong("FORMAT REQUIREMENTS: "),
    "Format should be one gene per row. No header should be present. The first row should already contain a gene name."
  ),
  p(strong("RESET BUTTON: "),
  "Press the reset button to reset both file and text fields."),
  p(strong("INTERACTIVE EDITING: "),
    "Add genes to the list by typing them and clicking 'Add'. Delete genes from the list by selecting a row in the table on the left side and then pressing 'Delete'. ")
)

descriptionStyle = 'color: #353238; font-weight: lighter; padding: 1.5em;'

geneFilters_ui = function(id) {
  ns = NS(id)
  tagList(
    shinyjs::useShinyjs(),
    add_geneList_section(
      id = ns("genewlSection"),
      title = "Gene whitelist",
      sidebar_content = tags$div(wlDescription , generalInfo, style = descriptionStyle),
      sidebar_background = "#E7F8F1",
      sidebar_icon = "circle-info",
      content =
        tagList(fluidRow(column(
          5,
          div(id = "wlform",
              uiOutput(ns('whitelist')))
        ),
        column(
          7,
          div(id = "wltable",
              fluidRow(
                column(
                  6,
                  uiOutput(ns('genewlInfo')),
                  br(),

                  textInput(
                    ns("newWLGene"),
                    "Type gene to add or delete:",
                    placeholder = "Gene name (HGNC)",
                    width = '200px'
                  ),
                  div( style = "display: 'inline'",
                  actionButton(ns("addWLGene"), "Add"),
                  actionButton(ns("deleteWLGene"), "Delete")
                  )

                ),
                column(5,
                       DTOutput(ns("genewlTable")))
              ))
        )))

    ),

    add_geneList_section(
      id = ns("geneblSection"),
      title = "Gene blacklist",
      status = "danger",
      sidebar_content = tags$div(blDescription , generalInfo, style = descriptionStyle),
      sidebar_background = "#FCECEB",
      sidebar_icon = "circle-info",
      content =
        tagList(fluidRow(column(
          5,
          div(id = "blform",
              uiOutput(ns('blacklist')))
        ),
        column(
          7,
          div(id = "bltable",
              fluidRow(
                column(
                  6,
                  uiOutput(ns('geneblInfo')),
                  br(),

                  textInput(
                    ns("newBLGene"),
                    "Type gene to add or delete:",
                    placeholder = "Gene name (HGNC)",
                    width = '200px'
                  ),
                  div( style = "display: 'inline'",
                       actionButton(ns("addBLGene"), "Add"),
                       actionButton(ns("deleteBLGene"), "Delete")
                  )

                ),
                column(5,
                       DTOutput(ns("geneblTable")))
              ))
        )))

    ),
  )
}



geneFilters_server = function(id, gene_whitelist, gene_blacklist) {
  moduleServer(id,
               function(input, output, session) {
                 ns = session$ns

                 #WHITELIST OBSERVER FUNCTIONS --------------------------

                 #Observe WL area input
                 observeEvent(input$genewlArea, {
                   if (!is.null(input$genewlArea)) {
                     genes = strsplit(input$genewlArea, "\n")
                     names(genes) = "genes"
                     genes[["genes"]] = genes[["genes"]][genes[["genes"]] != ""]
                     gene_whitelist$genes = unique(genes)
                   }
                 })

                 #Observe WL file input
                 observeEvent(input$genewlFile, {
                   if (!is.null(input$genewlFile)) {
                     wldata = read.delim(input$genewlFile$datapath, header = F)
                     if (ncol(wldata) > 1)
                     {
                       shinyalert::shinyalert("Column Error",
                                              "Uploaded Data has more than 1 column",
                                              type = "error")
                       shinyjs::reset(id = 'genewlFile')
                     } else {
                       colnames(wldata) = "genes"
                       gene_whitelist$genes = unique(wldata)
                       gene_whitelist$row_selected = NULL
                       gene_whitelist$clear = F
                     }
                   }
                 })

                 #Observe WL reset inputs
                 observeEvent(input$genewlRemove, {
                   shinyjs::reset(id = 'genewlFile')
                   gene_whitelist$genes = NULL
                   gene_whitelist$clear = TRUE
                   gene_whitelist$row_selected = NULL
                 })



                 #Observe WL add genes if input is not empty
                 observeEvent(input$addWLGene,
                              {
                                if (!is.null(gene_whitelist$genes) & length(unlist(gene_whitelist$genes)) > 0) {
                                  current_genes = unlist(gene_whitelist$genes)
                                  new_genes = unique(c(current_genes, toupper(input$newWLGene)))

                                  gene_whitelist$genes = new_genes
                                }
                              })

                 #Observe WL delete genes if input is not empty
                 observeEvent(input$deleteWLGene, {
                   if (!is.null(gene_whitelist$genes) & length(unlist(gene_whitelist$genes)) > 0) {

                     current_genes = unlist(gene_whitelist$genes)
                     new_genes = setdiff(current_genes, toupper(input$newWLGene))

                     gene_whitelist$genes = new_genes
                   }
                 })


                 genesWL = reactive({
                   unlist(gene_whitelist$genes)
                 })

                 #WHITELIST UI OUTPUTS---------

                 #Output WL section
                 output$whitelist = renderUI({
                   if (is.null(genesWL()) | identical(genesWL(), character(0))) {
                     value = ""
                   } else {
                     value = HTML(paste(genesWL(), collapse = "\n"))
                   }


                   add_input_list(id = ns("genewl"),
                                  title = "Gene whitelist",
                                  value = value)
                 })

                 #Output WL gene number
                 output$genewlInfo = renderUI({
                   if (!is.null(gene_whitelist$genes)) {
                     x = length(unlist(gene_whitelist$genes))
                   } else {
                     x = 0
                   }
                   add_number_info(x, "WHITELIST GENES","primary")
                 })

                 #Output WL genes (datatable)
                 output$genewlTable = DT::renderDT({
                   req(shiny::isTruthy(length(unlist(
                     gene_whitelist$genes
                   )) != 0))
                   df = data.frame(Genes = unlist(gene_whitelist$genes),
                                   row.names = NULL)
                   add_custom_DT(df)

                 })


                 #BLACKLIST OBSERVER FUNCTIONS --------------------------

                 #Observe BL area input
                 observeEvent(input$geneblArea, {
                   if (!is.null(input$geneblArea)) {
                     genes = strsplit(input$geneblArea, "\n")
                     names(genes) = "genes"
                     genes[["genes"]] = genes[["genes"]][genes[["genes"]] != ""]
                     gene_blacklist$genes = unique(genes)
                   }
                 })

                 #Observe BL file input
                 observeEvent(input$geneblFile, {
                   if (!is.null(input$geneblFile)) {
                     bldata = read.delim(input$geneblFile$datapath, header = F)
                     if (ncol(bldata) > 1)
                     {
                       shinyalert::shinyalert("Column Error",
                                              "Uploaded Data has more than 1 column",
                                              type = "error")
                       shinyjs::reset(id = 'geneblFile')
                     } else {
                       colnames(bldata) = "genes"
                       gene_blacklist$genes = unique(bldata)
                       gene_blacklist$row_selected = NULL
                       gene_blacklist$clear = F
                     }
                   }
                 })

                 #Observe BL reset inputs
                 observeEvent(input$geneblRemove, {
                   shinyjs::reset(id = 'genewlFile')
                   gene_blacklist$genes = NULL
                   gene_blacklist$clear = TRUE
                   gene_blacklist$row_selected = NULL
                 })



                 #Observe WL add genes if input is not empty
                 observeEvent(input$addBLGene,
                              {
                                if (!is.null(gene_blacklist$genes) & length(unlist(gene_blacklist$genes)) > 0) {
                                  current_genes = unlist(gene_blacklist$genes)
                                  new_genes = unique(c(current_genes, toupper(input$newBLGene)))

                                  gene_blacklist$genes = new_genes
                                }
                              })

                 #Observe BL delete genes if input is not empty
                 observeEvent(input$deleteBLGene, {
                   if (!is.null(gene_blacklist$genes) & length(unlist(gene_blacklist$genes)) > 0) {

                     current_genes = unlist(gene_blacklist$genes)
                     new_genes = setdiff(current_genes, toupper(input$newBLGene))

                     gene_blacklist$genes = new_genes
                   }
                 })


                 genesBL = reactive({
                   unlist(gene_blacklist$genes)
                 })

                 #BLACKLIST UI OUTPUTS---------

                 #Output BL section
                 output$blacklist = renderUI({
                   if (is.null(genesBL()) | identical(genesBL(), character(0))) {
                     value = ""
                   } else {
                     value = HTML(paste(genesBL(), collapse = "\n"))
                   }


                   add_input_list(id = ns("genebl"),
                                  title = "Gene blacklist",
                                  value = value)
                 })

                 #Output BL gene number
                 output$geneblInfo = renderUI({
                   if (!is.null(gene_blacklist$genes)) {
                     x = length(unlist(gene_blacklist$genes))
                   } else {
                     x = 0
                   }
                   add_number_info(x, "BLACKLIST GENES","danger")
                 })

                 #Output BL genes (datatable)
                 output$geneblTable = DT::renderDT({
                   req(shiny::isTruthy(length(unlist(
                     gene_blacklist$genes
                   )) != 0))
                   df = data.frame(Genes = unlist(gene_blacklist$genes),
                                   row.names = NULL)

                   add_custom_DT(df)
                 })

               })

}
