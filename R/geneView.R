geneView_ui <- function(id) {
  ns = NS(id)

  tagList(fluidRow(column(
    2, selectInput(ns("select_gene"), "Select gene", choices = "")
  ),),
  fluidRow(
    box(
      title = "Gene stats",
      status = "primary",
      width = 12,
      conditionalPanel(
        "output.valid_gene === 'VALID'",
        ns = ns,
        shinycssloaders::withSpinner(fluidRow(
          column(width = 3, style = "min-width: 25%; max-width: 200px;",
                 plotlyOutput(ns("datasetPlot"))),
          column(width = 3, style = "min-width: 25%; max-width: 200px;",
                 plotlyOutput(ns("variantPlot"))),
          column(width = 6, style = "min-width: 250px;",
                 plotlyOutput(ns(
                   "consequencePlot"
                 )))
        ),
        type = 8)
      ),

      conditionalPanel(
        "output.valid_gene === 'WARNING'",
        ns = ns,
        span(icon("binoculars"), "The requested gene is not present in the data. Please enter a valid gene name to proceed.", style = "color: #404040;")
      )
    ),
    box(
      title = "Variant table",
      status = "primary",
      width = 12,
      downloadButton(ns("download_tsv"), "Download full table (.tsv)"),

      conditionalPanel(
        "output.valid_gene === 'VALID'",
        ns = ns,
        shinycssloaders::withSpinner(DT::DTOutput(ns("varTable")),
                                     type = 8)
      )
    )
  ))

}



geneView_server = function(id, datasets) {
  moduleServer(id,
               function(input, output, session) {
                 ns = session$ns

                 data = reactive({
                   as.data.frame(datasets$df)
                 })

                 observe({
                   updateSelectInput(session, "select_gene", choices = sort(unique(data()$gene)))
                 })

                 output$selectedGene = renderText(input$select_gene)



                 output$valid_gene = renderText(if (input$select_gene %in% data()$gene) {
                   "VALID"
                 } else {
                   "WARNING"
                 })
                 outputOptions(output, "valid_gene", suspendWhenHidden = FALSE)


                 not_all_na =  function(x)
                   any(!is.na(x))

                 gene_data = reactive({
                   edit_consequence_summary(data()[data()$gene == input$select_gene, ]) %>%
                     select(where(not_all_na))
                 })

                 output$selectedGene = renderText(input$select_gene)

                 theme_colors = c("#34AF88", "#F6CA28", "#E21A1A", "#5BC8C8")

                 output$datasetPlot = renderPlotly({
                   plotly_piechart(gene_data(), "dataset", theme_colors, "Dataset")
                 })

                 output$variantPlot = renderPlotly({
                   plotly_piechart(gene_data(),
                                   "variant_type",
                                   theme_colors,
                                   "Variant type")
                 })

                 output$consequencePlot = renderPlotly({
                   plotly_barplot(gene_data(),
                                  "consequence_summary",
                                  theme_colors[1],
                                  "Variant consequence")
                 })

                 output$varTable = DT::renderDT(server = T, {
                   cols = c("dataset",
                            "variant_type",
                            "consequence_summary",
                            colnames(gene_data())[!colnames(gene_data()) %in% c("variant_id",
                                                                                "sample_id",
                                                                                "gene",
                                                                                "variant_type",
                                                                                "consequence_summary")])
                   datatable(
                     gene_data()[, cols],
                     filter = "top",
                     rownames = F,
                     callback = JS("$('div.dwnld').append($('#download_tsv'));"),
                     extensions = "FixedColumns",
                     options = list(
                       paging = TRUE,
                       scrollX = TRUE,
                       searching = TRUE,
                       ordering = F,
                       dom = 'B<"dwnld">frtip',
                       pageLength = 10,
                       fixedColumns = list(leftColumns = 3)
                     )
                   ) %>% formatStyle(
                     columns = c(1:3),
                     target = 'cell',
                     backgroundColor =  colorspace::lighten(theme_colors[1], 0.9),
                   )
                 })



                 output$download_tsv = downloadHandler(
                   filename = function() {
                     paste("carver_",
                           unique(gene_data()$gene),
                           "_variants-",
                           Sys.Date(),
                           ".tsv",
                           sep = "")
                   },
                   content = function(file) {
                     write.table(
                       gene_data(),
                       file,
                       sep = "\t",
                       quote = F,
                       col.names = T,
                       row.names = F
                     )
                   }
                 )


               })
}
