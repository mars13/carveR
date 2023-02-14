mod_uploads_ui = function(id){
  ns = NS(id)

    tagList(
    sidebarLayout(
      sidebarPanel(
        add_box(id = 1),
        hr(id = "anchor"),
        fluidRow(
          column(1,
                 conditionalPanel("output.counter <= 2 & output.filenum !== 0",
                                  actionButton("add_btn", icon("plus", class = NULL, lib = "font-awesome"))
                 )
          ),
          column(1,
                 conditionalPanel("output.counter > 1",
                                  actionButton("rm_btn", icon("minus", class = NULL, lib = "font-awesome"))
                 )
          )),

      ),
      mainPanel(
        fluidRow(
          bs4Dash::valueBox(h4(textOutput("counter")),
                            "DATASETS",
                            icon = icon("book", lib = "font-awesome"),
                            color = "success"),
          bs4Dash::valueBox(h4(textOutput("filenum")),
                            "FILES",
                            icon = icon("file", lib = "font-awesome"),
                            color = "info"),
          bs4Dash::valueBox(h4(textOutput("datarows")),
                            "DATA ROWS",
                            icon = icon("table", lib = "font-awesome"),
                            color = "warning")

        ),
        bs4Dash::box(width = 9,
            title = "Uploaded files",
            status = "primary",
            id = "uploadsbox",
            collapsible = TRUE,
            closable = FALSE,
            conditionalPanel("output.filenum === 0", p("No files uploaded yet", style="color:grey")),
            tableOutput("contents")
        )
      )
    ))

}


mod_uploads_server <- function(id, datasets, files){
  #assertions::assert_reactive(maf_data_pool)

  moduleServer(
    id,
    function(input, output, session) {
  })
}
