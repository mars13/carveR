homePage_ui = function(id){
  ns = NS(id)
  tagList(
    br(),
    fluidRow(
      column(width = 12, align="center",
      img(src="carver_hexsticker.png",
          style = "width: 10%; max-width: 250px; min-width: 100px; align: center; height: auto; display: block; margin: auto",
          align = "center")),
    ),
    br(),
    fluidRow(
      column(width = 12, align = "center",
             h2("Cancer Variants Prioritisation Dashboard for Virtual Gene Panels",
                style = "text-align: center; font-size: 2.8vh; color: #46B390"))
    ),

    br(),
    br(),
    fluidRow(
      column(12, align="center",
            box(title = "OVERVIEW",
                solidHeader = F,
                status = "primary",
                closable = F,
                collapsible = F,
                width = 9,
                img(src="300ppi/homepage_workflow.png",
                    style = "width: 95%; max-width: 900px; min-width: 450px; height: auto; display: block; margin: auto",
                    align = "center"))
      )

    )
  )
}

#' home Server Functions
#'
#' @noRd
homePage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns = session$ns

  })
}
