#' Add dataset input box
#'
#' @param id id corresponding to the dataset number
addBox_ui= function(id) {
  ns = NS(id)
  uiOutput(ns("new_box"))

}


addBox_server = function(id, name) {
  moduleServer(id,
               function(input, output, session) {
                 output$new_box = renderUI({
                   tags$div(id = paste0("new_box_", name),
                            box(
                              title = paste0("Dataset ",name),
                              status = "primary",
                              width = 12,
                              solidHeader = T,
                              textInput(inputId =paste0("dataset ",name), label = "Name", value = "", width = NULL, placeholder = "Write dataset name..."),
                              fileInput(
                                inputId = paste0("files ",name),
                                label = "Choose files",
                                multiple = TRUE,
                                accept = c("text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".tsv")))
                   )
                 })
               })
}

