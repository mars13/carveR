#' Add dataset input box
#'
#' @param id id corresponding to the dataset number
add_box = function(id){
  ns <- NS(id)
  tags$div(id = paste0("new_box", id),
           box(
             title =  paste0("Dataset #", id),
             status = "primary",
             width = 12,
             solidHeader = T,
             textInput(inputId = paste0("dataset", id), label = "Name", value = "", width = NULL, placeholder = "Write dataset name..."),
             fileInput(
               inputId = paste0("files", id),
               label = "Choose files",
               multiple = TRUE,
               accept = c("text/csv",
                          "text/comma-separated-values,text/plain",
                          ".tsv")))
  )
}

#' Add prioritisation radio button
#'
#' @param id id corresponding to the sorting variable name
add_radiobtn = function(id){
  ns <- NS(id)
  tags$div(id = paste0("new_radio", id),
           radioButtons(inputId = paste0("radio_", id),
                        label = paste0("Sort ", id, ":"),
                        choices = c("Ascending", "Descending"),
                        selected = "Descending",
                        inline = T)
  )
}

#' Add gene list
#'
#' @param id ID corresponding to the gene list name
#' @param title Box title
#' @param description Description the gene list use within the app
#' @param status Status of the box, will determine the color
#'
add_input_list = function(id, title, description, status = "primary") {
  tags$div(id = id,
           box( title = title,
                status = status,
                width = 10,
                solidHeader = T,
                collapsible = F,
                p(description),
                textAreaInput(inputId = paste0(id,"Area"),
                              label = "Enter genes...",
                              width = "80%"),
                fileInput(inputId = paste0(id,"File"),
                          label = "... or upload a file",
                          multiple = F, width = "80%",
                          accept = c("text/plain",
                                     ".tsv", ".txt")),
                actionButton(paste0(id,"Remove"), 'Remove')
           )

  )
}
