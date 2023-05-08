#' Add dataset input box
#'
#' @param id id corresponding to the dataset number
add_box = function(id, name, dataset_id, file_id){

  ns = NS(id)

  tags$div(id = paste0("new_box_",name),
           box(
             title = paste("Dataset", name),
             status = "primary",
             width = 12,
             solidHeader = T,
             textInput(inputId = dataset_id, label = "Name", value = "", width = NULL, placeholder = "Write dataset name..."),
             fileInput(
               inputId = file_id,
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
add_radiobtn = function(id, ns_id, item){
  tags$div(id = paste0("new_radio_", id),
           radioButtons(inputId = paste0(ns_id, "_radio"),
                        label = paste0("Sort ", item, ":"),
                        choices = c("Ascending", "Descending"),
                        selected = "Descending",
                        inline = T)
  )
}


#' Add gene list input form
#'
#' This function creates an input form for uploading or manually entering a gene list.
#'
#' @param id A character string representing the ID of the gene list input form.
#' @param title A character string representing the title of the input form box.
#' @param value A character string representing the initial value for the text area input.
#' @param status A character string representing the color of the input form box.
#'
#' @return A `div` element containing a file input, a text area input, and a reset button.
#'
#' @import shiny
#'
#' @examples
#' add_input_list("gene_list", "Gene List", "", "primary")
#'
#' @export
add_input_list = function(id, title, value = "", status = "primary") {
  tags$div(id = id,
                fileInput(inputId = paste0(id,"File"),
                          label = "Upload a file...",
                          multiple = F, width = "80%",
                          accept = c("text/plain",
                                     ".tsv", ".txt")),
                textAreaInput(inputId = paste0(id,"Area"),
                              value = value,
                              label = "...or type/paste below",
                              placeholder = "Gene1\nGene2\nGene3",
                              width = "80%",
                              height = '86px'),
                actionButton(paste0(id,"Remove"), 'Reset')

  )
}


#' Add User Info Box
#'
#' This function creates a box containing a title and content.
#'
#' @param id A character string representing the id of the box.
#' @param title A character string representing the title of the box.
#' @param content A list of elements to be included in the content area of the box.
#' @param status A character string representing the status of the box. (e.g., "primary",
#'   "info", "danger").
#'
#' @return A `box` containing a title and content.
#'
#' @import shinydashboard
#'
#' @examples
#' add_userInfo_box("user_info", "User Information", tagList(), "primary")
#'
#' @export
add_userInfo_box = function(id, title, content, status){
    box(
      title = title,
      status = status,
      solidHeader = T,
      width = 12,
      content
    )
}

#' Add Gene List Section
#'
#' This function creates a collapsible box containing a title, a sidebar, and content.
#' The sidebar is created using the `bs4CardSidebar` function from the `shinyBS` package.
#'
#' @param id A character string representing the id of the section.
#' @param title A character string representing the title of the section.
#' @param sidebar_content A tagList of elements to be included in the sidebar.
#' @param sidebar_icon A character string representing the icon name (free font-awesome icons only) to be used in the sidebar.
#' @param sidebar_background A character string representing the background color of the sidebar.
#' @param content A list of elements to be included in the content area of the section.
#' @param status A character string representing the status of the box. (e.g., "primary",
#'   "info", "danger").
#'
#' @return A `div` containing a collapsible box with a title, a sidebar, and content.
#'
#' @import shinyBS
#'
#' @examples
#' add_geneList_section("gene_list", "Gene List", tagList(),
#'                       sidebar_icon = "info", sidebar_background = "#272c30", tagList())
#'
#' @export
add_geneList_section = function(id, title, sidebar_content, sidebar_icon = "info", sidebar_background = "#272c30", content, status = "primary"){
  tags$div(id = id,
           box( title = title,
                status = status,
                width = 11,
                solidHeader = T,
                collapsible = T,
                sidebar = bs4CardSidebar(
                  icon = shiny::icon(sidebar_icon),
                  id = paste0("sidebar_",title),
                  background = sidebar_background,
                  sidebar_content
                ),
                content
           )

  )

}


#' Add Number Info
#'
#' This function creates a tag list containing a badge and some text. The badge is
#' created using the `dashboardBadge` function from the `shinydashboard` package.
#'
#' @param value A numeric value to be displayed on the badge.
#' @param text A character string to be displayed next to the badge.
#' @param status A character string indicating the status for the color of the badge (e.g., "primary",
#'   "info", "danger").
#'
#' @return A `tagList` containing two `span` elements, one for the badge and one for the text.
#'
#' @import shinydashboard
#'
#' @examples
#' add_number_info(100, "Genes", "danger")
#'
#' @export
add_number_info = function(value, text, status= "primary") {
  tagList(
    span(
      dashboardBadge(
        value,
        color = status,
        position = "left",
        rounded = T
      ),
      style = 'font-size: 25px;'
    ),
    span(tags$sub(text), style = 'font-size: 20px; padding-left: 5px;')
  )
}


#' Format data table for gene filters
#'
#' This function creates a formatted data table for displaying gene filter information.
#'
#' @param df A data frame containing the gene filter information.
#'
#' @return A data table formatted using the DT package with additional CSS styling.
#'
#' @import DT
#' @importFrom htmlwidgets JS
#'
#' @examples
#' add_custom_DT(mtcars)
#'
#' @export
add_custom_DT = function(df, lineHeight = '60%', scrollY = '150') {
  DT::datatable(
    df,
    selection = list(mode = "single"),
    rownames = F,
    class = 'hover',
    filter = list(position = 'top'),
    options = list(
      dom = 't',
      paging = F,
      scrollY = scrollY,
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'font-size': '15px','height': '15px', 'margin-top': '0', 'margin-bottom': '0'});",
        "}"
      )
    )
  ) %>%
    formatStyle(0, target = 'row',  lineHeight = lineHeight)

}
