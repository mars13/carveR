theme_col = "#34AF88"

header_style = paste0("color:", theme_col)

#--- INPUT CONETNT----------
input_content = tagList(
  h5("Datasets", style = header_style),
  fluidRow(column(
    width = 6,
    p(
      "Users can upload up to 3 different datasets, selecting multiple files for each dataset.
       Different datasets are not required to share all types of molecular alterations."
    ),
  ),
  column(
    width = 6,
    img(src = "300ppi/userGuide-datasets.png",
        style = "width: 90%; max-width: 250px; min-width: 150px; align: center; height: auto; display: block; margin: auto")
  )),
  span(
    icon("lightbulb"),
    strong(" TIP "),
    "For improved performance we suggest defining the dataset name before uploading the files."
    ,
    style = "padding-left: 1em; color:#7D868D;"
  ),
  br(),
  br(),
  h5("Format requirements", style = header_style),
  p(
    "
    All data files should be in tab-separated files with .tsv extension.
      Regardless of the variant type, all should contain the following columns:
    "
  ),
  tags$table(
    style = "width:40%",
    tags$tr(tags$th("gene"),
            tags$th("sample_id")),
    tags$tr(tags$td("TP53"),
            tags$td("S0001"))
  ),
  br(),
  p("Additional requirements for each variant file:"),
  fluidRow(
    column(1,  strong("SNV files", style = header_style), style = "min-width: 100px"),
    column(6, tags$table(
      style = "width:80%",
      tags$tr(
        tags$th("chr"),
        tags$th("pos"),
        tags$th("ref"),
        tags$th("alt"),
        tags$th("consequence")
      ),
      tags$tr(
        tags$td("chr17"),
        tags$td("7578211"),
        tags$td("C"),
        tags$td("T"),
        tags$td("missense_variant")
      )
    ))
  ),
  fluidRow(
    column(1,strong("CNV files", style = header_style), style = "min-width: 100px"),
    column(6,
           tags$table(style = "width:50%",
                      tags$tr(tags$th("avecopynumber")),
                      tags$tr(tags$td("-0.72")))
    )),
  fluidRow(column(1,strong("SV files", style = header_style), style = "min-width: 100px"),
           column(6,
                  tags$table(
                    style = "width:60%",
                    tags$tr(
                      tags$th("start_fusion"),
                      tags$th("end_fusion"),
                      tags$th("sv_type")
                    ),
                    tags$tr(tags$td("ETV6"),
                            tags$td("NTRK3"),
                            tags$td("BND"))
                  )
           )),
  br(),
  p(
    "Any additional column (text or numeric) can be included to be used for gene prioritisation."
  ),
)

genelist_content = tagList (
  h5("Format", style = header_style),
  p("Format should be one gene per row."),
  p(
    "No header should be present, the first row should already contain a gene name. Example:"
  ),
  tags$table(
    style = "width:40%",
    tags$tr(tags$td("TP53")),
    tags$tr(tags$td("ETV6")),
    tags$tr(tags$td("NTRAK3")),
    tags$tr(tags$td("RUNX1")),
    tags$tr(tags$td("BRAF"))
  )
)

#---PRIORITISATION CONTENT--------
genefilters_content = tagList(
  h5("Whitelist", style = header_style),
  span(
    strong("Show only:"),
    "
      By setting this option in the Dashboard prioritisation menu (bottom left),
      genes not present in the whitelist will be filter out.
      "
    ,
    style = "padding-left: 1em;"
  ),
  br(),
  br(),
  span(
    strong("Show first:"),
    "
      By setting this option in the Dashboard prioritisation menu (bottom left),,
      genes in the whitelist will be prioritised over all other prioritisation variables
      (see Ranking customisation in this page for more information).
      "
    ,
    style = "padding-left: 1em;"
  ),
  br(),
  br(),
  h5("Blacklist", style = header_style),
  span(
    "If a blacklist is provided, genes in the blacklist will be automatically filtered out.",
    a("somaticFlags", href = "https://github.com/CCICB/somaticflags"),
    "can be used as blacklist of frequently mutated genes in somatic cancer datasets unlikely to drive disease."
  )
)

ranking_custom = tagList(
  h5("Sub-setting menu", style = header_style),
  span(
    strong("Datasets"),
    "and",
    strong("variant types"),
    "can be sub-set using the checkboxes in the sub-setting menu."
  ),
  br(),
  br(),
  span(
    "The maximum number of diplayed genes can be modified by toggling",
    strong("Set max gene number"),
    ". When activated, the barplot will only display the top N genes in the ranking (default=200).
       This threshold will not affect the table below.
       For datasets with more than 500 genes, it will automatically toggled."
  ),
  br(),
  br(),


  h5("Prioritisation menu", style = header_style),
  span(
    "Prioritisation menu allows to select the preferred method for",
    strong("gene whitelist"),
    "(see previous section) and include any other" ,
    strong("prioritisation field"),
    "from the original data."
  ),
  br(),
  p("The order in which prioritisation fields are included will determine their relative importance."),
  p("
    A radio button will appear for each prioritisation field, to determine wether the variable should
    be prioritised in Ascending or Descending order.
    "),
  p("
    As the ranking is provided at gene level, for variant-specific prioritisation fields
    the highest scoring variant for each gene according will be taken into account.
    ")
)

#---OUTPUT CONTENT-------
freq_dash = tagList(
  p(
    "Genes will appear ranked first by frequency, then by presence in the whitelist
    (if provided) and finally by all the prioritisation fields provided in the corresponding order."
  ),
  p("See the following example:"),
  img(src = "300ppi/userGuide-ranking.png",
      style = "width: 100%; max-width: 800px; align: center; height: auto; display: block; margin: auto"),
  span(
    strong("I:"),
    "First priority genes will be the ones in the whitelist list present in cohort A."
  ),
  span(
    strong("II:"),
    "Second priority genes will be the ones in the whitelist list present only in cohort B (not seen in cohort A)."
  ),
  span(
    strong("III:"),
    "Third priority genes will be the ones NOT in the whitelist list present in cohort A."
  ),
  span(
    strong("IV:"),
    "Forth priority genes will be the ones NOT in the whitelist list present only in cohort B (not seen in cohort A)."
  )
)

gene_dash = tagList(
  p("
    Gene view allows to explore at variant level within each gene.
    "),
  p("
    Variant consequence is extracted directly from consequence field for SNVs, from the sv_type field for SV
    and calculated from the avecopynumber for CNVs.
    ")
)

#--- UI ----------
userGuide_ui = function(id) {
  ns = NS(id)

  tagList(tags$head(tags$style(
    HTML("
      .ug_theme ul {
        padding-left: 0;
        margin-bottom: 10px;
      }")
  )),
  tags$div(
    class = 'ug_theme',
    tabsetPanel(
      type = "pills",
      vertical = T,
      id = "ugtabset",
      tabPanel(
        "Inputs",
        add_userInfo_box(
          id = ns,
          title = span(
            icon("file", lib = "font-awesome", class = "fa-solid"),
            "  Input data requirements"
          ),
          status = "primary",
          content = input_content
        ),

        add_userInfo_box(
          id = ns,
          title = span(icon("list"), "  Gene lists (whitelist/blacklist)"),
          status = "primary",
          content = genelist_content
        )

      ),
      tabPanel(
        "Prioritisation",
        add_userInfo_box(
          id = ns,
          title = span(icon("filter"), "  Gene lists filters"),
          status = "primary",
          content = genefilters_content
        ),
        add_userInfo_box(
          id = ns,
          title = span(icon("list-ol"), "  Ranking customisation"),
          status = "primary",
          content = ranking_custom
        ),
      ),
      tabPanel(
        "Outputs",
        add_userInfo_box(
          id = ns,
          title = span(icon("chart-simple"), "  Frequency dashboard"),
          status = "primary",
          content = freq_dash
        ),
        add_userInfo_box(
          id = ns,
          title = span(icon("chart-pie"), "  Gene dashboard"),
          status = "primary",
          content = gene_dash
        )
      )
    )
  ))
}
