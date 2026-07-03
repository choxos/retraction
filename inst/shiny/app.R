# Bundled Shiny triage app for the retraction package. Launch via
# retraction::retraction_app().

accepted <- c(".bib", ".ris", ".json", ".xml", ".enw", ".docx", ".pdf",
              ".txt", ".md", ".rmd", ".qmd", ".tex", ".html", ".csv", ".tsv")

ui <- shiny::fluidPage(
  shiny::titlePanel("retraction: scan a bibliography for retracted work"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::fileInput("file", "Bibliography or document", accept = accepted),
      shiny::checkboxInput("offline", "Offline (use local snapshot)", FALSE),
      shiny::helpText(
        "Online mode sends only identifiers (DOIs/PMIDs) to the retraction",
        "sources. Offline mode requires a snapshot built with retraction_sync()."
      ),
      shiny::hr(),
      shiny::downloadButton("dl", "Download results (CSV)")
    ),
    shiny::mainPanel(
      shiny::verbatimTextOutput("summary"),
      DT::DTOutput("table")
    )
  )
)

server <- function(input, output, session) {
  result <- shiny::reactive({
    shiny::req(input$file)
    path <- input$file$datapath
    ext <- tools::file_ext(input$file$name)
    if (nzchar(ext)) {
      withext <- paste0(path, ".", ext)
      if (file.copy(path, withext, overwrite = TRUE)) path <- withext
    }
    shiny::withProgress(message = "Checking references...", {
      tryCatch(
        retraction::check_file(path, offline = isTRUE(input$offline), progress = FALSE),
        error = function(e) {
          shiny::showNotification(paste("Could not check file:", conditionMessage(e)),
                                  type = "error", duration = NULL)
          NULL
        }
      )
    })
  })

  output$summary <- shiny::renderPrint({
    shiny::req(result())
    print(result())
  })

  output$table <- DT::renderDT({
    shiny::req(result())
    df <- as.data.frame(result())
    cols <- intersect(c("id", "doi", "pmid", "status", "is_retracted", "confidence",
                        "matched_title", "journal", "retraction_date", "reason",
                        "sources"), names(df))
    DT::datatable(df[, cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE)) |>
      DT::formatStyle("is_retracted", target = "row",
                      backgroundColor = DT::styleEqual(TRUE, "#fdecea"))
  })

  output$dl <- shiny::downloadHandler(
    filename = function() "retraction-results.csv",
    content = function(file) utils::write.csv(as.data.frame(result()), file, row.names = FALSE)
  )
}

shiny::shinyApp(ui, server)
