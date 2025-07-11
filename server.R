server <- function(input, output, session) {
  authors <- reactiveVal(list())
  entries <- reactiveVal(list())
  
  # Add new author
  observeEvent(input$add_author, {
    req(input$author_name)
    
    new_author <- list(
      name = input$author_name,
      affiliation = input$author_affiliation,
      orcid = ifelse(nzchar(input$author_orcid), input$author_orcid, NULL)
    )
    
    updated_authors <- append(authors(), list(new_author))
    authors(updated_authors)
    
    # Update selection box
    updateCheckboxGroupInput(
      session,
      "selected_authors",
      choices = sapply(updated_authors, function(a) a$name)
    )
  })
  
  # Add new metadata entry
  observeEvent(input$add, {
    req(input$pdf, input$title)
    
    # Match selected authors to full list
    selected <- input$selected_authors
    full_authors <- authors()
    selected_authors <- lapply(full_authors, function(a) {
      if (a$name %in% selected) {
        list(
          name = a$name,
          affiliation = a$affiliation,
          orcid = a$orcid
        )
      } else NULL
    }) |> purrr::compact()
    
    keywords <- str_split(input$keywords, ";")[[1]] |> str_trim()
    
    new_entry <- list(
      metadata = list(
        title = input$title,
        upload_type = "publication",
        publication_type = "article",
        description = input$description,
        creators = selected_authors,
        keywords = keywords,
        publication_date = as.character(input$pub_date),
        access_right = input$access_right,
        license = input$license
      ),
      file_path = input$pdf$datapath
    )
    
    updated_entries <- append(entries(), list(new_entry))
    entries(updated_entries)
  })
  
  # Display entries
  output$entry_table <- renderDT({
    dat <- lapply(entries(), function(e) {
      data.frame(
        Title = e$metadata$title,
        Authors = paste(sapply(e$metadata$creators, `[[`, "name"), collapse = "; "),
        File = basename(e$file_path),
        Date = e$metadata$publication_date,
        Access = e$metadata$access_right,
        License = e$metadata$license,
        stringsAsFactors = FALSE
      )
    })
    if (length(dat) > 0) datatable(do.call(rbind, dat))
  })
  
  # Display author list
  output$author_table <- renderDT({
    aut <- authors()
    if (length(aut) > 0) {
      datatable(do.call(rbind, lapply(aut, as.data.frame)))
    }
  })
  
  # Download JSON
  output$download_json <- downloadHandler(
    filename = function() {
      paste0("zenodo_metadata_", Sys.Date(), ".json")
    },
    content = function(file) {
      write_json(entries(), file, pretty = TRUE, auto_unbox = TRUE, null = "null")
    }
  )
}