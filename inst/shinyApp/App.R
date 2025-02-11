library(nextGenShinyApps)
library(DT)
library(jsonlite)
library(rlist)
library(knitr)
library(markdown)

source("utils.R")


if (exists("csvfolder")){
    folder_path_default <- csvfolder
} else {
    folder_path_default <- getwd()
}


# Define UI using nextGenShinyApps framework
ui <- fluidPage(
  theme = 2,
  header = titlePanel(left = "CSV Viewer App", right = div(img(src = "logo-foundation.png", height = 50, width = 150))),

  mainPanel(
    width = 12,
    
    fluidRow(
      column(width = 8, card(
      toolbar = list(collapse = TRUE,
                       maximize = FALSE, close = FALSE, menu = FALSE),
      selectInput("csv_file", "Select a CSV File:", choices = NULL, width = "100%"),
      title = "Selected File", textOutput("selected_file_name")
    )),
    column(width = 4, card(
      toolbar = list(collapse = TRUE,
                     maximize = FALSE, close = FALSE, menu = FALSE),
      title = "Options",
      textInput("folder", "Folder Path:", value = folder_path_default,width = "100%"),
      checkboxInput("show_only_data", "Show only files with data", value = TRUE)
    ))),
    
    fluidRow(
      column(
        width = 6,
        card(style = "overflow-y:scroll; max-height: 300px; position:relative; align: left", 
             title = "Documentation", 
             uiOutput("markdown")
        )
      ),
      column(
        width = 6,
        card(
          title = "Notes",
          width = 12,
          toolbar = list(collapse = TRUE,
                         maximize = TRUE, close = FALSE, menu = TRUE),
          nextGenShinyApps::textAreaInput(
            inputId = "comments",
            label = "Enter your comments for this file:",
            value = "",
            width = "100%",
            height = "100%",
            resize = "both",
            style = "round"
          ),
           actionButton(inputId = "save", label = "Save Comments")
        )
      )
    ),
    card(
      title = "Data Table",
      width = 12,
      toolbar = list(collapse = TRUE,
                     maximize = TRUE, close = FALSE, menu = TRUE),
      style = "margin-top: 20px;",
      DTOutput("data_table")
    )
    # Add a modal dialog for unsaved changes
    # tags$div(id = "unsaved_changes_modal", class = "modal fade", tabindex = "-1", role = "dialog",
    #          div(class = "modal-dialog modal-dialog-centered", role = "document",
    #              div(class = "modal-content",
    #                  div(class = "modal-header",
    #                      h5(class = "modal-title", "Unsaved Changes"),
    #                      tags$button(type = "button", class = "close", `data-dismiss` = "modal", `aria-label` = "Close",
    #                             span(`aria-hidden` = "true", "&times;")
    #                      )
    #                  ),
    #                  div(class = "modal-body",
    #                      p("You have unsaved changes. Do you want to discard them and proceed?")
    #                  ),
    #                  div(class = "modal-footer",
    #                      actionButton(inputId = "confirm_discard", label = "Discard and Proceed"),
    #                      actionButton(inputId = "cancel_discard", label = "Cancel")
    #                  )
    #              )
    #          )
    # )
  )
)

# Define server logic using nextGenShinyApps framework
server <- function(input, output, session) {
  # Reactive value to store the list of CSV files
  csv_files <- reactiveVal()
  
  # Update the list of CSV files when the folder path is changed
  observeEvent(input$folder, {
    req(input$folder)
    folder_path <- input$folder
    # Check if folder exists
    if (!dir.exists(folder_path)) {
       return()
    }
    
    file_paths_labels <- get_file_paths(folder_path, show_only_data = input$show_only_data, max_depth = 5)

    if (!is.null(file_paths_labels)) {
      
      file_paths <- file_paths_labels$file_paths
      file_labels <- file_paths_labels$file_labels
      
      # Update the reactive value
      csv_files(file_paths)
      
      # Update the selectInput with file names only
      updateSelectInput(
        session, 
        "csv_file", 
        choices = setNames(file_paths, file_labels), # Use filtered file paths and labels
        selected = if (length(file_paths) > 0) file_paths[1] else NULL
      )
      
      load_selected_csv_file()
      loading(FALSE)
      print(loading())
    } 

  })
  
  # change the list of CSV files when the show_only_data checkbox is changed
  observeEvent(input$show_only_data, {
    folder_path <- input$folder
    
    # Check if folder exists
    if (!dir.exists(folder_path)) {
      showNotification("The specified folder does not exist.", type = "error")
      return()
    }
    
    file_paths_labels <- get_file_paths(
      folder_path,
      show_only_data = input$show_only_data,
      max_depth = 5
    )
    
    if (!is.null(file_paths_labels)) {
      file_paths <- file_paths_labels$file_paths
      file_labels <- file_paths_labels$file_labels
      
      # Update the reactive value
      csv_files(file_paths)
      
      # Update the selectInput with file names only
      updateSelectInput(
        session,
        "csv_file",
        choices = setNames(file_paths, file_labels),
        # Use filtered file paths and labels
        selected = if (length(file_paths) > 0)
          file_paths[1]
        else
          NULL
      )
    }
  }
  )
  
  # Display the name of the selected CSV file
  output$selected_file_name <- renderText({
    req(input$csv_file)
    selected_file <- input$csv_file
    # Get two levels of parent folders and the file name
    file_path_parts <- strsplit(normalizePath(selected_file), .Platform$file.sep)[[1]]
    path_length <- length(file_path_parts)
    # Get file information
    file_info <- file.info(selected_file)
    
    # Extract creation date and size
    creation_date <- as.Date(file_info$ctime)
    file_size <- file_info$size   
    
    
    if (path_length >= 3) {
      paste(paste(file_path_parts[(path_length - 2):path_length], collapse = "/"), "creation date:", creation_date, " size:", file_size, "bytes")
    } else {
      paste(basename(selected_file), "creation date:", creation_date, " size:", file_size, "  bytes")
    }
  })
  
  # Load and render the selected CSV file
  output$data_table <- renderDT({
    req(input$csv_file)
    selected_file <- input$csv_file
    req(length(selected_file) == 1)

    # Read the selected CSV file
    data <- tryCatch(
      read.csv(selected_file, stringsAsFactors = FALSE),
      error = function(e) {
        showNotification("Failed to read the CSV file.", type = "error")
        return(NULL)
      }
    )
    req(!is.null(data))

    # Render the data table
    datatable(data, options = list(scrollX = TRUE, searching = TRUE, pageLength = 50))
  })
  
  # Save the comments to the JSON file
  observeEvent(input$save, {
    # Retrieve the comments
    comments <- input$comments
    
    # Define the file path to save the JSON
    comments_json <- paste0(input$folder,"/comments.json")
    
    new_entry <- list(
      name = basename(input$csv_file),
      comments = comments,
      updated = format(Sys.time(), "%A, %B %d, %Y %I:%M:%S %p")
    )
    # Write comments to the JSON file
    update_json_file(comments_json, "files", new_entry, "name")
    
  })
  
  # Check with user if unsaved changes to comments when a new file is selected
  observeEvent(input$csv_file, {
    load_selected_csv_file()
  })
  
  # Load the selected CSV file, documentation, and comments
  load_selected_csv_file <- function() {

    req(input$csv_file)
    selected_file <- input$csv_file
    
    # update comments
    comments_json <- paste0(input$folder, "/comments.json")
    comments <- get_comments_by_filename(comments_json, basename(selected_file))
    
    if (!is.null(comments)) {
      updateTextAreaInput(session, "comments", value = comments)
    } else {
      updateTextAreaInput(session, "comments", value = "")
    }
    
    # update documentation
    description <- get_markdown_by_filename(input$folder, selected_file)
    
    if (is.null(description)) {
      output$markdown <- renderUI({
        includeMarkdown("**No documentation available for this file.**")
      })
    } else {
      output$markdown <- renderUI({
        includeMarkdown(description)
      })
    }
    
    # Read the selected CSV file
    output$data_table <- renderDT({
      req(length(selected_file) == 1)
      
      # Read the selected CSV file
      data <- tryCatch(
        read.csv(selected_file, stringsAsFactors = FALSE),
        error = function(e) {
          showNotification("Failed to read the CSV file.", type = "error")
          return(NULL)
        }
      )
      req(!is.null(data))
      
      # Render the data table
      datatable(data, options = list(scrollX = TRUE, searching = TRUE, pageLength = 50))
    })  
  }
}

# Run the application using nextGenShinyApps
shinyApp(ui = ui, server = server)
