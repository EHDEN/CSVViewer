library(nextGenShinyApps)
library(DT)
library(RJSONIO)
library(rlist)
library(knitr)

folder_path_default <- "/Users/rijnbeek/Documents/Github/ehden/CSVViewer"
documentation_json <- paste0(folder_path_default,"/Eunomia/documentation.json")
documentation_rmd <- paste0(folder_path_default,"/Eunomia/test-doc.Rmd")

# Define UI using nextGenShinyApps framework
ui <- fluidPage(
  theme = 2,
  header = titlePanel(left = "CSV Viewer App", right = "Image logo"),
  
  sidebarPanel(
    title = "CSV Viewer",
    textInput("folder", "Folder Path:", value = folder_path_default,),
    checkboxInput("show_only_data", "Skip Empty Files", value = TRUE)  # Add checkbox
  ), 
  mainPanel(
    width = 12,
    div(
      style = "margin-bottom: 20px; text-align: left;",
      selectInput("csv_file", "Select a CSV File:", choices = NULL, width = "50%") # Centered and styled input
    ),
    card(
      title = "Selected File",
      textOutput("selected_file_name")
    ),
    fluidRow(
      column(
        width = 6,
        card(
          title = "Documentation",
         # uiOutput('markdown',fill = TRUE)
         includeMarkdown(documentation_rmd)
         
        )
      ),
      column(
        width = 6,
        card(
          title = "Notes",
          width = 12,
          toolbar = list(collapse = TRUE,
                         maximize = FALSE, close = FALSE, menu = TRUE),
          textAreaInput(
            inputId = "comments",
            label = "Enter your comments:",
            value = "",
            width = "100%",
            height = "100%"
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
      showNotification("The specified folder does not exist.", type = "error")
      return()
    }
    
    # List all CSV files in the folder and subfolders
    all_csv_files <- list.files(
      path = folder_path, 
      pattern = "\\.csv$", 
      recursive = TRUE, 
      full.names = TRUE
    )
    
        # Filter files by depth (max depth = 5)
    all_csv_files <- Filter(function(file) {
      file_depth <- length(strsplit(normalizePath(file), .Platform$file.sep)[[1]])
      folder_depth <- length(strsplit(normalizePath(folder_path), .Platform$file.sep)[[1]])
      (file_depth - folder_depth) <= 5
    }, all_csv_files)
    
    if (length(all_csv_files) == 0) {
      showNotification("No CSV files found in the specified folder.", type = "warning")
    }
    
    # Determine which files have more than just a header
    valid_files <- lapply(all_csv_files, function(file) {
      # Attempt to read the file
      data <- tryCatch(read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
      
      # Check if the file has valid data (at least one non-header row)
      has_data <- !is.null(data) && ncol(data) > 0 && nrow(data) > 0
      
      # Return the file path and its status
      list(
        file = file,
        has_data = has_data
      )
    })
    
    # Filter files based on the checkbox
    if (input$show_only_data) {
      filtered_files <- Filter(function(x) x$has_data, valid_files)
    } else {
      filtered_files <- valid_files
    }
    
    # Extract file paths and labels
    file_paths <- sapply(filtered_files, function(x) x$file)
    file_labels <- sapply(filtered_files, function(x) {
      formatted_name <-  basename(x$file)
      if (!x$has_data) paste0(formatted_name, " (header only)") else formatted_name
    })
    
    # Update the reactive value
    csv_files(file_paths)
    
    # Update the selectInput with file names only
    updateSelectInput(
      session, 
      "csv_file", 
      choices = setNames(file_paths, file_labels), # Use filtered file paths and labels
      selected = if (length(file_paths) > 0) file_paths[1] else NULL
    )
  })
  
  # Display the name of the selected CSV file
  output$selected_file_name <- renderText({
    req(input$csv_file)
    selected_file <- input$csv_file
    # Get two levels of parent folders and the file name
    file_path_parts <- strsplit(normalizePath(selected_file), .Platform$file.sep)[[1]]
    path_length <- length(file_path_parts)
    if (path_length >= 3) {
      paste(paste(file_path_parts[(path_length - 2):path_length], collapse = "/"))
    } else {
      paste(basename(selected_file))
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
  
  observeEvent(input$save, {
    # Retrieve the comments
    comments <- input$comments
    
    # Define the file path to save the JSON
    json_file <- "comments.json"
    
    new_entry <- list(
        name = input$csv_file,
        comments = comments,
        updated = format(Sys.time(), "%A, %B %d, %Y %I:%M:%S %p")
    )
    # Write comments to the JSON file
    update_json_file(json_file, "files", new_entry, "name")
    
    # Notify the user
    showModal(modalDialog(
      title = "Success",
      "Your comments have been saved successfully!",
      easyClose = TRUE
    ))
  })
  
  output$markdown <- renderUI({
    HTML(markdown::markdownToHTML(knit(documentation_rmd, quiet = TRUE)))
  })
}

# Run the application using nextGenShinyApps
shinyApp(ui = ui, server = server)
