
get_file_paths <- function(folder_path, show_only_data=TRUE, max_depth=5){

  # List all CSV files in the folder and subfolders
  all_csv_files <- list.files(
    path = folder_path, 
    pattern = "\\.csv$", 
    recursive = TRUE, 
    full.names = TRUE
  )
  
  # Filter files by depth (max depth = 5 by default)
  all_csv_files <- Filter(function(file) {
    file_depth <- length(strsplit(normalizePath(file), .Platform$file.sep)[[1]])
    folder_depth <- length(strsplit(normalizePath(folder_path), .Platform$file.sep)[[1]])
    (file_depth - folder_depth) <= max_depth
  }, all_csv_files)
  
  if (length(all_csv_files) == 0) {
    return(NULL)
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
  
  # Filter files based on the check box
  if (show_only_data) {
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
  
  result<- list(file_paths=file_paths,file_labels=file_labels)
  return(result)
}




update_json_file <- function(file_path, section, new_item, unique_key) {
  tryCatch({
    # Check if the file exists
    if (file.exists(file_path)) {
      # Read the existing JSON file
      data <- fromJSON(file_path, simplifyVector = FALSE)
    } else {
      # Initialize an empty list if the file doesn't exist
      data <- list()
    }
    
    # Ensure the section exists and is a list
    if (!section %in% names(data)) {
      data[[section]] <- list()
    } else if (!is.list(data[[section]])) {
      stop(sprintf("The section '%s' is not a list.", section))
    }
    
    # Check for an existing item with the same unique_key
    existing_index <- NULL
    for (i in seq_along(data[[section]])) {
      if (!is.null(data[[section]][[i]][[unique_key]]) &&
          data[[section]][[i]][[unique_key]] == new_item[[unique_key]]) {
        existing_index <- i
        break
      }
    }
    
    # Update if the item exists, otherwise append
    if (!is.null(existing_index)) {
      data[[section]][[existing_index]] <- new_item
    } else {
      data[[section]] <- append(data[[section]], list(new_item))
    }
    
    # Write the updated data back to the file
    write(toJSON(data, pretty = TRUE, auto_unbox = TRUE), file_path)
    
    return(TRUE)
  }, error = function(e) {
    message(sprintf("Error: %s", e$message))
    return(FALSE)
  })
}

get_description_by_filename <- function(json_file_path, target_filename) {
  # Load the JSON data
  tryCatch({
    if (file.exists(json_file_path)) {
      # Read the existing JSON file
      data <- jsonlite::fromJSON(json_file_path, simplifyVector = FALSE)
    } else {
      return("No documention.json found in the selected folder.")
    }
    
    # Iterate through the files in the JSON
    for (file_info in data$files) {
      # Split the "name" field into a list of filenames
      file_names <- unlist(strsplit(file_info$name, ","))
      
      # Check if the target filename is in the list
      if (target_filename %in% file_names) {
        return(file_info$description)
      }
    }
    
    # Return NULL if no match is found
    return("No documentation available for this csv file")
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

get_markdown_by_filename <- function(resultsfolder, target_filename) {
  # Load the JSON data
  tryCatch({
    documentation_path <- paste0(resultsfolder, "/documentation")
    json_file_path <- paste0(documentation_path, "/documentation.json")
    if (file.exists(json_file_path)) {
      # Read the existing JSON file
      data <- jsonlite::fromJSON(json_file_path, simplifyVector = FALSE)
    } else {
      return("No documentation.json found in the documentation folder.")
    }
    
    description <- "No documentation available for this file."
    
    # Iterate through the folders in the JSON
    subfolder <- gsub(resultsfolder, "", dirname(target_filename))
    print(subfolder)
    
    for (folder_info in data$folders) {
      # Split the "name" field into a list of folders
      folder_names <- unlist(strsplit(folder_info$name, ","))
      print(folder_names)
      
      # Check if the target filename is in the list
      if (subfolder %in% folder_names) {
        # Read the Markdown file content from the path specified in markdown_file field
        md_file_path <- paste0(documentation_path,"/",folder_info$markdown_file)
        if (file.exists(md_file_path)) {
          md_content <- readLines(md_file_path)
          description <- paste(md_content, collapse = "\n")
        } else {
          description <- paste("Markdown file", md_file_path, "not found.")
        }
      }
    }
    
    # Iterate through the files in the JSON
    for (file_info in data$files) {
      # Split the "name" field into a list of filenames
      file_names <- unlist(strsplit(file_info$name, ","))
      
      # Check if the target filename is in the list
      if (basename(target_filename) %in% file_names) {
        # Read the Markdown file content from the path specified in markdown_file field
        md_file_path <- paste0(documentation_path,"/",file_info$markdown_file)
        if (file.exists(md_file_path)) {
          md_content <- readLines(md_file_path)
          description <- paste(md_content, collapse = "\n")
        } else {
          description <- paste("Markdown file", md_file_path, "not found.")
        }
      }
    }
    
    # Return NULL if no match is found
    return(description)
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}

get_comments_by_filename <- function(json_file_path, target_filename) {
  # Load the JSON data
  tryCatch({
    if (file.exists(json_file_path)) {
      # Read the existing JSON file
      data <- jsonlite::fromJSON(json_file_path, simplifyVector = FALSE)
    } else {
      return(NULL)
    }
    # Iterate through the files in the JSON
    for (file_info in data$files) {
      # Split the "name" field into a list of filenames
      file_names <- unlist(strsplit(file_info$name, ","))
      
      # Check if the target filename is in the list
      if (target_filename %in% file_names) {
        return(file_info$comments)
      }
    }
    print(json_file_path)
    print(target_filename)
    return("")
  }, error = function(e) {
    return(paste("Error:", e$message))
  })
}