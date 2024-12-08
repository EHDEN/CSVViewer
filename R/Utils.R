
library(RJSONIO)

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
    data <- jsonlite::fromJSON(json_file_path, simplifyVector = FALSE)
    
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
