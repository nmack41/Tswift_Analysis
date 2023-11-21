# Load required libraries
library(dplyr)

# Load the taylor_all_songs dataset
load(file='C:/Projects/Tswift/taylor/data/taylor_all_songs.rda')

# Define the directory where the lyrics CSV files are stored
lyrics_dir <- "C:/Users/nmack/OneDrive/Documents/data/songs"

# Check if the directory exists; if not, create it
if (!dir.exists(lyrics_dir)) {
  dir.create(lyrics_dir, recursive = TRUE)
}

# Define the output directory where CSV files will be saved
output_dir <- "C:/Projects/Tswift/taylor/data/songs"

# Loop through each row in taylor_all_songs
for (i in seq_len(nrow(taylor_all_songs))) {
  # Extract the dataframe from the lyrics column
  current_lyrics_df <- taylor_all_songs$lyrics[[i]]
  
  # Make sure that it's actually a dataframe
  if (is.data.frame(current_lyrics_df)) {
    # Extract the track name for the current song
    track_name <- as.character(taylor_all_songs$track_name[i])
    
    # Create a valid filename from the track name
    # Remove special characters and spaces, convert to lower case
    filename <- paste0(gsub("[^[:alnum:] ]", "", track_name), ".csv")
    filename <- tolower(gsub(" ", "_", filename))
    
    # Create the full file path
    file_path <- file.path(output_dir, filename)
    
    # Write the dataframe to a CSV file in the specified folder
    write.csv(current_lyrics_df, file = file_path, row.names = FALSE)
  } else {
    warning(paste("The content of lyrics at index", i, "is not a dataframe."))
  }
}

# Sanitize filename function
sanitize_filename <- function(name) {
  name <- tolower(name)
  name <- gsub("'", "", name) # Remove apostrophes
  name <- gsub("&", "__", name) # Replace & with double underscore
  name <- gsub("[[:space:]]+", "_", name) # Replace one or more spaces with a single underscore
  name <- gsub("[^[:alnum:]_]", "", name) # Remove any remaining non-alphanumeric characters, except underscore
  name <- gsub("__+", "__", name) # Replace multiple underscores with double underscores if they occur
  name <- paste0(name, ".csv") # Append .csv extension
  return(name)
}

# Ensure the lyrics column exists in the dataframe and is initialized to NA
taylor_all_songs$lyrics <- vector("list", nrow(taylor_all_songs))

# Loop through each row in taylor_all_songs to match and read the lyrics CSV files
for (i in seq_len(nrow(taylor_all_songs))) {
  track_name <- as.character(taylor_all_songs$track_name[i])
  filename <- sanitize_filename(track_name)
  
  # Construct the full file path
  lyrics_file_path <- file.path(lyrics_dir, filename)
  
  # Check if the lyrics file exists and read it
  if (file.exists(lyrics_file_path)) {
    lyrics_df <- read.csv(lyrics_file_path, stringsAsFactors = FALSE)
    
    # Check if the dataframe is not empty and has the 'lyric' column
    if (!is.null(lyrics_df) && "lyric" %in% names(lyrics_df)) {
      # Extract lyrics from the 'lyric' column and collapse into a single string
      lyrics_string <- paste(lyrics_df$lyric, collapse = "\n")
      taylor_all_songs$lyrics[i] <- list(lyrics_string)
    } else {
      warning(paste("Lyrics file for", track_name, "does not have a 'lyric' column at", lyrics_file_path))
    }
  } else {
    warning(paste("Lyrics file for", track_name, "not found at", lyrics_file_path))
  }
}

# Convert the list column to a character vector for CSV output
taylor_all_songs$lyrics <- sapply(taylor_all_songs$lyrics, function(x) if (is.null(x)) NA else paste(x, collapse = "\n"))

# Save the updated dataframe to a new CSV file
write.csv(taylor_all_songs, "C:/Projects/Tswift/taylor/data/taylor_all_songs_updated.csv", row.names = FALSE)