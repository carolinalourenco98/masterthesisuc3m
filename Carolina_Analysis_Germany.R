### GERMANY ###


# Load the dplyr package
library(dplyr) 

# Create an empty data frame to store results
combined_results_DE <- data.frame()

# Specify the data frame names
data_frames <- c("g1","g2","g3") # Replace with your actual data frame names

# unique(combined_df$stimulus)

# Loop through each data frame
for (df_name in data_frames) {
  df <- get(df_name)  # Get the data frame by its name
  
  # Filter rows containing the stimuli 
  filtered_df <- filter(df, stimulus %in% c("DE_Shortterm_watch.png", 
                                            "DE_Shortterm_Headphones.png",
                                            "DE_Shortterm_Toothbrush.png",
                                            "DE_Longterm_Headphones.png",
                                            "DE_Longterm_Watch.png", 
                                            "DE_LongTerm_Toothbrush.png",
                                            "Control_Headphones.png",
                                            "Control_Watch.png",
                                            "Control_Toothbrush.png"))
  
  # Filter rows containing the rating scale
  filtered_df1 <- filter(df, trial_type %in% c("survey-likert"))
  
  # Make a new df with the relevant variables
  stim <- filtered_df %>% select(stimulus)
  ratings <- filtered_df1 %>% select(response)
  combined_df <- bind_cols(stim, ratings)
  
  # Count the values together in the JSON string
  library(jsonlite)
  combined_df$response <- sapply(combined_df$response, function(x) sum(unlist(fromJSON(x))))
  
  # Transform values in the stimulus column
  transformed_df <- combined_df %>%
    mutate(stimulus = case_when(
      stimulus %in% c("DE_Shortterm_watch.png", "DE_Shortterm_Headphones.png", "DE_Shortterm_Toothbrush.png") ~ "short",
      stimulus %in% c("DE_Longterm_Watch.png", "DE_Longterm_Headphones.png", "DE_LongTerm_Toothbrush.png") ~ "long",
      stimulus %in% c("Control_Watch.png", "Control_Headphones.png", "Control_Toothbrush.png") ~ "con",
      TRUE ~ stimulus  # leave as is if none of the above conditions are met
    ))
  
  # Group by stimulus and summarize the results
  results_DE <- transformed_df %>%
    group_by(stimulus) %>%
    summarize(Sum_Value = sum(response))
  
  # add new column indicating nationality 
  results_DE <- results_DE %>%
    mutate(nationality = c('German', "German", "German"))
  
  # Append the results to the combined data frame
  combined_results_DE <- bind_rows(combined_results_DE, results_DE)
}


