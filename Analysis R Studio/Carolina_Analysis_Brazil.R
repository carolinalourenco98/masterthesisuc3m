### BRAZIL ###

# Load the dplyr package
library(dplyr)


# Create an empty data frame to store results
combined_results_PT <- data.frame()

# Specify the data frame names
data_frames <- c("b1", "b2","b3")  # Replace with your actual data frame names

# Loop through each data frame
for (df_name in data_frames) {
  df <- get(df_name)  # Get the data frame by its name
  
  # Filter rows containing the stimuli 
  filtered_df <- filter(df, stimulus %in% c("PT_Shortterm_Watch.png", 
                                            "PT_Shortterm_Headphones.png",
                                            "PT_Shortterm_Toothbrusch.png",
                                            "PT_Longterm_Headphones.png",
                                            "PT_Longterm_Watch.png", 
                                            "PT_Longterm_Toothbrusch.png",
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
  
  transformed_df <- combined_df %>%
    mutate(stimulus = case_when(
      stimulus %in% c("PT_Shortterm_Watch.png", "PT_Shortterm_Headphones.png", "PT_Shortterm_Toothbrusch.png") ~ "short",
      stimulus %in% c("PT_Longterm_Watch.png", "PT_Longterm_Headphones.png", "PT_Longterm_Toothbrusch.png") ~ "long",
      stimulus %in% c("Control_Watch.png", "Control_Headphones.png", "Control_Toothbrush.png") ~ "con",
      TRUE ~ stimulus  # leave as is if none of the above conditions are met
    ))
  
  
  # Group by stimulus and summarize the results
  results_PT <- transformed_df %>%
    group_by(stimulus) %>%
    summarize(Sum_Value = sum(response))
  
  # add new column indicating nationality 
  results_PT <- results_PT %>%
    mutate(nationality = c('Brazil', "Brazil", "Brazil"))
  
  # Append the results to the combined data frame
  combined_results_PT <- bind_rows(combined_results_PT, results_PT)
}

# Print the combined results
print(combined_results_PT)




