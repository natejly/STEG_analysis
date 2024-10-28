# Load necessary libraries
if (!require(ggplot2)) install.packages("ggplot2", dependencies=TRUE)
if (!require(tidyr)) install.packages("tidyr", dependencies=TRUE)
if (!require(stringr)) install.packages("stringr", dependencies=TRUE)

library(ggplot2)
library(tidyr)
library(stringr)

# Define the analyze_survey function
analyze_survey <- function(csv_file) {
  # Read in the data
  data <- read.csv(csv_file)
  
  # Extract the base name of the CSV file to use as folder name
  base_name <- tools::file_path_sans_ext(basename(csv_file))
  # Replace spaces and special characters with underscores to make a valid folder name
  folder_name <- gsub("[^A-Za-z0-9_]", "_", base_name)
  
  # Create a directory to save images and results, if it doesn't exist
  if(!dir.exists(folder_name)){
    dir.create(folder_name)
  }
  
  # Clean up the column names
  colnames(data) <- gsub(pattern = ".*(Pre|Post).*", replacement = "PrePost", x = colnames(data))
  colnames(data) <- gsub(pattern = ".*(Name).*", replacement = "Name", x = colnames(data))
  colnames(data) <- gsub(pattern = "To.me..", replacement = "", x = colnames(data))
  colnames(data) <- gsub(pattern = ".is....", replacement = "_", x = colnames(data))
  colnames(data) <- gsub(pattern = "a.CAREER.in.science..technology..and.engineering",
                         replacement = "STE", x = colnames(data))
  colnames(data) <- gsub(pattern = "a.CAREER.in.geography", replacement = "GEO",
                         x = colnames(data))
  
  # Separate the data into pre and post
  pre_data <- data[data$PrePost == "PRE", ]
  post_data <- data[data$PrePost == "POST", ]
  
  # Remove the first column
  pre_data <- pre_data[, -1]
  post_data <- post_data[, -1]
  
  # Append _PRE and _POST to column names
  colnames(pre_data)[-1] <- paste0(colnames(pre_data)[-1], "_PRE")
  colnames(post_data)[-1] <- paste0(colnames(post_data)[-1], "_POST")
  
  # Get the mean for each column and collect it in a data frame
  mean_data <- data.frame(
    Question = colnames(pre_data),
    Mean_PRE = sapply(pre_data, function(x) mean(as.numeric(x), na.rm = TRUE))
  )
  mean_data$Mean_POST <- sapply(post_data, function(x) mean(as.numeric(x), na.rm = TRUE))
  
  # Remove the 'Name' row
  mean_data <- mean_data[-1, ]
  
  # Save the mean data used for graphs to a text file
  write.table(mean_data, file = file.path(folder_name, "mean_data.txt"), row.names = FALSE, sep = "\t")
  
  # Define a function to plot and save figures and datasets
  plot_figures <- function(data, grep_string, filename, title = NULL) {
    # Filter data based on the grep_string
    subset <- data[grepl(grep_string, data$Question), ]
    colnames(subset) <- c("Question", "Pre-Survey", "Post-Survey")
    
    # Save the subset data used for this graph
    subset_filename <- paste0(tools::file_path_sans_ext(filename), "_data.txt")
    write.table(subset, file = file.path(folder_name, subset_filename), row.names = FALSE, sep = "\t")
    
    # Convert data to long format
    subset_long <- pivot_longer(subset, 
                                cols = c("Pre-Survey", "Post-Survey"), 
                                names_to = "Type", 
                                values_to = "Mean")
    
    # Create a mapping for the original questions to the new labels
    label_mapping <- c("Fascinating", "Exciting", "Interesting", "Important")
    names(label_mapping) <- unique(subset_long$Question)
    
    # Ensure the bars appear in the order of label_mapping
    subset_long$Question <- factor(subset_long$Question, levels = names(label_mapping))
    
    # Ensure Pre-Survey appears to the left of Post-Survey
    subset_long$Type <- factor(subset_long$Type, levels = c("Pre-Survey", "Post-Survey"))
    
    # Create a title
    if (is.null(title)){
      title <- paste("To Me", str_to_title(grep_string), "is:")
    }
    p <- ggplot(subset_long, aes(x = Question, y = Mean, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = title, x = "", y = "Average score") +
      scale_fill_manual(values = c("Pre-Survey" = "#2050f0", "Post-Survey" = "#fbaf17")) +
      scale_x_discrete(labels = label_mapping) +
      theme(
        plot.title = element_text(hjust = 0.5, size = 18),  # Center the title
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        legend.title = element_blank(),  # Remove legend title
        plot.margin = margin(20, 0, 20, 10),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_text(size = 12),  # Increase x-axis label size
        axis.title.y = element_text(size = 12)
      ) + 
      geom_text(aes(label = round(Mean, 3)),  # Add text labels with rounded mean values
                position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5)
    
    # Save the plot in the specified folder
    ggsave(filename = file.path(folder_name, filename), plot = p)
  }
  
  # Generate and save plots and datasets
  plot_figures(mean_data, "SCIENCE", filename = "science_plot.png")
  plot_figures(mean_data, "TECHNOLOGY", filename = "technology_plot.png")
  plot_figures(mean_data, "ENGINEERING", filename = "engineering_plot.png")
  plot_figures(mean_data, "GEOGRAPHY", filename = "geography_plot.png")
  plot_figures(mean_data, "STE", filename = "ste_plot.png", title = "To Me a Career in STE is:")
  plot_figures(mean_data, "GEO_", filename = "geo_plot.png", title = "To Me a Career in Geography is:")
  
  # Create a new dataframe for mean scores for each subject
  subject_data <- data.frame(
    Subject = c("Science", "Technology", "Engineering", "Geography", "Career in STE", "Career in GEO"),
    `Pre-Survey` = numeric(6),
    `Post-Survey` = numeric(6)
  )
  
  # SCIENCE
  subject_data[subject_data$Subject == "Science", "Pre-Survey"] <- mean(mean_data[grepl("SCIENCE", mean_data$Question), "Mean_PRE"])
  subject_data[subject_data$Subject == "Science", "Post-Survey"] <- mean(mean_data[grepl("SCIENCE", mean_data$Question), "Mean_POST"])
  
  # TECHNOLOGY
  subject_data[subject_data$Subject == "Technology", "Pre-Survey"] <- mean(mean_data[grepl("TECHNOLOGY", mean_data$Question), "Mean_PRE"])
  subject_data[subject_data$Subject == "Technology", "Post-Survey"] <- mean(mean_data[grepl("TECHNOLOGY", mean_data$Question), "Mean_POST"])
  
  # ENGINEERING
  subject_data[subject_data$Subject == "Engineering", "Pre-Survey"] <- mean(mean_data[grepl("ENGINEERING", mean_data$Question), "Mean_PRE"])
  subject_data[subject_data$Subject == "Engineering", "Post-Survey"] <- mean(mean_data[grepl("ENGINEERING", mean_data$Question), "Mean_POST"])
  
  # GEOGRAPHY
  subject_data[subject_data$Subject == "Geography", "Pre-Survey"] <- mean(mean_data[grepl("GEOGRAPHY", mean_data$Question), "Mean_PRE"])
  subject_data[subject_data$Subject == "Geography", "Post-Survey"] <- mean(mean_data[grepl("GEOGRAPHY", mean_data$Question), "Mean_POST"])
  
  # STE
  subject_data[subject_data$Subject == "Career in STE", "Pre-Survey"] <- mean(mean_data[grepl("STE", mean_data$Question), "Mean_PRE"])
  subject_data[subject_data$Subject == "Career in STE", "Post-Survey"] <- mean(mean_data[grepl("STE", mean_data$Question), "Mean_POST"])
  
  # GEO
  subject_data[subject_data$Subject == "Career in GEO", "Pre-Survey"] <- mean(mean_data[grepl("GEO_", mean_data$Question), "Mean_PRE"])
  subject_data[subject_data$Subject == "Career in GEO", "Post-Survey"] <- mean(mean_data[grepl("GEO_", mean_data$Question), "Mean_POST"])
  
  # Save the subject data used for the graph
  write.table(subject_data, file = file.path(folder_name, "subject_mean_scores_data.txt"), row.names = FALSE, sep = "\t")
  
  # Reshape data into long format
  subject_data_long <- pivot_longer(subject_data, cols = c("Pre-Survey", "Post-Survey"),
                                    names_to = "Survey_Type", values_to = "Mean_Score")
  
  # Ensure the order of subjects on the x-axis
  subject_data_long$Subject <- factor(subject_data_long$Subject, levels = subject_data$Subject)
  
  # Create the double bar graph
  p_subject <- ggplot(subject_data_long, aes(x = Subject, y = Mean_Score, fill = Survey_Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Pre-Survey vs Post-Survey Mean Scores by Subject",
         x = "Subject",
         y = "Average Score") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),  # Center the title
      panel.grid.major = element_blank(),  # Remove major grid lines
      panel.grid.minor = element_blank(),  # Remove minor grid lines
      legend.title = element_blank(),  # Remove legend title
      plot.margin = margin(20, 0, 20, 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(size = 12),  # Increase x-axis label size
      axis.title.y = element_text(size = 12)
    )
  
  # Save the subject plot in the folder
  ggsave(filename = file.path(folder_name, "subject_mean_scores.png"), plot = p_subject)
  
  # Now perform T-tests and save the results
  
  # List of subjects
  subjects <- c("SCIENCE", "TECHNOLOGY", "ENGINEERING", "GEOGRAPHY", "STE", "GEO_")
  
  # Initialize a data frame to store p-values
  t_test_results <- data.frame(Subject = character(), P_Value = numeric(), stringsAsFactors = FALSE)
  
  for (subject in subjects) {
    cat("\nPerforming t-test for subject:", subject, "\n")
    
    # Extract the pre-test and post-test columns matching the subject
    pre_cols <- grep(subject, colnames(pre_data), value = TRUE)
    post_cols <- grep(subject, colnames(post_data), value = TRUE)
    
    # Ensure the same number of columns are found for both pre and post data
    if (length(pre_cols) != length(post_cols)) {
      warning("Mismatch in the number of columns found for pre and post data for subject: ", subject)
      p_value <- NA
    } else {
      # Combine pre_data and post_data on 'Name'
      combined_data <- merge(pre_data[, c("Name", pre_cols)], post_data[, c("Name", post_cols)], by = "Name")
      
      # Identify rows where all pre and post data for the subject are complete (non-NA and non-empty)
      valid_rows <- apply(combined_data[, -1], 1, function(row) all(!is.na(row) & row != ""))
      complete_data <- combined_data[valid_rows, ]
      
      # Check if there are enough data points to perform t-test
      if (nrow(complete_data) < 2) {
        warning("Not enough complete data to perform t-test for subject: ", subject)
        p_value <- NA
      } else {
        # Print the students that have complete data for this subject
        cat("Students with complete data for", subject, ":\n")
        print(complete_data$Name)
        
        # Save the complete data used for the t-test
        t_test_data_filename <- paste0("t_test_data_", subject, ".txt")
        write.table(complete_data, file = file.path(folder_name, t_test_data_filename), row.names = FALSE, sep = "\t")
        
        # Convert pre and post data to numeric vectors
        pre <- as.numeric(unlist(complete_data[, pre_cols]))
        post <- as.numeric(unlist(complete_data[, post_cols]))
        
        # Perform the paired t-test
        t_result <- t.test(pre, post, paired = TRUE)
        p_value <- t_result$p.value
      }
    }
    # Append the result
    t_test_results <- rbind(t_test_results, data.frame(Subject = subject, P_Value = p_value))
  }
  
  # Rename STE to "Career in STE" and GEO_ to "Career in GEO"
  t_test_results$Subject[t_test_results$Subject == "STE"] <- "Career in STE"
  t_test_results$Subject[t_test_results$Subject == "GEO_"] <- "Career in GEO"
  
  # Save the t-test results to a text file in the folder
  write.table(t_test_results, file = file.path(folder_name, "t_test_results.txt"), row.names = FALSE, sep = "\t")
  
  # Optionally, print the t-test results
  cat("\nT-test Results:\n")
  print(t_test_results)
  
}

# Find all CSV files in the current directory
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

# Loop over each CSV file and run analyze_survey
for (csv_file in csv_files) {
  cat("\nProcessing file:", csv_file, "\n")
  analyze_survey(csv_file)
}
