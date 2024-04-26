# Creating a contingency table
contingency_table <- table(cleaned_crime_data$offense, cleaned_crime_data$motivation_bias)

# Converting the contingency table to a data frame
contingency_df <- as.data.frame.matrix(contingency_table)

# Adding columns for offenses and biases
contingency_df$Offense <- rownames(contingency_df)
contingency_df <- contingency_df %>%
  gather(Bias, Count, -Offense)

# Converting 'Count' to numeric
contingency_df$Count <- as.numeric(contingency_df$Count)
# Defining custom colors for biases
bias_colors <- c("Anti-arab" = "#54B435",   # Green
                 "Anti-asian" = "#00FFFF",  # Cyan
                 "Anti-black" = "#000000",  # Black
                 "Anti-hispanic" = "#3C0753",  # Purple
                 "Anti-immigrant" = "#954535",  # ChestnutBrown
                 "Anti-jewish" = "#9ED2BE", 
                 "Anti-lgbtqia" = "#D20062",  # Pink
                 "Anti-white" = "#E9FFC2",  
                 "Religious Supremacist" = "#F28500",  #Tangerine Orange
                 "Xenophobic" = "#747264")  # Grey

# Creating interactive bar plot with custom colors
plot <- plot_ly(contingency_df, x = ~Offense, y = ~Count, color = ~Bias, colors = bias_colors,
                text = ~paste("Offense:", Offense, "<br>Bias:", Bias, "<br>Count:", Count),
                hoverinfo = "text", type = "bar", width = 1.8)  

# Adding layout
plot <- plot %>% layout(title = "Offenses vs Biases Interactive Bar Plot",
                        xaxis = list(title = "Offense"),
                        yaxis = list(title = "Count"))

# Showing plot
plot

# Saving the interactive plot to an HTML file 
saveWidget(plot, "offenses_vs_biases_interactive_plot.html", title = "Offenses vs Biases Interactive Bar Plot", selfcontained = TRUE)
