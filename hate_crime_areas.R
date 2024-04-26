# Creating a contingency table
contingency_table <- table(cleaned_crime_data$offense, cleaned_crime_data$district_number, cleaned_crime_data$community)

# Converting the contingency table to a data frame
contingency_df <- as.data.frame.table(contingency_table)

# Renaming the columns
names(contingency_df) <- c("Offense", "District_Number", "Community", "Count")


View(contingency_df)


# Converting District_Number to numeric
contingency_df$District_Number <- as.numeric(contingency_df$District_Number)
# Arranging the District_Number column from smallest to largest
contingency_df <- contingency_df %>%
  arrange(District_Number)



#creating an interactive plot
plot <- plot_ly(contingency_df, x = ~District_Number, y = ~Community, z = ~Count, type = "heatmap",
                colorscale = "Viridis", text = ~paste("Offense:", Offense, "<br>District Number:", District_Number, "<br>Community:", Community, "<br>Count:", Count),
                hoverinfo = "text")

plot <- plot %>% layout(title = "Offenses Occurring in Different Areas",
                        xaxis = list(title = "District Number"),
                        yaxis = list(title = "Community"))

# Show plot
plot

saveWidget(plot, "hate_crime_areas.html", title = "Offenses vs Biases Interactive Bar Plot", selfcontained = TRUE)


# Finding the row with the highest count of offenses
highest_count_row <- contingency_df[which.max(contingency_df$Count), ]

# Extracting the name of the community with the highest count and the count itself
highest_community <- as.character(highest_count_row$Community)
highest_count <- highest_count_row$Count

cat("Community with the highest count of offenses:", highest_community, "\n")
cat("Number of offenses in this community:", highest_count, "\n")
