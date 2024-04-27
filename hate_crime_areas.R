# Creating a contingency table
contingency_table <- table(cleaned_crime_data$offense, cleaned_crime_data$community)

# Converting the contingency table to a data frame
contingency_df <- as.data.frame.table(contingency_table)

# Renaming the columns
names(contingency_df) <- c("Offense", "Community", "Count")

# Creating an interactive plot
plot <- plot_ly(contingency_df, x = ~Community, y = ~Offense, z = ~Count, type = "heatmap",
                colorscale = "Viridis", text = ~paste("Offense:", Offense, "<br>Community:", Community, "<br>Count:", Count),
                hoverinfo = "text")

plot <- plot %>% layout(title = "Offenses Occurring in Different Areas",
                        xaxis = list(title = "Community", tickangle = 45, categoryorder = "category ascending"),
                        yaxis = list(title = "Offense"))

# Show plot
plot

saveWidget(plot, "hate_crime_areas.html", title = "Offenses vs Biases Interactive Bar Plot", selfcontained = TRUE)

# Finding the rows with the top 5 highest counts of offenses
top_n_communities <- contingency_df[order(contingency_df$Count, decreasing = TRUE), ][1:5, ]

# Extracting the names of the communities with the highest counts and the counts themselves
top_community_counts <- paste(top_n_communities$Community, ": ", top_n_communities$Count, sep = "")

cat("Communities with the highest counts of offenses:\n")
cat(top_community_counts, sep = "\n")
