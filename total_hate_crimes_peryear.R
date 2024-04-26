# Calculating total hate crimes per year
hate_crimes_per_year <- aggregate(offense ~ years, data = cleaned_crime_data, FUN = length)

# Creating an interactive plot
plot <- plot_ly(hate_crimes_per_year, x = ~years, y = ~offense, type = 'scatter', mode = 'lines+markers',
                text = ~paste("Year:", years, "<br>Total Hate Crimes:", offense),
                hoverinfo = 'text', marker = list(color = 'rgb(16, 32, 77)'))

plot <- plot %>% layout(title = "Total Hate Crimes Over the Years",
                        xaxis = list(title = "Year"),
                        yaxis = list(title = "Total Hate Crimes"))

# Showing plot
plot

# Saving the interactive plot to an HTML file 
saveWidget(plot, "total_hate_crimes_over_years_interactiveplot.html", title = "Total Hate Crimes Over the Years", selfcontained = TRUE)