# Analysis Overview

## Objective:
The analysis aims to understand hate crimes in Chicago, focusing on the period from 2012 to 2023.

### Data Source:
The data was sourced from data.world, providing a comprehensive dataset on hate crimes in Chicago.
```
crime_data <- read_excel('Hate Crime in Chicago.xlsx')
View(crime_data)

## cleaned dataset
write.csv(cleaned_crime_data, "cleaned_crime_data.csv", row.names = FALSE)

```

### Key Objectives:

#### Investigate whether hate crimes in Chicago have been increasing or decreasing over the years.
#### Identify the biases associated with different types of hate crimes.
#### Identify the areas where hate crimes are most prevalent.

### Libraries Used:
```
library(ggplot2)
library(plotly)
library(dplyr)
library(htmlwidgets)
library(lubridate)
library(tidyverse)
```


