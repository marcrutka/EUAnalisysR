# Terrorist Attacks Visualization in Europe

This repository contains a Shiny application and visualizations in R language analyzing terrorist attacks across European countries between 1970-2021. The data is processed to explore trends, attack methods, and fatalities using interactive and static visualizations.

---

## Features
1. **Interactive Bar Plot**  
   A Shiny app where users can explore the number of terrorist attacks in European countries for a specific year using a slider.

2. **Total Attacks by Country**  
   A static bar plot showing countries with more than 200 terrorist attacks.

3. **Attack Methods (Pie Chart)**  
   A Shiny app visualizing the distribution of attack methods in countries with significant terrorist activity.

4. **Total Attacks by Method**  
   A static bar plot summarizing the total number of terrorist attacks by method.

5. **Fatalities by Type (Pie Chart)**  
   A Shiny app comparing the percentage of fatalities caused by suicide attacks and other means.

6. **Fatalities by Type (Bar Plot)**  
   A static bar plot showing the number of fatalities caused by suicide attackers versus non-suicidal terrorists.

7. **Relationship Between Attacks and Fatalities**  
   A scatter plot with a regression line highlighting the correlation between the number of terrorist attacks and fatalities.

---

### Prerequisites
- **R (version 4.0 or newer)**
- **RStudio**
- Install the following R libraries if not already installed:
  ```R
  install.packages(c("shiny", "plotly", "ggplot2", "dplyr", "tidyr"))

