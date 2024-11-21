# Load necessary libraries
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)


# Load and prepare the dataset
Terror_Attacks <- read.csv(normalizePath("terrorist-attacks.csv"))

# Define European countries
european_countries <- c(
  "Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", 
  "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia", "Finland", 
  "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
  "Liechtenstein", "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
  "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal", 
  "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
  "Switzerland", "Ukraine", "United Kingdom", "Vatican"
)

# Filter data for European countries
Terror_EU <- subset(Terror_Attacks, Entity %in% european_countries)

# Remove age-related columns due to insufficient data
Terror_EU <- Terror_EU %>% select(-starts_with("Death.Age."))

# Simplify attack method labels
simplify_labels <- function(labels) {
  gsub("Attack.method\\.\\.", "", labels)
}

# ----------------- Visualization 1: Terrorist Attacks Over Time -----------------
ui1 <- fluidPage(
  titlePanel("Number of Terrorist Attacks in European Countries Over the Years"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Select Year:",
                  min = min(Terror_EU$Year),
                  max = max(Terror_EU$Year),
                  value = min(Terror_EU$Year),
                  step = 1)
    ),
    mainPanel(plotlyOutput("terroristPlot"))
  )
)

server1 <- function(input, output) {
  filteredData <- reactive({
    subset(Terror_EU, Year == input$yearRange & Terrorist.attacks > 0)
  })
  
  output$terroristPlot <- renderPlotly({
    fig <- plot_ly(
      filteredData(),
      x = ~Entity,
      y = ~Terrorist.attacks,
      type = "bar",
      text = ~paste("Number of Attacks: ", Terrorist.attacks),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "European Countries"),
        yaxis = list(title = "Number of Terrorist Attacks"),
        showlegend = FALSE
      )
    fig
  })
}

shinyApp(ui1, server1)

# ----------------- Visualization 2: Total Attacks Per Country > 200 -----------------
sums_by_country <- aggregate(Terror_EU$Terrorist.attacks, by = list(Entity = Terror_EU$Entity), sum)
selected_countries <- sums_by_country[sums_by_country$x > 200, ]

ggplot(selected_countries, aes(x = Entity, y = x, fill = Entity)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Terrorist Attacks in European Countries",
    x = "European Countries", 
    y = "Total Terrorist Attacks (>200)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# ----------------- Visualization 3: Pie Chart of Attack Methods -----------------
create_pie_chart <- function(data) {
  data$variable <- simplify_labels(data$variable)
  
  ggplot(data, aes(x = "", y = value, fill = variable)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = "right") +
    labs(title = 'Countries with <100 Terrorist Attacks') +
    scale_fill_manual(
      name = "Attack Method",
      values = c(
        "Hijacking" = "red",
        "Hostage.Taking..Barricade.Incident." = "orange",
        "Unarmed.Assault" = "yellow",
        "Facility.Infrastructure.Attack" = "green",
        "Hostage.Taking..Kidnapping." = "blue",
        "Assassination" = "purple",
        "Armed.Assault" = "brown",
        "Bombing.Explosion" = "gray"
      )
    )
}

ui2 <- fluidPage(
  titlePanel("Terrorist Attacks by Method Over Time"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Select Country:", 
                  choices = sums_by_country$Entity[sums_by_country$x > 100])
    ),
    mainPanel(plotOutput("pieChart"))
  )
)

server2 <- function(input, output) {
  output$pieChart <- renderPlot({
    selected_data <- Terror_EU[Terror_EU$Entity == input$country, ]
    selected_data <- selected_data[, c(
      "Attack.method..Hijacking", 
      "Attack.method..Hostage.Taking..Barricade.Incident.", 
      "Attack.method..Unarmed.Assault", 
      "Attack.method..Facility.Infrastructure.Attack",
      "Attack.method..Hostage.Taking..Kidnapping.", 
      "Attack.method..Assassination",
      "Attack.method..Armed.Assault", 
      "Attack.method..Bombing.Explosion"
    )]
    selected_data_long <- tidyr::gather(selected_data, key = "variable", value = "value")
    pie_chart <- create_pie_chart(selected_data_long)
    print(pie_chart)
  })
}

shinyApp(ui2, server2)

# ----------------- Additional Visualizations -----------------
# Visualization 4: Total Attacks by Method
sums_by_attack_method <- colSums(Terror_EU[, grep("Attack.method", colnames(Terror_EU))], na.rm = TRUE)
sums_data <- data.frame(
  AttackMethod = simplify_labels(names(sums_by_attack_method)), 
  Count = as.numeric(sums_by_attack_method)
)

ggplot(sums_data, aes(x = AttackMethod, y = Count, fill = AttackMethod)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Total Terrorist Attacks by Method",
    x = "Attack Method", 
    y = "Total Number of Attacks"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank())


# Visualization 5: Pie Chart of Terrorist Death Types
create_pie_chart2 <- function(data) {
  total_deaths <- sum(data$Terrorism.deaths)
  percentages <- c(
    Suicide = sum(data$Terrorist.Death.Type...Suicide) / total_deaths * 100,
    Killed = sum(data$Terrorist.Death.Type...Killed) / total_deaths * 100
  )
  
  ggplot() +
    geom_bar(
      aes(x = "", y = percentages, fill = names(percentages)), 
      stat = "identity", 
      width = 1
    ) +
    geom_text(
      aes(
        x = 1.5, 
        y = cumsum(percentages) - percentages / 2, 
        label = paste(names(percentages), sprintf("%.1f%%", percentages))
      ), 
      color = "black", 
      size = 6
    ) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = "right") +
    labs(
      title = 'Percentage of Deaths: Suicide vs Killed by Others'
    ) +
    scale_fill_manual(
      name = "Victim Type",
      values = c("Suicide" = "blue", "Killed" = "lightblue")
    )
}

# Visualization 6: Bar Chart for Suicide vs Killed
data <- data.frame(
  Type = c("Suicide", "Killed"),
  Deaths = c(sum(Terror_EU$Terrorist.Death.Type...Suicide), sum(Terror_EU$Terrorist.Death.Type...Killed))
)

ggplot(data, aes(x = Type, y = Deaths, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Deaths), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(
    title = "Fatalities: Suicide vs Killed by Others",
    x = "Type", 
    y = "Number of Deaths"
  ) +
  scale_fill_manual(
    name = "Type of Death",
    values = c("Suicide" = "blue", "Killed" = "lightblue")
  ) +
  theme_minimal()

# Visualization 7: Correlation Between Attacks and Deaths
ggplot(Terror_EU, aes(x = Terrorist.attacks, y = Terrorism.deaths)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Relationship Between Terrorist Attacks and Fatalities",
    x = "Number of Terrorist Attacks",
    y = "Number of Fatalities"
  ) +
  theme_minimal()

