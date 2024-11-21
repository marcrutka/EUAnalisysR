Ataki_Terror <- read.csv(normalizePath("terrorist-attacks.csv"))


kraje_europejskie <- c("Albania", "Andorra", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", "Czechia",
                       "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", "Latvia", "Liechtenstein",
                       "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", "Portugal",
                       "Romania", "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Ukraine", "United Kingdom", "Vatican")

Terror_EU <- subset(Ataki_Terror, Entity %in% kraje_europejskie)

### Nie ujmowałem przedziałów wiekowych, ponieważ zawierają one za mało informacji, wynikających
# z braku zebrania danych wszystkich ofiar śmiertelnych w danych przedzialach. 

install.packages("shiny")
install.packages("plotly")
install.packages("dplyr")
library(dplyr)
Terror_EU <- Terror_EU %>%
  select(-starts_with("Death.Age."))

########################################## 1 wykres 

library(shiny)
library(plotly)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Liczba ataków terrorystycznych w krajach europejskich na przestrzeni lat"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearRange", "Wybierz rok:",
                  min = min(Terror_EU$Year),
                  max = max(Terror_EU$Year),
                  value = min(Terror_EU$Year),
                  step = 1)),
    mainPanel(plotlyOutput("terroristPlot"))))
server <- function(input, output) {
  filteredData <- reactive({
    subset(Terror_EU, Year == input$yearRange & Terrorist.attacks > 0)})
  output$terroristPlot = renderPlotly({
    fig <- plot_ly(
      filteredData(),
      x = ~Entity,
      y = ~Terrorist.attacks,
      type = "bar",
      text = ~paste("Liczba ataków: ", Terrorist.attacks),
      hoverinfo = "text") %>% 
      layout( xaxis = list(title = "Państwa Europy"),
        yaxis = list(title = "Liczba ataków terrorystycznych"),
        showlegend = FALSE)
    fig})}
shinyApp(ui, server)

###################################################### 2 Wykres
sums_by_country <- aggregate(Terror_EU$Terrorist.attacks, by = list(Entity = Terror_EU$Entity), sum)
selected_countries <- sums_by_country[sums_by_country$x > 200, ]
ggplot(selected_countries, aes(x = Entity, y = x, fill = Entity)) +
  geom_bar(stat = "identity") +
  labs(title = "Sumy ataków terrorystycznych w Europie",
       x = "Państwa Europy", y = "Suma ataków terrorystycznych >200") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

################################################### 3 Wykres 
create_pie_chart_ggplot <- function(data) {
  pie_chart = ggplot(data, aes(x = "", y = value, fill = variable)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = "right") +
    labs(title = 'Państwa gdzie ataki terrorystyczne <100') +
    scale_fill_manual(name = "Rodzaj ataku",
                      values = c("Attack.method..Hijacking" = "red",
                                 "Attack.method..Hostage.Taking..Barricade.Incident." = "orange",
                                 "Attack.method..Unarmed.Assault" = "yellow",
                                 "Attack.method..Facility.Infrastructure.Attack" = "green",
                                 "Attack.method..Hostage.Taking..Kidnapping." = "blue",
                                 "Attack.method..Assassination" = "purple",
                                 "Attack.method..Armed.Assault" = "brown",
                                 "Attack.method..Bombing.Explosion" = "gray"))
  return(pie_chart)}
ui <- fluidPage(
  titlePanel("Ataki terrorystyczne według rodzaju w całym okresie"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Wybierz państwo:", 
                  choices = sums_by_country$Entity[sums_by_country$x > 100])),
    mainPanel(plotOutput("pieChart"))))
server <- function(input, output) {
  output$pieChart <- renderPlot({
    selected_data <- Terror_EU[Terror_EU$Entity == input$country, ]
    selected_data <- selected_data[, c("Attack.method..Hijacking", "Attack.method..Hostage.Taking..Barricade.Incident.", 
                                       "Attack.method..Unarmed.Assault", "Attack.method..Facility.Infrastructure.Attack",
                                       "Attack.method..Hostage.Taking..Kidnapping.", "Attack.method..Assassination",
                                       "Attack.method..Armed.Assault", "Attack.method..Bombing.Explosion")]
    selected_data_long <- tidyr::gather(selected_data, key = "variable", value = "value")
    pie_chart <- create_pie_chart_ggplot(selected_data_long)
    print(pie_chart)})}
shinyApp(ui, server)

################################################### 4 Wykres 
sums_by_attack_method <- colSums(Terror_EU[, grep("Attack.method", colnames(Terror_EU))], na.rm = TRUE)
sums_data <- data.frame(AttackMethod = names(sums_by_attack_method), Count = as.numeric(sums_by_attack_method))

ggplot(sums_data, aes(x = AttackMethod, y = Count, fill = AttackMethod)) +
  geom_bar(stat = "identity") +
  labs(title = "Sumy ataków terrorystycznych według rodzaju",
       x = "Rodzaj ataku", y = "Suma ataków terrorystycznych") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

################################################### 5 Wykres 
create_pie_chart2 <- function(data) {
  total_deaths <- sum(data$Terrorism.deaths)
  
  percentages <- c(
    Suicide = sum(data$Terrorist.Death.Type...Suicide) / total_deaths * 100,
    Killed = sum(data$Terrorist.Death.Type...Killed) / total_deaths * 100
  )
    pie_chart <- ggplot() +
    geom_bar(aes(x = "", y = percentages, fill = names(percentages)), stat = "identity", width = 1) +
    geom_text(aes(x = 1.5, y = cumsum(percentages) - percentages / 2, 
                  label = paste(names(percentages), sprintf("%.1f%%", percentages))), 
              color = "black", size = 6) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = "right") +
    labs(title = 'Procentowy udział ofiar śmiertelnych zabitych przez samobójców 
         i terrorystów zabitych nie z własnej woli ') +
    scale_fill_manual(name = "Typ ofiary",
                      values = c("Suicide" = "blue", "Killed" = "lightblue"))
    return(pie_chart)}
ui <- fluidPage(
  titlePanel("Analiza zabójstw terrorystów w poszczególnych krajach"),
  sidebarLayout(
    sidebarPanel(
      selectInput("country", "Wybierz państwo:", choices = unique(Terror_EU$Entity))),
    mainPanel(
      plotOutput("pieChart"))))
server <- function(input, output) {
  output$pieChart <- renderPlot({
    selected_data <- Terror_EU[Terror_EU$Entity == input$country, ]
    pie_chart <- create_pie_chart2(selected_data)
    print(pie_chart)})}

shinyApp(ui, server)

################################################### 6 wykres
data = data.frame(
  Type = c("Suicide", "Killed"),
  Deaths = c(sum(Terror_EU$Terrorist.Death.Type...Suicide),sum(Terror_EU$Terrorist.Death.Type...Killed))
)
ggplot(data, aes(x = Type, y = Deaths, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Deaths), position = position_dodge(width = 0.9), vjust = -0.5) +  
  labs(title = "Ofiary śmiertelne samobójców \n i zabitych terrorystów",
       x = "Typ zabójcy", y = "Liczba ofiar") +
  scale_fill_manual(name = "Typ zabójcy",
                    values = c("Suicide" = "blue", "Killed" = "lightblue")) +
  theme_minimal()
## 7 Wykres 
ggplot(Terror_EU, aes(x = Terrorist.attacks, y = Terrorism.deaths)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Relacja między liczbą ataków \n terrorystycznych,a liczbą ofiar śmiertelnych",
       x = "Liczba ataków terrorystycznych",
       y = "Liczba ofiar śmiertelnych") +
  theme_minimal()


