library(shiny)
library(tidyverse)

# UI
ui <- fluidPage(
  titlePanel("Blood Type Proportions Across Mexican States"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("region", "Select Region(s):", 
                         choices = unique(df_long$Region), 
                         selected = unique(df_long$Region)[1:6]
                         ),
      
      radioButtons("bloodTypeView", "Blood Type Display:",
                   choices = c("Rh specified" = "specific",
                               "Rh unspecified" = "general"),
                   selected = "specific"),
      
      conditionalPanel(
        condition = "input.bloodTypeView == 'specific'",
        checkboxGroupInput("bloodType", "Select Blood Type(s):",
                           choices = c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"),
                           selected = c("A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-"))
      ),
      
      conditionalPanel(
        condition = "input.bloodTypeView == 'general'",
        checkboxGroupInput("baseBloodType", "Select Blood Type(s):",
                           choices = c("A", "B", "AB", "O"),
                           selected = c("A", "B", "AB", "O"))
      ),
      
      radioButtons("sort", "Sort States By:", 
                   choices = c("Alphabetical", "Total Sample Size"), 
                   selected = "Alphabetical"),
      
 #     checkboxInput("showPie", "Show Pie Charts", value = TRUE)
    ),
    
    mainPanel(
      fluidRow(
        column(8, plotOutput("bloodPlot", height = "600px")),
        column(4, conditionalPanel(
          condition = "input.showPie == true",
          plotOutput("pieChart", height = "600px")
        ))
      ),
      tableOutput("summaryTable")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    data <- df_long %>% filter(Region %in% input$region)
    
    if (input$bloodTypeView == "specific") {
      data <- data %>% filter(BloodType %in% input$bloodType)
    } else {
      data <- data %>%
        filter(BaseBloodType %in% input$baseBloodType) %>%
        group_by(Region, State, N, BaseBloodType) %>%
        summarise(Count = sum(Count), 
                  Percentage = sum(Percentage), 
                  .groups = "drop") %>%
        rename(BloodType = BaseBloodType)
    }
    
    data
  })
  
  output$bloodPlot <- renderPlot({
    data <- filtered_data()
    
    if (input$sort == "Total Sample Size") {
      state_order <- data %>% 
        group_by(State) %>% 
        summarise(N = first(N), .groups = "drop") %>% 
        arrange(desc(N)) %>% 
        pull(State)
    } else {
      state_order <- sort(unique(data$State))
    }
    
    data$State <- factor(data$State, levels = state_order)
    
    ggplot(data, aes(x = State, y = Count, fill = BloodType)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Blood Type Distribution by State", x = "State", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
  })
  
  output$summaryTable <- renderTable({
    filtered_data() %>%
      group_by(BloodType) %>%
      summarise(Total = sum(Count), 
                Mean = mean(Count), 
                SD = sd(Count), 
                .groups = "drop")
  })
  
  # output$pieChart <- renderPlot({
  #   data <- filtered_data() %>%
  #     group_by(BloodType) %>%
  #     summarise(Total = sum(Count), .groups = "drop")
  #   
  #   ggplot(data, aes(x = "", y = Total, fill = BloodType)) +
  #     geom_bar(stat = "identity", width = 1) +
  #     coord_polar("y") +
  #     theme_void() +
  #     labs(title = "Overall Blood Type Proportions") +
  #     scale_fill_brewer(palette = "Set2")
  # })
}

# Run the app
shinyApp(ui = ui, server = server)
