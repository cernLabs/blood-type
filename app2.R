library(shiny)
library(tidyverse)

# Load pie chart data
df_pie <- df_long

# UI
ui <- fluidPage(
  titlePanel("Blood Type Proportions Across Mexican States"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("region", "Select Region(s):", 
                         choices = unique(df_long$Region), 
                         selected = unique(df_long$Region)),
      
      radioButtons("bloodTypeView", "Blood Type Display:",
                   choices = c("Specific (A+, A-, B+, etc.)" = "specific",
                               "General (A, B, AB, O only)" = "general"),
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
      
      checkboxInput("showPie", "Show Pie Charts", value = TRUE)
    ),
    
    mainPanel(
      fluidRow(
        column(8, plotOutput("bloodPlot", height = "600px")),
        column(4, uiOutput("pieChartUI"))
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
  
  output$pieChartUI <- renderUI({
    if (!input$showPie) return(NULL)
    
    selected_states <- unique(filtered_data()$State)
    
    tagList(
      lapply(selected_states, function(state) {
        pie_data <- df_pie %>% filter(State == state)
        if (nrow(pie_data) == 0) return(NULL)
        
        renderPlot({
          slices <- c(pie_data$A, pie_data$B, pie_data$AB, pie_data$O)
          labels <- c("A", "B", "AB", "O")
          pie(slices, labels = paste0(labels, ": ", round(slices, 1), "%"),
              main = paste("Blood Type Distribution in", state))
        })
      })
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
