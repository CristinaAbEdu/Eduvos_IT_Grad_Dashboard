library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)  # Load this to fix str_trim() error

# Load the cleaned dataset 
campus_filtered <- read.csv("~/Documents/Eduvos University/Data Science 3rd Year/Block 1/ITRDA3-11 /Assignment/campus_filtered.csv", stringsAsFactors = FALSE)

# User Interface (UI) - This is what the user sees
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),  # Apply a sleek theme
  
  # Dashboard title with styling
  titlePanel(div(style = "color: #2C3E50; font-size: 28px; font-weight: bold; text-align: center;", "Eduvos IT Graduate Insights")),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for user selections with styling
    sidebarPanel(
      selectInput("study_field", "Choose a Study Field:", 
                  choices = unique(campus_filtered$StudyField)),
      selectInput("visual_type", "Choose What to View:", 
                  choices = c("Programming Languages", "Databases", "Web Frameworks", "Employment Rate")),
      style = "background-color: #ECF0F1; padding: 15px; border-radius: 10px;"
    ),
    
    # Main panel where results will be displayed
    mainPanel(
      plotOutput("data_plot", height = "700px"),  # Increased plot size
      style = "background-color: white; padding: 20px; border-radius: 10px;"
    )
  )
)

# Server - This is where the logic happens
server <- function(input, output) {
  
  output$data_plot <- renderPlot({
    
    # Filter data based on user input
    filtered_data <- campus_filtered %>%
      filter(StudyField == input$study_field)
    
    # Generate the appropriate chart based on user selection
    if (input$visual_type == "Programming Languages") {
      plot_data <- filtered_data %>% separate_rows(ProgLang, sep = ";") %>%
        count(ProgLang, sort = TRUE)
      
      ggplot(plot_data, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
        geom_col() +
        coord_flip() +
        labs(title = "Top Programming Languages Used", x = "Language", y = "Number of Graduates") +
        theme_minimal() +
        theme(legend.position = "right", plot.title = element_text(face = "bold", size = 18))
      
    } else if (input$visual_type == "Databases") {
      plot_data <- filtered_data %>% separate_rows(Databases, sep = ";") %>%
        count(Databases, sort = TRUE)
      
      ggplot(plot_data, aes(x = reorder(Databases, n), y = n, fill = Databases)) +
        geom_col() +
        coord_flip() +
        labs(title = "Most Popular Databases", x = "Database", y = "Number of Graduates") +
        theme_minimal() +
        theme(legend.position = "right", plot.title = element_text(face = "bold", size = 18))
      
    } else if (input$visual_type == "Web Frameworks") {
      plot_data <- filtered_data %>% separate_rows(WebFramework, sep = ";") %>%
        count(WebFramework, sort = TRUE)
      
      ggplot(plot_data, aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
        geom_col() +
        coord_flip() +
        labs(title = "Top Web Frameworks", x = "Web Framework", y = "Number of Graduates") +
        theme_minimal() +
        theme(legend.position = "right", plot.title = element_text(face = "bold", size = 18))
      
    } else if (input$visual_type == "Employment Rate") {
      employment_summary <- filtered_data %>%
        separate_rows(Employment, sep = ";") %>%
        mutate(Employment = str_trim(Employment)) %>%
        mutate(Employment = tolower(Employment)) %>%
        mutate(Employed = ifelse(grepl("employed|self-employed|independent contractor", Employment, ignore.case = TRUE), "Employed", "Unemployed")) %>%
        count(Employed) %>%  # Count employed vs. unemployed
        mutate(Percentage = (n / sum(n)) * 100)  # Calculate percentage
      
      print(employment_summary)  # Debugging step to check values
      
      ggplot(employment_summary, aes(x = "", y = Percentage, fill = Employed)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Graduate Employment Rate", x = "", y = "Percentage of Graduates") +
        theme_void() +
        theme(legend.title = element_blank(), plot.title = element_text(face = "bold", size = 18)) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), color = "white", size = 6)
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)


