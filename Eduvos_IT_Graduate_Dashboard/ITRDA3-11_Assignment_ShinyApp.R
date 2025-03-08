library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(shinythemes)

# Load the cleaned dataset
campus_filtered <- read.csv("~/Documents/Eduvos University/Data Science 3rd Year/Block 1/ITRDA3-11 /Assignment/campus_filtered.csv", stringsAsFactors = FALSE)

# Ensure necessary columns exist in case they are missing
expected_columns <- c("AISearch", "AITool", "Platforms")
for (col in expected_columns) {
  if (!(col %in% colnames(campus_filtered))) {
    campus_filtered[[col]] <- ""  # Create the column if it's missing
  }
}

# Convert necessary columns to character to avoid errors
campus_filtered <- campus_filtered %>% mutate(across(all_of(expected_columns), as.character))

# User Interface (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Eduvos IT Graduate Insights"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("study_field", "Choose a Study Field:", choices = unique(campus_filtered$StudyField)),
      selectInput("visual_type", "Choose What to View:", 
                  choices = c("Programming Languages", "Databases", "Web Frameworks", "Employment Rate", 
                              "AI Tools", "AI Search", "Platforms")),
    ),
    mainPanel(
      plotOutput("data_plot", height = "700px", hover = hoverOpts(id = "plot_hover"))
    )
  )
)

# Server Logic
server <- function(input, output) {
  output$data_plot <- renderPlot({
    filtered_data <- campus_filtered %>%
      filter(StudyField == input$study_field)
    
    if (input$visual_type == "Programming Languages") {
      plot_data <- filtered_data %>% separate_rows(ProgLang, sep = ";") %>%
        count(ProgLang, sort = TRUE)
      ggplot(plot_data, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
        geom_bar(stat = "identity", width = 0.6) +
        labs(title = "Top Programming Languages Used", x = "Language", y = "Number of Graduates") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$visual_type == "Databases") {
      plot_data <- filtered_data %>% separate_rows(Databases, sep = ";") %>%
        count(Databases, sort = TRUE)
      ggplot(plot_data, aes(x = Databases, y = n, fill = Databases)) +
        geom_point(size = 5) +
        labs(title = "Most Popular Databases", x = "Database", y = "Number of Graduates") +
        theme_minimal()
    } else if (input$visual_type == "Web Frameworks") {
      plot_data <- filtered_data %>% separate_rows(WebFramework, sep = ";") %>%
        count(WebFramework, sort = TRUE)
      ggplot(plot_data, aes(x = reorder(WebFramework, n), y = n, fill = WebFramework)) +
        geom_col(width = 0.6) + coord_flip() +
        labs(title = "Top Web Frameworks", x = "Web Framework", y = "Number of Graduates") +
        theme_minimal()
    } else if (input$visual_type == "Employment Rate") {
      employment_summary <- filtered_data %>%
        separate_rows(Employment, sep = ";") %>%
        mutate(Employment = str_trim(Employment)) %>%
        mutate(Employment = tolower(Employment)) %>%
        mutate(Employed = ifelse(grepl("employed|self-employed|independent contractor", Employment, ignore.case = TRUE), "Employed", "Unemployed")) %>%
        count(Employed) %>%
        mutate(Percentage = (n / sum(n)) * 100)
      ggplot(employment_summary, aes(x = "", y = Percentage, fill = Employed)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "Graduate Employment Rate", x = "", y = "Percentage of Graduates") +
        theme_void() +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_stack(vjust = 0.5), color = "white", size = 6)
    } else if (input$visual_type == "AI Tools") {
      plot_data <- filtered_data %>% separate_rows(AITool, sep = ";") %>% count(AITool, sort = TRUE)
      ggplot(plot_data, aes(x = AITool, y = n, fill = AITool)) +
        geom_line(aes(group = 1), size = 1) +
        labs(title = "Most Popular AI Tools", x = "AI Tools", y = "Number of Graduates") +
        theme_minimal()
    } else if (input$visual_type == "AI Search") {
      plot_data <- filtered_data %>% separate_rows(AISearch, sep = ";") %>% count(AISearch, sort = TRUE)
      ggplot(plot_data, aes(x = AISearch, y = n, fill = AISearch)) +
        geom_bar(stat = "identity", width = 0.6) +
        labs(title = "Most Popular AI Search Tools", x = "AI Search", y = "Number of Graduates") +
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    } else if (input$visual_type == "Platforms") {
      plot_data <- filtered_data %>% separate_rows(Platforms, sep = ";") %>% count(Platforms, sort = TRUE)
      ggplot(plot_data, aes(x = reorder(Platforms, n), y = n, fill = Platforms)) +
        geom_col(width = 0.6) + coord_flip() +
        labs(title = "Most Popular Platforms", x = "Platforms", y = "Number of Graduates") +
        theme_minimal()
    }
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)

