# Eduvos IT Graduate Insights Dashboard - ShinyLive Compatible
# Developed using R Shiny & Plotly

# Load necessary libraries
library(shiny)          
library(ggplot2)        
library(dplyr)          
library(tidyr)          
library(stringr)        
library(shinythemes)    
library(RColorBrewer)   
library(plotly)         

# Load the dataset
campus_filtered <- read.csv("campus_filtered.csv", stringsAsFactors = FALSE)


# Ensure necessary columns exist (Prevents missing column errors)
expected_columns <- c("AITool", "Platforms", "ProgLang", "Databases", "WebFramework", "AISearch")
for (col in expected_columns) {
  if (!(col %in% colnames(campus_filtered))) {
    campus_filtered[[col]] <- ""  
  }
}

# Convert necessary columns to character type for consistency
campus_filtered <- campus_filtered %>% mutate(across(all_of(expected_columns), as.character))

# USER INTERFACE (UI)
ui <- fluidPage(
  theme = shinytheme("flatly"),  
  titlePanel("Eduvos IT Graduate Insights"),  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("study_field", "Choose a Study Field:", 
                  choices = unique(campus_filtered$StudyField)), 
      
      selectInput("visual_type", "Choose What to View:", 
                  choices = c("Programming Languages", "Databases", "Web Frameworks", 
                              "AI Search Tools", "Platforms", "Graduate Trends")), 
      
      selectInput("graph_color", "Choose Graph Color:", 
                  choices = c("Default", "Blue", "Red", "Green", "Purple"), selected = "Default"), 
      
      checkboxInput("show_labels", "Show Data Labels", value = TRUE), 
      checkboxInput("show_grid", "Show Gridlines", value = TRUE), 
      
      downloadButton("download_plot", "Download Graph")  
    ),
    
    mainPanel(
      plotlyOutput("data_plot", height = "700px")  # Interactive visualizations
    )
  )
)

# SERVER LOGIC
server <- function(input, output) {
  
  # Function to dynamically assign color palettes
  get_color_palette <- function(categories, selected_color) {
    base_palette <- brewer.pal(min(8, length(categories)), "Set2")
    
    if (selected_color == "Blue") {
      return(rep("blue", length(categories)))
    } else if (selected_color == "Red") {
      return(rep("red", length(categories)))
    } else if (selected_color == "Green") {
      return(rep("green", length(categories)))
    } else if (selected_color == "Purple") {
      return(rep("purple", length(categories)))
    }
    return(colorRampPalette(base_palette)(length(categories)))
  }
  
  # Render visualizations
  output$data_plot <- renderPlotly({
    
    filtered_data <- campus_filtered %>% filter(StudyField == input$study_field)
    
    text_size_fix <- theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size = 16, face = "bold"),
      plot.margin = margin(20, 20, 20, 20)
    )
    
    # BAR CHART: Programming Languages
    if (input$visual_type == "Programming Languages") {
      plot_data <- filtered_data %>% separate_rows(ProgLang, sep = ";") %>% count(ProgLang, sort = TRUE)
      if(nrow(plot_data) == 0) return(NULL)
      
      color_values <- setNames(get_color_palette(unique(plot_data$ProgLang), input$graph_color), unique(plot_data$ProgLang))
      
      p <- ggplot(plot_data, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
        geom_col(width = 0.7) +
        labs(title = "Top Programming Languages", x = "Language", y = "Number of Graduates") +
        theme_minimal() + text_size_fix +
        scale_fill_manual(values = color_values) +
        geom_text(aes(label = n), vjust = -0.5, size = 5)
      
      return(ggplotly(p))
      
      # SCATTER PLOT: Databases 
    } else if (input$visual_type == "Databases") {
      plot_data <- filtered_data %>% separate_rows(Databases, sep = ";") %>% count(Databases, sort = TRUE)
      if(nrow(plot_data) == 0) return(NULL)  
      
      p <- ggplot(plot_data, aes(x = Databases, y = n, color = Databases)) +
        geom_point(size = 5) + geom_line(group = 1) +
        labs(title = "Most Popular Databases", x = "Database", y = "Number of Graduates") +
        theme_minimal() + text_size_fix
      
      return(ggplotly(p))
      
      # TREEMAP: Web Frameworks 
    } else if (input$visual_type == "Web Frameworks") {
      plot_data <- filtered_data %>% separate_rows(WebFramework, sep = ";") %>% count(WebFramework, sort = TRUE)
      if(nrow(plot_data) == 0) return(NULL)
      
      p <- plot_ly(
        data = plot_data, 
        type = "treemap",
        labels = ~WebFramework,
        parents = NA,
        values = ~n,
        textinfo = "label+value"
      ) %>% layout(title = "Most Popular Web Frameworks")
      
      return(p)
      
      # BAR CHART: AI Search Tools
    } else if (input$visual_type == "AI Search Tools") {
      plot_data <- filtered_data %>% separate_rows(AISearch, sep = ";") %>% count(AISearch, sort = TRUE)
      if(nrow(plot_data) == 0) return(NULL)  
      
      p <- ggplot(plot_data, aes(x = reorder(AISearch, n), y = n, fill = AISearch)) +
        geom_col(width = 0.7) +
        labs(title = "Most Popular AI Search Tools", x = "AI Search Tool", y = "Number of Graduates") +
        theme_minimal() + text_size_fix +
        geom_text(aes(label = n), vjust = -0.5, size = 5)
      
      return(ggplotly(p))
      
      # PIE CHART: Platforms
    } else if (input$visual_type == "Platforms") {
      plot_data <- filtered_data %>% separate_rows(Platforms, sep = ";") %>% count(Platforms, sort = TRUE)
      if(nrow(plot_data) == 0) return(NULL)
      
      p <- plot_ly(plot_data, labels = ~Platforms, values = ~n, type = 'pie', textinfo = "label+percent") %>%
        layout(title = "Most Popular Platforms")
      
      return(p)
      
      # LINE CHART: Graduate Trends
    } else if (input$visual_type == "Graduate Trends") {
      trend_data <- campus_filtered %>% group_by(StudyField) %>% summarise(Graduates = n())
      if(nrow(trend_data) == 0) return(NULL)  
      
      p <- ggplot(trend_data, aes(x = StudyField, y = Graduates, group = 1)) +
        geom_line(size = 1.5, color = "blue") +
        geom_point(size = 4, color = "red") +
        labs(title = "Graduate Trends Over Time", x = "Study Field", y = "Number of Graduates") +
        theme_minimal() + text_size_fix
      
      return(ggplotly(p))
    }
  })
}

# RUN THE SHINY APP
shinyApp(ui = ui, server = server)
