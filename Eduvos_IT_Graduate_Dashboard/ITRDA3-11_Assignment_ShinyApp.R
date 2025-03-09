# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(shinythemes)
library(RColorBrewer)

# Load the dataset 
campus_filtered <- read.csv("~/Documents/Eduvos University/Data Science 3rd Year/Block 1/ITRDA3-11 /Assignment/campus_filtered.csv", 
                            stringsAsFactors = FALSE)

# Ensure necessary columns exist (Prevents missing column errors)
expected_columns <- c("AISearch", "AITool", "Platforms", "ProgLang", "Databases", "WebFramework", "Employment")
for (col in expected_columns) {
  if (!(col %in% colnames(campus_filtered))) {
    campus_filtered[[col]] <- ""  # Create empty column if missing
  }
}

# Convert necessary columns to character type
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
                              "Employment Rate", "AI Tools", "AI Search", "Platforms", "Graduate Trends")), 
      
      selectInput("graph_color", "Choose Graph Color:", 
                  choices = c("Default", "Blue", "Red", "Green", "Purple"), selected = "Default"), 
      
      checkboxInput("show_labels", "Show Data Labels", value = TRUE), 
      checkboxInput("show_grid", "Show Gridlines", value = TRUE), 
      
      downloadButton("download_plot", "Download Graph") 
    ),
    
    mainPanel(
      plotOutput("data_plot", height = "700px", hover = hoverOpts(id = "plot_hover")) 
    )
  )
)

#SERVER LOGIC

server <- function(input, output) {
  
  # Function to assign color palette dynamically
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
  
  # Render the selected visualization
  output$data_plot <- renderPlot({
    
    filtered_data <- campus_filtered %>%
      filter(StudyField == input$study_field)
    
    # Bar Chart for Programming Languages
    if (input$visual_type == "Programming Languages") {
      plot_data <- filtered_data %>% separate_rows(ProgLang, sep = ";") %>%
        count(ProgLang, sort = TRUE)
      
      color_values <- setNames(get_color_palette(unique(plot_data$ProgLang), input$graph_color), unique(plot_data$ProgLang))
      
      ggplot(plot_data, aes(x = reorder(ProgLang, n), y = n, fill = ProgLang)) +
        geom_col(width = 0.7) +
        labs(title = "Top Programming Languages", x = "Language", y = "Number of Graduates") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
              panel.grid.major = element_line(color = ifelse(input$show_grid, "grey", NA))) +
        scale_fill_manual(values = color_values) +
        geom_text(aes(label = n), hjust = -0.2, size = 4)
      
      # Pie Chart for Employment Rate
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
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), color = "white", size = 6)
      
      # Bar Chart for AI Tools
    } else if (input$visual_type == "AI Tools") {
      plot_data <- filtered_data %>% separate_rows(AITool, sep = ";") %>% count(AITool, sort = TRUE)
      
      if(nrow(plot_data) == 0) return(NULL) 
      
      ggplot(plot_data, aes(x = reorder(AITool, n), y = n, fill = AITool)) +
        geom_col(width = 0.7) +
        labs(title = "Most Popular AI Tools", x = "AI Tools", y = "Number of Graduates") +
        theme_minimal()
      
      # Bar Chart for AI Search
    } else if (input$visual_type == "AI Search") {
      plot_data <- filtered_data %>% separate_rows(AISearch, sep = ";") %>% count(AISearch, sort = TRUE)
      
      if(nrow(plot_data) == 0) return(NULL) 
      
      ggplot(plot_data, aes(x = reorder(AISearch, n), y = n, fill = AISearch)) +
        geom_col(width = 0.7) +
        labs(title = "Most Popular AI Search Tools", x = "AI Search", y = "Number of Graduates") +
        theme_minimal()
      
      # Line Graph for Graduate Trends
    } else if (input$visual_type == "Graduate Trends") {
      trend_data <- campus_filtered %>%
        group_by(StudyField) %>%
        summarise(Graduates = n())
      
      ggplot(trend_data, aes(x = StudyField, y = Graduates, group = 1)) +
        geom_line(size = 1, color = "blue") +
        geom_point(size = 3, color = "red") +
        labs(title = "Graduate Trends Over Time", x = "Study Field", y = "Number of Graduates") +
        theme_minimal()
    }
  })
  
  # Download graph functionality
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("Eduvos_Insights", input$visual_type, ".png", sep = "_") 
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), width = 10, height = 6)
    }
  )
}

# RUN THE SHINY APP
shinyApp(ui = ui, server = server)
