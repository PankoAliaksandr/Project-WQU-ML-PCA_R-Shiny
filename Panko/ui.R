library(shiny)

# Define UI for app
ui <- fluidPage(
  
  # App title
  titlePanel("Clustering Countries"),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel( 
      
      # Input: Slider for the number of clusters
      sliderInput(inputId = "NumberOfClusters",
                  label = "Number of clusters:",
                  min = 2,
                  max = 8,
                  value = 3),
      
      #Input; Radiobuttons for Ellipse Type
      radioButtons("EllipseType", "Ellipse Type:",
                   c("Convex" = "Convex",
                     "Euclid" = "Euclid")),
      
      # Output: PCA
      plotOutput("PCA")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Clusters Plot
      plotOutput(outputId = "Clusters"),
      
      # Output: World Map Plot
      plotOutput(outputId = "WorldMap")
      
    )
  )
)
