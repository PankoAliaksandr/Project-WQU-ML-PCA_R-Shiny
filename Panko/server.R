# Libraries
library(shiny)
library(openxlsx)
library(caret)
library(randomcoloR)
library(rworldxtra)
library(factoextra)
library(rworldmap)
library(gridExtra)

# Define server
server <- function(input, output) {
  
  df = read.xlsx(xlsx = "CountryData.xlsx")
  
  # Removing blank rows generated
  df<- df[c(1:21)]
  
  # Renaming the varibles to be human readable :)
  names(df)[3:21]<- c("ForeignInvestment", "ElectricityAccess", "RenewableEnergy", "CO2Emission", "Inflation", "MobileSubscriptions", "InternetUse", "Exports", "Imports", "GDP", "MortalityMale", "MortalityFemale", "BirthRate", "DeathRate", "MortalityInfant", "LifeExpectancy", "FertilityRate", "PopulationGrowth", "UrbanPopulation")
  
  # Setting seed so out results are imputation results are reprodcuible
  set.seed(0)
  
  # We impute missing values with a random forest
  imputation_model = preProcess(x = df[,-c(1,2)],method = "bagImpute")
  imputated_data = predict(object = imputation_model,newdata=df[,-c(1,2)])
  
  # Adding country names to the rows
  rownames(imputated_data)<-df[,2] 
  
  # Scaling our data
  scaled_data = scale(imputated_data)
  
  p <- reactive({
    palette <- distinctColorPalette(input$NumberOfClusters)
  })
  
  # We plot to the World Map
  output$WorldMap <- renderPlot({
    
    # K-means clustering
    km.res <- kmeans(scaled_data, input$NumberOfClusters, nstart = 50)
    
    cluster = as.numeric(km.res$cluster)
    
    par(mfrow=c(1,1))
    spdf = joinCountryData2Map(data.frame(cluster,df$CountryName), joinCode="NAME", nameJoinColumn="df.CountryName",verbose = TRUE,mapResolution = "low")
    mapCountryData(spdf, nameColumnToPlot="cluster", catMethod="fixedWidth",colourPalette = palette(),addLegend = TRUE, lwd = 0.5, mapTitle = "World Map")
    
  }
  )
  
  # We plot to Clusters
  output$Clusters <- renderPlot({
    
    # K-means clustering
    km.res <- kmeans(scaled_data, input$NumberOfClusters, nstart = 50)
    
    # Cluster visualisation
    library(factoextra) 
    if (input$EllipseType == "Euclid"){
      ellipse_type = "euclid"
    }
    else if (input$EllipseType == "Convex"){
      ellipse_type = "convex"
    }
    
    fviz_cluster(km.res, 
                 data = scaled_data,
                 ellipse.type = ellipse_type,
                 palette = palette(),
                 star.plot = TRUE, # Add segments from centroids to items
                 repel = TRUE, # Avoid label overplotting (slow)
                 ggtheme = theme_minimal(),
                 main = "Clusters"
    )
  }
  
  )
  
  
  # We plot to PCA
  output$PCA <- renderPlot({
    
    # PCA for our Biplot. Note scale =TRUE as we need to standardize our data
    pca.out<-prcomp(imputated_data,scale=TRUE)
    
    # contributions 
    plot1 <- fviz_contrib(pca.out, choice="var", axes = 1, top = 19)
    plot2 <- fviz_contrib(pca.out, choice="var", axes = 2, top = 19, color = "lightgrey")
    
    grid.arrange(plot1, plot2, nrow=2)
    
  }
  )
}