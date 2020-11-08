library(shiny)
library(keras)
library(tensorflow)
library("jpeg")

#mohlo by fungovat 
#library(imager)
#im<-load.image("myimage")
#plot(im)


# Load the model
model <- load_model_tf("nature-mod/")

# Define the UI
ui <- fluidPage(
  # App title ----
  titlePanel("Recognize mushroom, tree or flower"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: File upload
      fileInput("Pic1", label = "Select image",
                multiple = F,
                accept = "image/*")
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Prediction ---
      textOutput(outputId = "prediction"),
      plotOutput(outputId = "image")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  image <- reactive({
    req(input$Pic1)
    jpeg::readJPEG(input$Pic1$datapath)
  })
  
  output$prediction <- renderText({
    
    img <- image() %>% 
      array_reshape(., dim = c(1, dim(.), 1))
    
    paste0("The predicted object is ", predict_classes(model, img))
  })
  
  output$image <- renderPlot({
    plot(as.raster(image()))
  })
  
}

shinyApp(ui, server)
