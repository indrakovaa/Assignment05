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
    image_array_resize(jpeg::readJPEG(input$Pic1$datapath), height=160, width=160, data_format = c("channels_last"))
  })
  
  output$prediction <- renderText({
    
    img <- image() %>% 
      array_reshape(., dim = c(1, dim(.), 1))
        
    categories = c("flower", "mushroom", "tree")
    probability=c(predict(model, img))
    pred_name = categories[which.max(probability)]
    paste0("The predicted object is ", pred_name, " with probality: ", probability[which.max(probability)])
  })
  
  output$image <- renderPlot({
    plot(as.raster(image()))
  })
  
}

shinyApp(ui, server)
