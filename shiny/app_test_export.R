library(shiny)

ui <- pageWithSidebar(
  headerPanel("renderImage example"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
      min = 0, max = 1000, value = 500
    ),
    actionButton("button", "Update", icon("refresh"),
                 class = "btn btn-primary")
  ),
  mainPanel(
    imageOutput("myImage"),
    downloadButton("download", "Download Image")
  )
)

server <- function(input, output, session) {
  image <- eventReactive(eventExpr = input$button, {
    outfile <- tempfile(fileext = ".png")

    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()

    list(
      src = outfile,
      contentType = "image/png",
      width = 400,
      height = 300,
      alt = "This is alternate text"
    )
  })
  
  output$myImage <- renderImage(
    {
      image()
    },
    deleteFile = FALSE
  )

  output$download <- downloadHandler(
    filename = function() {
      "generated_plot.png"
    },
    content = function(file) {
      img <- image()$src
      file.copy(img, file)
    }
  )
}

shinyApp(ui, server)