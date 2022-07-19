library(shiny)
library(ximage)
library(whatarelief)

get_map <- function(x = c(-180, 180, -90, 90), dimension = c(360, 180)) {
  whatarelief::elevation(extent = x,
                         threshold = 0.05, resample = "near")
}

## shamelessly stolen from Wotherspoon's SGATII work
topo_getter <- function(extent = c(-180, 180, -90, 90), dimension = c(1280, 640)) {

  runApp(shinyApp(

    ui=fluidPage(
      title="Topography",
      checkboxInput("native_resolution", "Native resolution", FALSE),
      plotOutput(outputId = "topo_image",width="100%",click="image_click",dblclick="image_dblclick",
                 hover=hoverOpts(id="image_hover",delay=80),brush=brushOpts(id="image_brush",delay=400,resetOnNew=TRUE)),

      tags$script('
  pressedKeyCount = 0;
  $(document).on("keydown", function (e) {
       Shiny.setInputValue("pressedKey", pressedKeyCount++);
       Shiny.setInputValue("pressedKeyId", e.which);
  });'
      )),


    server=function(input, output, session) {


      ext <- reactiveValues(ext=extent)
      mapdata <- reactiveValues(map = get_map(dimension = dimension))

      session$onSessionEnded(function() {
        stopApp(isolate(ext$ext))
      })



      observeEvent(input$image_brush,{
        b <- input$image_brush
        ext$ext <- c(b$xmin,b$xmax,b$ymin,b$ymax)
        mapdata$map <- get_map(c(b$xmin,b$xmax,b$ymin,b$ymax), dimension = dimension)
      })

      observeEvent(input$image_dblclick,{

        ext$ext <- c(-180, 180, -90, 90)
        mapdata$map <- get_map(c(-180, 180, -90, 90), dimension = dimension)
      })


      ## Plot window
      output$topo_image <- renderPlot({
        ximage(mapdata$map, extent = ext$ext, asp = 1/cos(mean(ext$ext[3:4]) * pi/180), col = grey.colors(255))

      },height=600)

    }))
}


topo_getter()


