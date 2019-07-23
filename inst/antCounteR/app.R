library(shiny)
library(shinyBS)
library(shinyFiles)
library(shinyjs)
library(Rvision)
library(antCounteR)

#####
# Javascript
#####
jscode <- "shinyjs.init = function() {
  $(document).keydown(function(e) {
    // alert('Key pressed: ' + e.which);

    if(e.which == 32) {
      e.preventDefault();
      $('#playPause')[0].click();
    };

    if(e.which == 37) {
      e.preventDefault();
      $('#minusFrame')[0].click();
    };

    if(e.which == 39) {
      e.preventDefault();
      $('#plusFrame')[0].click();
    };

    if(e.which == 40) {
      e.preventDefault();
      $('#minusSec')[0].click();
    };

    if(e.which == 38) {
      e.preventDefault();
      $('#plusSec')[0].click();
    };
  });
}"

####
# Globals
####
my_stream <- c()
my_writer <- c()
save_path <- c()

search <- TRUE
counter <- 0
cam_list <- c("No camera selected")
while (search) {
  cam <- try(stream(counter), TRUE)

  if (inherits(cam, "try-error")) {
    search <- FALSE
  } else {
    release(cam)
    cam_list <- c(cam_list, counter)
    counter <- counter + 1
  }
}

#####
# UI
#####
ui <- function(request) {
  shinyUI(fluidPage(
    id = "ui",

    titlePanel("antCounteR"),

    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    useShinyjs(),
    extendShinyjs(text = jscode, functions = "onKeypress"),

    fluidRow(
      column(
        12,

        bsCollapse(
          id = "panels",
          multiple = TRUE,
          open = c("selectingPanel", "controlPanel", "statsPanel"),
          bsCollapsePanel(
            title = NULL,
            value = "selectingPanel",
            selectInput("camera", "Select camera from list", "No camera selected", width = "100%")
          ),

          bsCollapsePanel(
            title = NULL,
            value = "controlPanel",

            tags$table(style = "width: 100%; margin-bottom: 10px;",
                       tags$tr(
                         disabled(shinySaveButton("output", "Select file", "Please select a file",
                                                  filetype = c("mp4"), class = "fullWidth"))
                       ),
                       tags$tr(
                         disabled(actionButton("start", "Start", class = "halfWidth")),
                         disabled(actionButton("stop", "Stop", class = "halfWidth"))
                       )
            )
          )
        )
      )
    )
  ))
}


#####
# SERVER
#####
server <- function(input, output, session) {
  #####
  # Select camera
  #####
  updateSelectInput(session, "camera", choices = cam_list)

  the_cam <- reactiveVal(-1)
  observe({
    invalidateLater(40, session)

    if (input$camera != "No camera selected") {
      if (the_cam() < 0) {
        the_cam(as.numeric(input$camera))
        my_stream <<- stream(the_cam())
        enable("output")
      }

      if (isStream(my_stream)) {
        if (isVideoWriter(my_writer)) {
          writeFrame(my_writer, readNext(my_stream))
          write(format(Sys.time(), "%Y-%m-%d %H:%M:%OS6"),
                file = paste0(save_path$datapath, ".csv"), append = TRUE)
        } else {
          display(readNext(my_stream), window_name = "antCounteR", delay = 1,
                  height = round(nrow(my_stream) / 2),
                  width = round(ncol(my_stream) / 2))
        }
      }
    } else {
      if (isStream(my_stream)) {
        destroyDisplay("antCounteR")
        release(my_stream)
        my_stream <<- c()
      }
      the_cam(-1)
      disable("output")
    }
  })

  #####
  #
  #####
  shinyFileSave(input, "output", roots = c(home = normalizePath("~")))

  observeEvent(input$output, {
    save_path <<- parseSavePath(roots = c(home = normalizePath("~")), input$output)

    if (nrow(save_path) > 0) {
      enable("start")
    }
  })

  #####
  #
  #####
  observeEvent(input$start, {
    disable("start")
    enable("stop")
    my_writer <<- videoWriter(save_path$datapath, "X264", 25, height = nrow(my_stream),
                              width = ncol(my_stream))
  })

  observeEvent(input$stop, {
    disable("stop")
    release(my_writer)
    my_writer <<- c()
  })

  #####
  # Clean up
  #####
  session$onSessionEnded(function() {
    if (isStream(my_stream)) {
      release(my_stream)
    }
    destroyAllDisplays()
  })
}

shinyApp(ui = ui, server = server)
