library(shiny)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(stringr)
library(pheatmap)
library(ggplotify)

set.seed(420)

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "Select points with path", titleWidth = "400px"),

  dashboardSidebar(collapsed = TRUE),

  dashboardBody(
    tags$head(tags$style(HTML('.main-header .logo{
                              font-family: "Georgia", Times, "Times New Roman", serif;
                              font-weight: bold;
                              font-size: 24px;
                              }'
                              )
                         )
              ),

    fluidRow(box(title = "Find your path", status = "primary",
                 solidHeader = TRUE, collapsible = TRUE, width = 12,
                 fluidRow(
                   column(4, textInput("tit", "Type a title", "t-SNE")),
                   column(4, numericInput("dist_min", "Choose a maximum distance from path", value = 10, min = 0, step = 0.5)),
                   column(4, numericInput("nb_point", "Choose the number of points you want to print", value = 4000, min = 10, step = 50))
                   ),
                 actionButton("go", "see plot", class = "btn-primary btn-lg"),
                 tags$hr(),

                 radioButtons("how_selec", "Choose how you want to select the points",
                              choices = list("Lasso selection" = 1,
                                             "Drawing a path" = 2),
                              selected = 2, inline = TRUE),
                 plotlyOutput("tsne", height = "800px")
                 )
             ),
    conditionalPanel(condition = "input.how_selec == 2",
                     tags$hr(),
                     fluidRow(box(title = "See which points have been selected", status = "info",
                                  solidHeader = TRUE, collapsible = TRUE, width = 12,
                                  plotOutput("tsne2", height = "600px"),
                                  downloadButton("downpath", "Save plot")
                                  )
                              )
                     ),
    fluidRow(box(title = "See heatmap from selected points", status = "primary",
                 solidHeader = TRUE, collapsible = TRUE, width = 12,
                 checkboxInput("HP_norm", "Apply z-score norm to line", TRUE),
                 plotOutput("HP", height = "800px"),
                 tags$hr(),
                 downloadButton("downheatmap", "Save heatmap")
                 )
             ),

    tags$head(tags$style(HTML('* {font-family: "Rockwell";
                                  font-size: 16px;
                              }'
                              )
                         )
              )

  )
)

server <- function(input, output, session){


  tsne <- reactiveValues(  #define plot in order to print it according a button
    g = NULL
  )

  idx <- reactive({
    if(input$nb_point < nrow(ggd)){
      sample(1:nrow(ggd), input$nb_point)
    }
    else{
      1:nrow(ggd)
    }
    })
  tsne_r <- reactive({
    int_plot(ggd[idx(),], img = img, tit = input$tit)
  })
  observeEvent(input$go, {
    showNotification("Getting plot, this can take a while. Please wait", type = "message")

    tsne$g <- tsne_r()
  })

  output$tsne <- renderPlotly({
    tsne$g
  })


  ### Get data from event on the plot
  PR_event <- reactiveValues(
    interest = NULL,
    shape = NULL,
    proj = NULL
  )
  observeEvent(list(event_data("plotly_relayout", source = "M"),
                event_data("plotly_selected", source = "M")), {   #event_data will search for event in plotly plot from source M

    interest <- NULL
    ord <- NULL

    if(input$how_selec == 1){
      PR <- event_data("plotly_selected", source = "M")
      if(!is.null(PR)){
        PR <- PR$customdata
        interest <- as.character(PR)
        ord <- 1:length(PR)
      }
    }
    else if (input$how_selec == 2){
      PR <- event_data("plotly_relayout", source = "M")
      if(!is.null(PR) & length(PR$shapes) != 0){
        if(class(PR$shapes) == "character"){
          PR <- PR$shapes
        }
        else if ("path" %in% colnames(PR$shapes)){
          PR <- PR$shapes$path
          PR <- stringr::str_split(PR, "L")[[1]]
          PR <- stringr::str_remove_all(PR, "M")
          PR <- Reduce(rbind, lapply(PR, function(x) {
            n <- as.numeric(str_split(x, ",")[[1]])
            data.frame(x=n[1], y=n[2])
          })
          )
        }
        else{
          PR <- PR$shapes
          PR <- data.frame(x = c(PR$x0, PR$x1), y = c(PR$y0, PR$y1))
        }
        n_PR <- nrow(PR)
        if(n_PR > 10){
          PR <- spline_path(PR$x, PR$y, ceiling(n_PR*(2/3)))
          n_PR <- nrow(PR)
        }
        if(input$nb_point > 3000){
          PR <- coord_imgtoplt(PR, scale_imgplot)
        }
        dst <- dist_line(ggd[idx(),c("tSNE2", "tSNE1")], PR)   # get distance between point and line

        interest <- rownames(ggd[idx(),])[which(dst$dist < as.numeric(input$dist_min))]

        # all this part is for getting order of points
        ord <- dst$proj[which(dst$dist < as.numeric(input$dist_min))]
        if(length(ord) == 0){
          interest <- NULL
          ord <- NULL
          showNotification("No points are close enough from the path you drew ! Try another one", type = "error")
        }
        else{
          ord <- Reduce(rbind, lapply(ord, function(x){data.frame(x=x[1], y=x[2])}))
          ord$t <- dst$which_line[which(dst$dist < as.numeric(input$dist_min))]
          ord$new_t <- rep(0, nrow(ord))

          i = 1
          id_list <- list(0)
          for(k in unique(ord$t)[order(unique(ord$t))]){
            id <- which(ord$t == k)
            A <- ord[id, c("x", "y")]

            B <- as.double(PR[k,])
            C <- as.double(PR[k+1,])

            id_list[[i+1]] <- length(id) + id_list[[i]]
            if(B[2] == C[2]){
              if(B[1] < C[1]){
                ord$new_t[id][order(A$x)] <- (id_list[[i]] + 1):(id_list[[i]] + id_list[[i+1]])
              }
              else{
                ord$new_t[id][order(A$x, decreasing = TRUE)] <- (id_list[[i]] + 1):(id_list[[i]] + id_list[[i+1]])
              }
            }
            else if(B[2] < C[2]){
              ord$new_t[id][order(A$y)] <- (id_list[[i]] + 1):(id_list[[i]] + id_list[[i+1]])
            }
            else if(B[2] > C[2]){
              ord$new_t[id][order(A$y, decreasing = TRUE)] <- (id_list[[i]] + 1):(id_list[[i]] + id_list[[i+1]])
            }
            i = i+1
          }

          interest <- interest[order(ord$new_t)]
        }
      }
      else{
        interest <- NULL
        ord <- NULL
      }
    }

    # update values
    PR_event$interest <- interest
    PR_event$shape <- PR
    PR_event$proj <- ord
  })
  # clear the set of points when a double-click occurs
  observeEvent(event_data("plotly_doubleclick", source = "M"), {     #can change event for clearing selected things if you want
    PR_event$interest <- NULL
    PR_event$shape <- NULL
    PR_event$proj <- NULL
  })


  ### Draw path, selected points and projections on original plot
  tsne_path <- reactive({
    if(!is.null(PR_event$interest) & input$how_selec == 2){
      if(class(PR_event$shape) == "data.frame"){
        ggplot(ggd[idx(),], aes(tSNE2, tSNE1)) + geom_point(pch = 19, size = .7) +
          geom_path(data = PR_event$shape, aes(x,y), color = "black") +
          geom_point(data = PR_event$proj, aes(x,y, color = new_t)) +
          geom_point(data = ggd[PR_event$interest,], aes(tSNE2, tSNE1), color = "green") +
          theme_light() + coord_fixed(ratio = 1)
      }
      else
        NULL
    }
    else{
      NULL
    }
  })

  output$tsne2 <- renderPlot({
    tsne_path()
  })

  output$downpath <- downloadHandler(
    filename = function(){
      paste0("Path_drew", Sys.Date(), ".png")
    },
    content = function(file){
      ggsave(file, tsne_path(), device = "png")
    }
  )

  ### Draw heatmap from selected points
  HP <- reactive({
    if(!is.null(PR_event$interest)){
      if(input$HP_norm){
        df <- t(apply(chosen.markers[PR_event$interest,], 1, function(x) (x - mean(x))/ sd(x) ))
      }
      else{
        df <- chosen.markers[PR_event$interest,]
      }

      if(input$how_selec == 2){
        annot_row <- data.frame(ord = PR_event$proj[order(PR_event$proj$new_t),"new_t"],
                               pred = ggd[PR_event$interest,"pred"])
        rownames(annot_row) = PR_event$interest

        p <- c("0 - 0.15" = "blue2", "0.15 - 0.3" = "lightblue",
                  "0.3 - 0.7" = "grey80", "0.7 - 0.85" = "pink",
                  "0.85 - 1" = "red2")         #always use a double for annotation color and not list !! otherwise, bug with grid call
        k <- 1
        for(i in annot_row$pred){
          if(i < 0.15){
            n <- "0 - 0.15"
          }
          else if(i < 0.3){
            n <- "0.15 - 0.3"
          }
          else if(i < 0.7){
            n <- "0.3 - 0.7"
          }
          else if(i < 0.85){
            n <- "0.7 - 0.85"
          }
          else{
            n <- "0.85 - 1"
          }
          annot_row$pred[k] <- n
          k <- k+1
        }
        mycolors <- list(ord = c("white", "green"),
                         pred = p[unique(annot_row$pred)])
        annot_row$pred <- factor(annot_row$pred, levels = c("0 - 0.15", "0.15 - 0.3", "0.3 - 0.7", "0.7 - 0.85", "0.85 - 1"))
        a <- pheatmap(df, cluster_rows = FALSE, annotation_row = annot_row,
                      annotation_colors = mycolors, silent = TRUE)
      }
      else{
        a <- pheatmap(df, cluster_rows = FALSE, silent = TRUE)
      }
      a <- as.ggplot(a)
    }
    else{
      a <- NULL
    }
    a
  })

  output$HP <- renderPlot({
    HP()
  })

  output$downheatmap <- downloadHandler(
    filename = function(){
      paste0("Heatmap", Sys.Date(), ".png")
    },
    content = function(file){
      ggsave(file, HP(), device = "png")
    }
  )


}

shinyApp(ui, server)
