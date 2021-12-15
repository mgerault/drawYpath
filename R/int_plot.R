#' int_plot
#'
#' Plot an interactive graph of the direct data if the number of points to print is
#' not to high, else it plot the image of the plot.
#'
#' @param data A data frame that contains the data. For now, needs to have 'tSNE1', 'tSNE2', 'pred'
#'             and 'index' columns. The 'tSNE' axes are the y and x axes respectively, 'pred' is the color
#'             and 'index' is the row names.
#' @param img The image of the plot. Need to be an 'Image' object from \code{EBImage} package
#' @param tit The title of the plot
#' @param src The source of the plotly plot, default is "M"
#' @param max_points The maximum number of points you want to print before ploting the image of the plot
#'
#' @return An interactive plot from plotly on which can draw path
#'
#' @export

int_plot <- function(data, img, tit = "t-SNE", src = "M", max_points = 3000){
  df <- data

  if(nrow(df) > max_points){
    img@.Data <- aperm(img@.Data, c(2,1,3))    #image of the plot, plotly reverse the axes, so need to permute them
    p <- plotly::plot_ly(type = "image", z = img*255,
                 hoverinfo = "text",
                 source = src) %>%
      plotly::layout(xaxis = list(visible = FALSE),
             yaxis = list(visible = FALSE),
             margin = list(t = 90, b = 90), dragmode = "drawopenpath",
             title = list(text = tit,
                          font = list(size = 25,
                                      family = "Times New Roman"))
      ) %>%
      plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape", "drawline"))
  }
  else{
    p <- plotly::plot_ly(type = "scatter", mode = "markers",
                 data = df,
                 x = ~tSNE2, y = ~tSNE1,
                 color = ~pred, size = .7,
                 colors = c("blue2", "grey80", "red2"),
                 text = ~paste("t-SNE 1: ", tSNE1, '<br>t-SNE 2:', tSNE2, '<br>Pred:', pred),
                 customdata = ~index,
                 hoverinfo = "text", source = src) %>%      #add source argument in order to differentiate plotly plot
      plotly::layout(title = list(text = tit,
                          font = list(size = 25,
                                      family = "Times New Roman")
      ),
      margin = list(t = 90, b = 90),
      dragmode = "drawopenpath",         #add possibility to draw path
      yaxis = list(scaleratio = 1)) %>%
      plotly::config(modeBarButtonsToAdd = list("drawopenpath", "eraseshape", "drawline")  #add possibility to draw path
      )
  }

  return(p)
}
