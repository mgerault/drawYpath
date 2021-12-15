#' coord_imgtoplt
#'
#' A function to get back coordinates from the original plot according
#' the coordinates from the image of the plot. You'll need to know
#' what are the coordinates on the image of the axes.
#'
#' @param data A data frame that contains the coordinates of the points on the image
#' @param scale A data frame of 2 rows and 4 columns built as follow :
#'             'x_img' is the column that contains x range on the image that correspond to the x axes of the plot
#'             'x_plot' is the column that contains range of the x axes of the plot
#'             'y_img' is the column that contains y range on the image that correspond to the y axes of the plot
#'             'y_plot' is the column that contains range of the y axes of the plot
#'
#' @return The data rescaled on the original plot
#'
#' @export

coord_imgtoplt <- function(data, scale){
  coef = sweep(scale[2,], 2, scale[1,], "-")
  coef = c(coef$x_plot/coef$x_img, coef$y_plot/coef$y_img)  #need to preserve same distance ratio

  in_plt <- which((data[,1] > scale$x_img[1] & data[,1] < scale$x_img[2])&  #check if points are out of the axes range (the image also contains coordinates in the margin)
                    (data[,2] > scale$y_img[2] & data[,2] < scale$y_img[1])
                  )
  data <- data[in_plt,]

  if(nrow(data) == 0){
    data <- data.frame(x = c(1e5, 1e6), y = c(1e5, 1e6))
    return(data)
  }
  else{
    data[,1] <- coef[1]*(data[,1] - scale$x_img[1]) + scale$x_plot[1]
    data[,2] <- coef[2]*(data[,2] - scale$y_img[1]) + scale$y_plot[1]
    return(data)
  }
}
