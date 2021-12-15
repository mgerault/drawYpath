load("data-raw/gbm_1.RData")  ### all data


library(EBImage)
library(ggplot2)
g <- ggplot(ggd, aes(tSNE2, tSNE1, col = pred)) + geom_point(pch = 19, size = .7) +
  scale_color_steps2(breaks = c(0, 0.15, 0.3, 0.7, 0.85, 1), low = "blue2", high = "red2",
                     mid = "grey80", midpoint = 0.5) +
  theme_light() + coord_fixed(ratio = 1)

img = readImage("data-raw/originalplot.png")   #read image from the plot, a 524 x 524 image
d = dim(img@.Data)

#contour <- array(rep(1, 524*524*3), dim = c(524,524,3))
coord_contour <- as.data.frame(matrix(nrow = 0, ncol = 2))
for(i in 1:d[1]){
  for(k in 1:d[2]){
    is_b <- img@.Data[i,k,]
    if(sum(is_b*255) == 179*3){
      #contour[i,k,] <- rep(0,3)
      coord_contour <- rbind(coord_contour, c(i,k))
    }
  }
}
colnames(coord_contour) <- c("x", "y")
#contour <- Image(contour, colormode = "Color")

xmin_c = min(coord_contour$x) #54
xmax_c = max(coord_contour$x) #434
ymin_c = min(coord_contour$y) #49
ymax_c = max(coord_contour$y) #441

### need to reverse for plotting
### BEWARE !!!! plotly starts coordinates to 0 so need to add one
### line is 1 pixel thickness
# so when getting data from plotly, need strictly sup than 57 (5 pixels length ticks)
# and stricly inf than 433, for y it's 48 and 438 (3 pixels length ticks)

scale_imgplot <- data.frame(x_img = c(xmin_c - 1 + 4, xmax_c - 1),
                            y_img = c(ymax_c - 1 - 2, ymin_c - 1),   # plotly reverse y axes !
                            x_plot = ggplot_build(g)$layout$panel_params[[1]]$x.range,
                            y_plot = ggplot_build(g)$layout$panel_params[[1]]$y.range)


usethis::use_data(ggd, overwrite = TRUE)
usethis::use_data(chosen.markers, overwrite = TRUE)
usethis::use_data(img, overwrite = TRUE)
usethis::use_data(scale_imgplot, overwrite = TRUE)
