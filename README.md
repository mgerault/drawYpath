# drawYpath
A shiny app for selecting points by drawing a path.

By simply running runDrawpath() after you loaded drawYpath with library(drawYpath), a shiny app will open; in it you can load a plotly graph
on which you can draw path/line.  The aim of this app is to manually find trajectories in the data instead of using classical algorithms.
The idea was from Samuel Granjeaud.

So after drawing a path on the plot, the closest points from this path will be selected and a heatmap from those points will be printed.

The package still use data on its own, but the aim will be for the user to load any data.

# How to install it
First you'll need to install [EBImage](https://bioconductor.org/packages/release/bioc/html/EBImage.html) from Bioconductor.
Indeed, to avoid a slow calculation when a lot of points are print on the plotly graph, it's better to print the plotly graph of the image of the plot and not
directly plot the data. To do so run : 

*> if(!requireNamespace("BiocManager", quietly = TRUE)){*

*> install.packages("BiocManager")*  

*> }*

*> BiocManager::install("EBImage")*  

Then you can install and load drawYpath by running : 

*> if(!requireNamespace("devtools", quietly = TRUE)){*

*> install.packages("devtools")*  

*> }*  

*> devtools::install_github("mgerault/drawYpath")*

*> library(drawYpath)*

If you want now to use the app just type :

*> runDrawpath()*
