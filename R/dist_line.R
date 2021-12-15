#' dist_line
#'
#' Calculate distance between a line and set of points; several lines = path
#'
#' @param points A data frame that contains the coordinates of the points
#' @param lines A data frame that contains the coordinates of the points that defines the path
#'
#' @return A list of 3 elements. 'dist' is the minimum distance between each points and the lines,
#'         'proj' is the projection from the point on the line,
#'         'which_line' is the index of the line where the point is projected.
#'
#' @export


dist_line <- function(points, lines){
  dist_p <- list("dist" = list(), "proj" = list())     #set list for final output
  dst <- data.frame(matrix(Inf, nrow = nrow(points), ncol = nrow(lines))) #set data frame for all distances
  for(k in 1:(nrow(lines) - 1)){
    B <- as.double(lines[k,])
    C <- as.double(lines[k+1,])

    hyp <- dist(t(matrix(c(B,C), nrow = 2)))   #get length of segment = hypothenuse
    cosa <- (C[1] - B[1])/hyp                  #then, only trigonometry (adj/hyp; opp/hyp)
    sina <- (B[2] - C[2])/hyp
    Tx <- -B[1]                                #get translation from new space to origin
    Ty <- -B[2]
    Txf <- Tx*cosa - Ty*sina                   #do rotation first, so combination between translation and rotation
    Tyf <- Tx*sina + Ty*cosa

    RT <- matrix(c(cosa, sina, -sina, cosa, Txf, Tyf), ncol = 3)  #origin space to new one (rotation, then translation)
    TR <-  matrix(c(cosa, -sina, sina, cosa, -Tx, -Ty), ncol = 3)  #new space to origin (translation, then rotation)

    points$dim3 <- rep(1, nrow(points))      # get all projected points in new space (define by segment)
    newM <- RT %*% t(points)
    newM <- t(newM)
    points$dim3 <- NULL

    dp = matrix(Inf, nrow = nrow(points), ncol = 3)         #set matrix for all results
    ok = newM[,1] < 0 #if point out of segment, project on B or C according placement
    dp[ok,1] <- sqrt(rowSums(sweep(points[ok,], 2, B, "-")^2))
    dp[ok, 2:3] <- B
    ok2 = newM[,1] > hyp
    dp[ok2,1] <- sqrt(rowSums(sweep(points[ok2,], 2, C, "-")^2))
    dp[ok2, 2:3] <- C
    ok <- ok + ok2 == 0                                            #else, use projection
    dp[ok,1] <- abs(newM[ok,2])
    mat <- c(newM[ok,1], rep(0, sum(ok)), rep(1, sum(ok)))
    mat <- matrix(mat, ncol = 3)
    dp[ok,2:3] <- t(TR %*% t(mat))                          #get back projection in origin space

    dst[,k] <- dp[,1]
    dist_p$proj[[k]] <- dp[,2:3]
  }
  dst_idx <- apply(dst, 1, which.min)    #get min distance
  pj <- dist_p$proj
  dist_p$proj <- list()
  for(i in 1:nrow(points)){
    dist_p$dist[[i]] <- dst[i,dst_idx[i]]
    dist_p$proj[[i]] <- pj[[dst_idx[i]]][i,]
  }
  dist_p$which_line <- dst_idx           #get line where the point is projected
  return(dist_p)
}
