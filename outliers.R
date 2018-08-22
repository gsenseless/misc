### Density based outlier detection which might be used for sales data.
require(dbscan)
detectOutliers <- function(vector) {
  if (length(vector) > 1500) {
    print(paste("huge vector"))
    stop() # return(1)
  }
  distMatrix <- dist(vector)
  clusters <- dbscan(x = distMatrix
                     ,eps = as.vector(quantile(vector, probs = .95) + 1e-15)
                     ,minPts = 2)
  clusters$cluster[clusters$cluster != 1] <- 0
  return(clusters$cluster)
  # return(cbind(vector, clusters$cluster))
}

### examples
checkOutliers <- function(vector) {
  summary(vector)
  boxplot(vector)
  Sys.sleep(1)
  plot(vector, type = 'h', ylim = c(0, max(vector)))
  outliers <- detectOutliers(vector = vector)
  if (all(outliers == 1)) {
    print("no outliers found")
  } else {
    Sys.sleep(1)
    lines(x = vector[], col = outliers + 2, type = 'h')
  }
}

data(treering); treering <- treering[1:300]
checkOutliers(treering)

data(treering); treering <- treering[1:300]
### lets manually input some outliers
treering[sample(x = 1:length(treering), size = 1)] <- max(treering) * 2
checkOutliers(treering)

data(treering); treering <- treering[1:300]
### lets manually input some outliers
treering[sample(x = 1:length(treering), size = 3)] <- 
  c(max(treering) * 1.5, max(treering) * 2, max(treering) * 2.5)
checkOutliers(treering)

data(treering); treering <- treering[1:300]
### lets manually input some outliers
treering[sample(x = 1:length(treering), size = 2)] <- 
  c(max(treering) * 2, max(treering) * 2.5)
checkOutliers(treering)