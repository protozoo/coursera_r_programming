library(datasets)
data(iris)
## get Sepal.Length column of elements filtered by Species==virginica
filtered <- iris[iris[, "Species"] == "virginica", "Sepal.Length"]

## Compute mean
mean( filtered )

