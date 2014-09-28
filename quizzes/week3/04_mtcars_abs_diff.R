library(datasets)
data(mtcars)

## get hp column of elements filtered by number of cylinders
filtered4Cyl <- mtcars[mtcars[, "cyl"] == 4, "hp"]
filtered8Cyl <- mtcars[mtcars[, "cyl"] == 8, "hp"]

avg4 <- mean( filtered4Cyl )
avg8 <- mean( filtered8Cyl )

res <- abs( avg4 - avg8 )
res
