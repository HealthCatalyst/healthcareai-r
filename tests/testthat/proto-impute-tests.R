d <- data.frame(x = c(3, -3, NA, 0), y = c("A", "A", "B", NA))
imputeDF(d)

target <- list(df = data.frame(x = c(3, -3, 1, 0), y = c("A", "A", "B", "C")),
               imputedVals = list(y = "C", x = 0))

# Should work as before
imputeDF(d)
imputeDF(d, list())

# Should produce target:
imputeDF(d, list(0, "C"))
imputeDF(d, list(y = "C", x = 0))
imputeDF(d, list(y = "C"))

# Should produce target with warning:
imputeDF(d, list("z" = 5, y = "C"))
imputeDF(d, list(y = "C", "z" = 5))

# Should error:
imputeDF(d, list("C"))
