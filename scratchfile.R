# build hot dog set
n = 300
df <- data.frame(id = 1:n,
                 vendorID = sample(1:9, size = n, replace = T),
                 length = rnorm(n, mean = 7, sd = 2),
                 diameter = rnorm(n, mean = 2, sd = 0.5), 
                 heat = sample(c("Cold", "Hot"), size = n, replace = T),
                 condiment = sample(c("Ketchup", "Mustard", "Wasabi", "Syrup"), 
                                    size = n, replace = T)
)

# give hotdog likeliness score
df['hotDogScore'] <- df['length'] - 2*df['diameter'] - 1
df$hotDogScore[df['heat'] == "Hot"]  = df$hotDogScore[df['heat'] == "Hot"] + 1
df$hotDogScore[df['heat'] == "Cold"]  = df$hotDogScore[df['heat'] == "Cold"] - 1
df$hotDogScore[df['condiment'] == "Ketchup"] <- df$hotDogScore[df['condiment'] == "Ketchup"] + 1
df$hotDogScore[df['condiment'] == "Mustard"] <- df$hotDogScore[df['condiment'] == "Mustard"] + 2
df$hotDogScore[df['condiment'] == "Wasabi"] <- df$hotDogScore[df['condiment'] == "Wasabi"] - 1
df$hotDogScore[df['condiment'] == "Syrup"] <- df$hotDogScore[df['condiment'] == "Syrup"] - 4


# Add noise
df$hotDogScore <- df$hotDogScore + rnorm(n, mean = 0, sd = 1.25)
df$hotDogScore <- ifelse(df$hotDogScore > 0, "Y", "N")

# Add missing data
df$condiment[sample(1:n, 32, replace = FALSE)] <- NA
df$length[sample(1:n, 51, replace = FALSE)] <- NA
df$heat[sample(1:n, 125, replace = FALSE)] <- NA
df$diameter[sample(1:n, 9, replace = FALSE)] <- NA

train_index <- caret::createDataPartition(
  df$hotDogScore,
  p = 0.8,
  times = 1,
  list = TRUE)

d_train <- df[train_index$Resample1, ]
d_test <- df[-train_index$Resample1, ]

rec_obj <- recipe(hotDogScore ~ ., data = d_train)





rec_obj2 <- rec_obj %>%
  step_bagimpute(all_predictors(), impute_with = imp_vars(all_predictors()))
rec_obj2

prepped2 <- rec_obj2 %>%
  prep(training = d_train) 

d_imputed2 <- prepped2 %>%
  bake(newdata = d_test)
countMissingData(d_test)
countMissingData(d_imputed2)







rec_obj3 <- rec_obj %>%
  hcai_impute(numeric_method = "bagimpute",
    numeric_params = list(impute_with = imp_vars(all_predictors())))
rec_obj3

d_imputed3 <- rec_obj3 %>%
  prep(training = d_train) %>%
  bake(newdata = d_test)
countMissingData(d_imputed3)
