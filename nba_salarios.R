library(readr)
library(glmnet)
library(rsample)
library(dplyr)


nba <- read.csv('./nba.csv')
# limpieza de datos
nba <- na.omit(nba)

# creacion de muestra training y test
set.seed(123)
nba_split <- initial_split(nba, prop = .7, strata = "Salary")
nba_train <- training(nba_split)
nba_test  <- testing(nba_split)

# Utilizamos logaritmos para el salario
nba_train_x <- model.matrix(Salary ~ ., nba_train)[, -1]
nba_train_y <- log(nba_train$Salary) 

nba_test_x <- model.matrix(Salary ~ ., nba_test)[, -1]
nba_test_y <- log(nba_test$Salary)


# Elastic net
elastic1 <- glmnet(nba_train_x, nba_train_y, alpha = 0.25) 
elastic2 <- glmnet(nba_train_x, nba_train_y, alpha = 0.75) 

# Grafico lambda
plot(elastic1, xvar = "lambda", main = "Elastic Net (Alpha = .25)\n\n\n")
plot(elastic2, xvar = "lambda", main = "Elastic Net (Alpha = .75)\n\n\n")

# Creacion de tabla mse y lambda
fold_id <- sample(1:10, size = length(nba_train_y), replace=TRUE)

tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .1),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)
knitr::(tuning_grid)

for(i in seq_along(tuning_grid$alpha)) {
  
  # generamos el modelo por cada alpha
  fit <- cv.glmnet(nba_train_x, nba_train_y, alpha = tuning_grid$alpha[i], foldid = fold_id)
  
  # obtenemos valores mse y lambda
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
}

knitr::(tuning_grid)

# elegiremos el modelo de menor lambda



