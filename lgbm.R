#' lightgbm_model
#'
#' Add the rest of your documentation here.
#' Typically this includes a "Specials" section
#'
#' @export
lgbm <- function(formula, ...) {
  # Create a model class which combines the training method, specials, and data checks
  model_lgbm <- new_model_class("lgbm",
                                   # The training method (more on this later)
                                   train = train_lgbm,
                                   # The formula specials (the next section)
                                   specials = specials_lgbm,
                                   # Any checks of the unprocessed data, like gaps, ordered, regular, etc.
                                   check = function(.data) {
                                     if (!tsibble::is_regular(.data)) stop("Data must be regular")
                                   }
  )

  # Return a model definition which stores the user's model specification
  new_model_definition(model_lgbm, {{formula}}, ...)
}

specials_lgbm <- new_specials(
  hyperparameters = function(hyperparameters = list()) {
    return(hyperparameters)
  },
  common_xregs,
  xreg = special_xreg(),
  .required_specials = c("hyperparameters","xreg"),
  .xreg_specials = names(common_xregs),
)

train_lgbm <- function(.data, specials, ...){
  mv <- tsibble::measured_vars(.data)
  y <- .data[[mv]]
  xreg <- specials$xreg[[1]]
  xreg <- xreg[,-1] %>% as.matrix()

  N = length(y)

  # define parameters for lgbm model
  params = specials$hyperparameters[[1]]

  # get training data
  train_x <- xreg %>%
    head(ceiling(0.9*N))
  train_y <- y %>%
    head(ceiling(0.9*N))

  # get test data
  test_x <- xreg %>%
    tail(N-ceiling(0.9*N))
  test_y <- y %>%
    tail(N-ceiling(0.9*N))

  dtrain = lgb.Dataset(train_x, label = train_y)
  dtest = lgb.Dataset.create.valid(dtrain, test_x, label = test_y)

  # validataion data
  valids = list(test = dtest)

  # train model
  mdl = lgb.train(
    params = params
    , data = dtrain
    , valids = valids
    , verbose = -1
  )

  # Compute fitted values and residuals
  fit <- predict(mdl, xreg)
  e <- y - fit

  # Create S3 model object
  # It should be small, but contain everything needed for methods below
  structure(
    list(
      model = mdl,
      n = length(y),
      y_name = mv,
      hyperparameters = list(),
      fitted = fit,
      residuals = e,
      sigma2 = var(e, na.rm = TRUE)
    ),
    class = "model_lgbm"
  )
}

#' @importFrom fabletools forecast
#' @export
forecast.model_lgbm <- function(object, new_data, specials = NULL, bootstrap = FALSE,
                                   approx_normal = TRUE, times = 5000, ...){

  # Get xreg
  xreg <- specials$xreg[[1]]
  xreg <- xreg[,-1] %>% as.matrix() %>% t()
  # Extract required parameters
  model <- object$model
  # Create forecast distributions
  sigma <- 0
  mu <- predict(model, xreg)
  distributional::dist_normal(mu, sigma)
}

#' @importFrom fabletools fitted
#' @export
fitted.model_lgbm <- function(object, ...){
  object$fitted
}

#' @importFrom fabletools residuals
#' @export
residuals.model_lgbm <- function(object, ...){
  object$residuals
}
