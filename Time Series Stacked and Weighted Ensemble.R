#Alistair So-Schoos
#Time Series Model Building---- 
#Domo-enhanced version
#04-08-2021

###############################################################################
#                                                                              #
#                   Modeltime Stacked Ensemble Process                         #
#                                                                              #
# Dependencies: Parochial Data in Long Form for Time Series                    #
#                                                                              #
###############################################################################


#Modeltime Deployment 

# LIBRARIES ----

# Time Series Machine Learning
library(tidymodels)
library(modeltime)
library(modeltime.ensemble)

# EDA
library(DataExplorer)
library(dplyr)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core
library(tidyverse)
library(timetk)
library(lubridate)
library(readxl)
library(rstan)



# * Parallel Processing ----

registerDoFuture()
n_cores <- parallel::detectCores()
plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)

# 1.0 Data Prep ----

#Continuing from parochial data wide to long format.R

sample_closed_parish2019 <- sample_2019_closed_parish


sample_closed_parish2019  %>% glimpse()

# * Summary Diagnostics ----

sample_closed_parish2019 %>% tk_summary_diagnostics(.date_var = liturgy_date)

# * Visualizations ----

worship_trend <-  sample_closed_parish2019 %>% 
    plot_time_series(liturgy_date_week, attendance)

# 2.0 Determining Evaluation Period ----

# * Filtering ----
worship_evaluation_tbl <- sample_closed_parish2019  %>% 
    filter_by_time(
        .date_var = liturgy_date_week, 
        .start_date = "2005-04-10" , 
        .end_date = "2019-12-31")

worship_evaluation_tbl %>% 
    plot_time_series(liturgy_date_week, attendance, 
                     .smooth = FALSE,
                     .title = "Time Series of Attendance",
                     .x_lab = "Time",
                     .y_lab = "Attendance")

?plot_time_series

# * Split into Train/Test ----

splits <- worship_evaluation_tbl %>% 
    time_series_split(
        date_var = liturgy_date,
        initial = "10 years",
        assess = "3 years",
        cumulative = TRUE,
        lag = "1 year"
    ) 

splits %>% 
    tk_time_series_cv_plan() %>% 
    plot_time_series_cv_plan(liturgy_date, attendance,
                             .smooth = FALSE,
                             .title = "Time Series of Attendance",
                             .x_lab = "Time",
                             .y_lab = "Attendance")

# 3.0 PROPHET FORECASTING ----

# * Prophet Model using Modeltime/Parsnip ----

model_prophet_fit <- prophet_reg() %>% 
    set_engine("prophet") %>% 
    fit(attendance ~ liturgy_date, data = training(splits))

model_prophet_fit


# * Modeltime Process ----

model_tbl <- modeltime_table(
    model_prophet_fit
)


# * Calibration ----
calibration_tbl <- model_tbl %>% 
    modeltime_calibrate(
        new_data = testing(splits)
    )


# * Visualize Forecast ----
calibration_tbl %>% 
    modeltime_forecast(actual_data = worship_evaluation_tbl) %>% 
    plot_modeltime_forecast()

# * Get Accuracy Metrics ----
calibration_tbl %>% 
    modeltime_accuracy()


# 4.0 FORECASTING WITH FEATURE ENGINEERING ----

# * Identify Possible Features ----

worship_evaluation_tbl %>% 
    plot_seasonal_diagnostics(liturgy_date, log(attendance))

# * Recipes Spec ----

training(splits)

recipe_spec <- recipe(attendance ~ liturgy_date, data = training(splits)) %>% 
    
    # Time Series Signature
    step_timeseries_signature(liturgy_date) %>% 
    
    step_rm(ends_with(".iso"), ends_with(".xts"),
            contains("hour"), contains("minute"), contains("second"),
            contains("am.pm")
    ) %>% 
    step_normalize(ends_with("index.num"),
                   ends_with("_year")
    ) %>% 
    step_dummy(all_nominal())

recipe_spec %>% prep() %>% juice() %>% glimpse()

# * Machine Learning Specs ----

model_spec <- linear_reg() %>% 
    set_engine("lm")

workflow_fit_lm <- workflow() %>% 
    add_model(model_spec) %>% 
    add_recipe(recipe_spec) %>% 
    fit(training(splits))

workflow_fit_lm 

# *Modeltime Process ----

calibration_tbl <- modeltime_table(
    model_prophet_fit,
    workflow_fit_lm
) %>% 
    modeltime_calibrate(testing(splits))

calibration_tbl %>% modeltime_accuracy()

calibration_tbl %>% 
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = worship_evaluation_tbl
    ) %>% 
    plot_modeltime_forecast()


# * Parsnip Model (ARIMA)  ----

training(splits)

model_fit_arima <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(attendance ~ liturgy_date, data = training(splits))

model_fit_arima

# * Workflow (ARIMA + Date Features) ----

model_spec_arima <- arima_reg() %>% 
    set_engine("auto_arima")

recipe_spec_fourier <- recipe(attendance ~ liturgy_date, data = training(splits)) %>% 
    step_fourier(liturgy_date, period = c(7, 14, 30, 90), K = 1)

recipe_spec_fourier %>% prep() %>% juice() %>% glimpse()

workflow_fit_arima <- workflow() %>% 
    add_recipe(recipe_spec_fourier) %>% 
    add_model(model_spec_arima) %>% 
    fit(training(splits))

workflow_fit_arima 

# *Modeltime Process ----

calibration_tbl <- modeltime_table(
    model_prophet_fit,
    workflow_fit_lm,
    workflow_fit_arima
) %>% 
    modeltime_calibrate(testing(splits))

calibration_tbl %>% modeltime_accuracy()

calibration_tbl %>% 
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = worship_evaluation_tbl
    ) %>% 
    plot_modeltime_forecast()

# AUTO ARIMA + XREGS ----

# * Model ----

model_fit_auto_arima <- arima_reg() %>% 
    set_engine("auto_arima") %>% 
    fit(
        attendance ~ liturgy_date,
        data = training(splits)
    )

# * Calibrate ----

calibration_tbl <- modeltime_table(
    model_prophet_fit,
    workflow_fit_lm,
    workflow_fit_arima,
    model_fit_auto_arima
) %>% 
    modeltime_calibrate(testing(splits))

# * Forecast Test ----

calibration_tbl %>% 
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = worship_evaluation_tbl
    ) %>% 
    plot_modeltime_forecast()

# * Accuracy Test -----
calibration_tbl %>% modeltime_accuracy()

# 5.0 Function to calibrate & plot ----

calibrate_and_plot <-
    function(..., type = "testing") {
        
        if (type == "testing") {
            new_data <- testing(splits)
        } else {
            new_data <- training(splits) %>% drop_na()
        }
        
        calibration_tbl <- modeltime_table(...) %>%
            modeltime_calibrate(new_data)
        
        print(calibration_tbl %>% modeltime_accuracy())
        
        calibration_tbl %>%
            modeltime_forecast(
                new_data = new_data,
                actual_data = worship_evaluation_tbl
            ) %>%
            plot_modeltime_forecast(.conf_interval_show = FALSE)
        
    }

# 6.0 RANDOM FOREST ----

model_spec_rf <- rand_forest(
    mode = "regression", 
    mtry = 25, 
    trees = 1000, 
    min_n = 25
) %>%
    set_engine("randomForest")

# Spline

set.seed(123)
wflw_fit_rf_spline <- workflow() %>%
    add_model(model_spec_rf) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits))

calibrate_and_plot(
    wflw_fit_rf_spline,
    type = "testing"
)

# * Lag Recipe ----

recipe_spec %>% prep() %>% juice() %>% glimpse()

recipe_spec_lag <- recipe_spec %>%
    step_rm(liturgy_date) %>%
    step_naomit(starts_with("lag_"))

recipe_spec_lag %>% prep() %>% juice() %>% glimpse()


# Lag

wflw_fit_rf_lag <- wflw_fit_rf_spline %>%
    update_recipe(recipe_spec_lag) %>%
    fit(training(splits))


# Calibrate & Plot

calibrate_and_plot(
    wflw_fit_rf_spline,
    wflw_fit_rf_lag
)

# * Lag Workflow ----

workflow_fit_lm_2_lag <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe_spec_lag) %>%
    fit(training(splits))

workflow_fit_lm_2_lag

workflow_fit_lm_2_lag %>% pull_workflow_fit() %>% pluck("fit") %>% summary()

# * Compare with Modeltime -----

calibration_tbl <- modeltime_table(
    workflow_fit_lm,
    workflow_fit_lm_2_lag
) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
    modeltime_forecast(new_data    = testing(splits), 
                       actual_data = worship_evaluation_tbl) %>%
    plot_modeltime_forecast()

calibration_tbl %>%
    modeltime_accuracy()


# 7.0 ELASTIC NET REGRESSION ----

model_spec_glmnet <- linear_reg(
    mode = "regression",
    penalty = 0.01,
    mixture = 0
) %>%
    set_engine("glmnet")

model_spec_glmnet

# Spline

wflw_fit_glmnet_spline <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_1) %>%
    fit(training(splits))


# Lag

wflw_fit_glmnet_lag <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec_lag) %>%
    fit(training(splits))


# Calibrate & Plot

calibrate_and_plot(
    wflw_fit_glmnet_spline,
    wflw_fit_glmnet_lag,
    type = "testing"
)

# 8.0 XGBOOST ----

model_spec_boost <- boost_tree(
    mode = "regression",
    mtry = 25, 
    trees = 1000, 
    min_n = 2, 
    tree_depth = 12, 
    learn_rate = 0.3, 
    loss_reduction = 0
) %>%
    set_engine("xgboost")

# Spline

set.seed(123)
wflw_fit_xgboost_spline <- workflow() %>%
    add_model(model_spec_boost) %>%
    add_recipe(recipe_spec_1) %>%
    fit(training(splits))


# Lag
set.seed(123)
wflw_fit_xgboost_lag <- wflw_fit_xgboost_spline %>%
    update_recipe(recipe_spec_lag) %>%
    fit(training(splits))

# Calibrate & Plot

calibrate_and_plot(
    wflw_fit_xgboost_spline,
    wflw_fit_xgboost_lag,
    type = "testing"
)

# 9.0 CUBIST ----

model_spec_cubist <- cubist_rules(
    committees = 50, 
    neighbors = 7, 
    max_rules = 100
) %>%
    set_engine("Cubist")

# Spline

set.seed(123)
wflw_fit_cubist_spline <- workflow() %>%
    add_model(model_spec_cubist) %>%
    add_recipe(recipe_spec_1) %>%
    fit(training(splits))

# Lag
set.seed(123)
wflw_fit_cubist_lag <- wflw_fit_cubist_spline %>%
    update_recipe(recipe_spec_lag) %>%
    fit(training(splits))


# Calibrate & Plot

calibrate_and_plot(
    wflw_fit_cubist_spline,
    wflw_fit_cubist_lag
)

# 10.0 NEURAL NET ----

model_spec_nnet <- mlp(
    mode = "regression",
    hidden_units = 10,
    penalty = 1, 
    epochs = 100
) %>%
    set_engine("nnet")

# Spline

set.seed(123)
wflw_fit_nnet_spline <- workflow() %>%
    add_model(model_spec_nnet) %>%
    add_recipe(recipe_spec_1) %>%
    fit(training(splits))

# Lag

set.seed(123)
wflw_fit_nnet_lag <- wflw_fit_nnet_spline %>%
    update_recipe(recipe_spec_lag) %>%
    fit(training(splits))

# Calibrate & Plot

calibrate_and_plot(
    wflw_fit_nnet_spline,
    wflw_fit_nnet_lag,
    type = "testing"
)

# 11.0 NNETAR ----

model_spec_nnetar <- nnetar_reg(
    non_seasonal_ar = 2,
    seasonal_ar     = 1, 
    hidden_units    = 10,
    penalty         = 10,
    num_networks    = 10,
    epochs          = 50
) %>%
    set_engine("nnetar")


set.seed(123)
wflw_fit_nnetar_base <- workflow() %>%
    add_model(model_spec_nnetar) %>%
    add_recipe(recipe_spec) %>%
    fit(training(splits) %>% drop_na())

# Calibrate & Plot

calibrate_and_plot(
    wflw_fit_nnetar_base
)

# 12.0 MODELTIME TABLE ----
# - Organize

model_tbl <- modeltime_table(
    model_prophet_fit,
    workflow_fit_lm,
    model_fit_arima,
    workflow_fit_arima,
    wflw_fit_rf_lag,
    wflw_fit_rf_spline,
    workflow_fit_lm_2_lag,
    wflw_fit_glmnet_lag,
    wflw_fit_glmnet_spline,
    wflw_fit_xgboost_lag,
    wflw_fit_xgboost_spline,
    wflw_fit_cubist_lag,
    wflw_fit_cubist_spline,
    wflw_fit_nnet_lag,
    wflw_fit_nnet_spline,
    wflw_fit_nnetar_base
) %>% 
    update_model_description(2, "LM - Spline Recipe")%>% 
    update_model_description(5, "RANDOMFOREST - Lag Recipe")%>% 
    update_model_description(6, "RANDOMFOREST - Spline Recipe")%>% 
    update_model_description(7, "LM - Lag Recipe") %>% 
    update_model_description(8, "GLMNET - Lag Recipe")%>% 
    update_model_description(9, "GLMNET - Spline Recipe")%>% 
    update_model_description(10, "XGBOOST - Lag Recipe")%>% 
    update_model_description(11, "XGBOOST - Spline Recipe")%>% 
    update_model_description(12, "CUBIST - Lag Recipe") %>% 
    update_model_description(13, "CUBIST - Spline Recipe") %>% 
    update_model_description(14, "NNET - Lag Recipe") %>% 
    update_model_description(15, "NNET - Spline Recipe")

model_tbl 


# 13.0 CALIBRATION ----

calibration_tbl_new <- model_tbl %>% 
    modeltime_calibrate(new_data = testing(splits))

# 14.0 Testing Accuracy ----
calibration_tbl_new %>% 
    modeltime_accuracy()

# Table Modeltime Accuracy

calibration_tbl_new %>% 
    modeltime_accuracy(
        metric_set = default_forecast_accuracy_metric_set()
    ) %>% 
    table_modeltime_accuracy(
        .interactive = TRUE,
        bordered = TRUE,
        resizable = TRUE
    )

# Metric Sets

?default_forecast_accuracy_metric_set

metric_set(mae, rmse, iic)

calibration_tbl_new %>% 
    modeltime_accuracy(
        metric_set = metric_set(mae, rmse, iic)
    )

# 15.0 TEST FORECAST ----

calibration_tbl_new %>% 
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = worship_evaluation_tbl,
        conf_interval = 0.95
    ) %>% 
    plot_modeltime_forecast(
        .legend_max_width = 60,
        .legend_show = TRUE,
        .conf_interval_show = TRUE,
        .conf_interval_alpha = 0.10,
        .conf_interval_fill = "lightblue",
        .title = "Attendance Forecast"
    )

?plot_modeltime_forecast

# 16.0 REFITTING

# * Refit ----

refit_tbl <- calibration_tbl_new %>% 
    modeltime_refit(data = worship_evaluation_tbl)

refit_tbl


# Final Forecast ----

refit_tbl %>% 
    modeltime_forecast(
        h = "2 years",
        new_data = testing(splits),
        actual_data = worship_evaluation_tbl,
        conf_interval = 0.80
    ) %>% 
    plot_modeltime_forecast(
        .legend_max_width = 25,
        .conf_interval_fill = "lightblue",
        .interactive = FALSE
    )

# * Combine Tables & Organize ----

model_tbl <- combine_modeltime_tables(
    calibration_tbl_new
)

calibration_tbl_next <- model_tbl %>% 
    modeltime_calibrate(testing(splits))

calibration_tbl_next <- calibration_tbl_next %>% 
    modeltime_refit(
        data = training(splits) %>% drop_na()
    ) %>% 
    modeltime_calibrate(
        new_data = testing(splits)
    )

# * Sub-Model Selection ----

calibration_tbl_next %>% 
    modeltime_accuracy() %>% 
    table_modeltime_accuracy(
        defaultPageSize = 40,
        bordered = TRUE,
        resizable = TRUE
    )

model_id_calibration <- calibration_tbl_next %>% 
    modeltime_accuracy() %>% 
    arrange(rmse) %>% 
    filter(mae < 10) %>% 
    pull(.model_id)

submodels_tbl <- calibration_tbl_next %>% 
    filter(.model_id %in% model_id_calibration)

submodels_tbl

# 17.0 AVERAGE ENSEMBLES ----

?ensemble_average

# * Making an Average Ensemble ----

ensemble_fit_mean <- submodels_tbl %>% 
    ensemble_average(type = "mean")

modeltime_table(
    ensemble_fit_mean
) %>% 
    modeltime_accuracy(testing(splits))

# * Making a Median Model ----

ensemble_fit_median <- submodels_tbl %>%
    ensemble_average("median")

modeltime_table(
    ensemble_fit_mean,
    ensemble_fit_median
) %>%
    modeltime_accuracy(testing(splits))

# 18.0 WEIGHTED ENSEMBLES ----

?ensemble_weighted

loadings_tbl <- submodels_tbl %>%
    modeltime_accuracy() %>%
    mutate(rank = min_rank(-mae)) %>%
    select(.model_id, rank)

ensemble_fit_wt <- submodels_tbl %>%
    ensemble_weighted(loadings = loadings_tbl$rank)

ensemble_fit_wt$fit$loadings_tbl

modeltime_table(
    ensemble_fit_wt
) %>%
    modeltime_accuracy(testing(splits))

# 19.0 STACKING ----

# *** TSCV ----

resamples_tscv <- training(splits) %>%
    drop_na() %>%
    time_series_cv(
        assess     = "3 years",
        skip       = "2 years",
        initial    = "5 years",
        cumulative = TRUE
    )

resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(liturgy_date, attendance)


submodels_resamples_tscv_tbl <- submodels_tbl %>%
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(
            verbose   = TRUE, 
            # allow_par = FALSE, 
            allow_par = TRUE,
            pkgs      = c("Cubist", "rules")
        )
    )

submodels_resamples_tscv_tbl$.resample_results[[1]]$.predictions

# *** K-FOLD ----

set.seed(123)
resamples_kfold <- training(splits) %>%
    drop_na() %>%
    vfold_cv(v = 10)

resamples_kfold %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(liturgy_date, attendance, .facet_ncol = 2)

submodels_resamples_kfold_tbl <- submodels_tbl %>%
    dplyr::slice(-1) %>%
    modeltime_fit_resamples(
        resamples = resamples_kfold,
        control   = control_resamples(
            verbose    = TRUE, 
            allow_par  = TRUE,
            pkgs       = c("Cubist", "rules")
        )
    )

# * 19.2 LM STACK ----

# TSCV 
set.seed(123)
ensemble_fit_lm_tscv <- submodels_resamples_tscv_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg() %>% set_engine("lm"),
        control    = control_grid(verbose = TRUE)
    )

modeltime_table(
    ensemble_fit_lm_tscv
) %>%
    modeltime_accuracy(testing(splits))

# K-FOLD
set.seed(123)
ensemble_fit_lm_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg() %>% set_engine("lm"),
        control    = control_grid(verbose = TRUE)
    )

modeltime_table(
    ensemble_fit_lm_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 19.3 GLMNET STACK ----

set.seed(123)
ensemble_fit_glmnet_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = linear_reg(
            penalty = tune(),
            mixture = tune()
        ) %>% 
            set_engine("glmnet"),
        kfolds  = 10,
        grid    = 10,
        control = control_grid(
            verbose   = TRUE, 
            allow_par = TRUE
        )
    )

modeltime_table(
    ensemble_fit_glmnet_kfold
) %>%
    modeltime_accuracy(testing(splits))



# * 19.4 RANDOM FOREST STACK ----

set.seed(123)
ensemble_fit_ranger_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = rand_forest(
            trees = tune(),
            min_n = tune()
        ) %>%
            set_engine("ranger"),
        kfolds  = 10, 
        grid    = 10,
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

modeltime_table(
    ensemble_fit_ranger_kfold
) %>%
    modeltime_accuracy(testing(splits))


# * 19.5 NNET STACK ----

set.seed(123)
ensemble_fit_nnet_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = mlp(
            hidden_units = tune(),
            penalty      = tune(),
            epochs       = tune()
        ) %>% set_engine("nnet"),
        kfolds = 10, 
        grid   = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

modeltime_table(
    ensemble_fit_nnet_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 19.6 XGBOOST STACK ----
set.seed(123)
ensemble_fit_xgboost_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = boost_tree(
            trees          = tune(),
            tree_depth     = tune(),
            learn_rate     = tune(),
            loss_reduction = tune()
        ) %>%
            set_engine("xgboost"),
        kfolds = 10, 
        grid   = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )


modeltime_table(
    ensemble_fit_xgboost_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 19.7 CUBIST STACK ----

set.seed(123)
ensemble_fit_cubist_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = cubist_rules(
            committees = tune(), 
            neighbors  = tune(),
            max_rules  = tune()
        ) %>%
            set_engine("Cubist"),
        kfold = 10,
        grid  = 10, 
        control = control_grid(
            verbose = TRUE, 
            allow_par = TRUE
        )
    )

modeltime_table(
    ensemble_fit_cubist_kfold
) %>%
    modeltime_accuracy(testing(splits))

# * 19.8 SVM STACK ----

set.seed(123)
ensemble_fit_svm_kfold <- submodels_resamples_kfold_tbl %>%
    ensemble_model_spec(
        model_spec = svm_rbf(
            mode      = "regression",
            cost      = tune(),
            rbf_sigma = tune(),  
            margin    = tune()
        ) %>%
            set_engine("kernlab"),
        kfold = 10, 
        grid  = 10, 
        control = control_grid(verbose = TRUE, allow_par = TRUE)
    )

modeltime_table(
    ensemble_fit_svm_kfold
) %>%
    modeltime_accuracy(testing(splits))

# 20.0 MULTI-LEVEL STACK ----

model_stack_level_2_accuracy_tbl <- modeltime_table(
    ensemble_fit_glmnet_kfold,
    ensemble_fit_ranger_kfold,
    ensemble_fit_nnet_kfold,
    ensemble_fit_xgboost_kfold, 
    ensemble_fit_cubist_kfold,
    ensemble_fit_svm_kfold
) %>%
    modeltime_accuracy(testing(splits))

model_stack_level_2_accuracy_tbl %>% table_modeltime_accuracy()


model_stack_level_3_tbl <- modeltime_table(
    ensemble_fit_ranger_kfold,
    ensemble_fit_cubist_kfold,
    ensemble_fit_glmnet_kfold
) %>%
    ensemble_weighted(loadings = c(1, 3, 5)) %>%
    modeltime_table()

model_stack_level_3_tbl %>%
    modeltime_accuracy(testing(splits))

# 21.0 MODELTIME ----

# * Calibration  ----

calibration_ensemble_tbl <- model_stack_level_3_tbl %>% 
    modeltime_calibrate(testing(splits))

calibration_ensemble_tbl

# * Accuracy ----

calibration_ensemble_tbl %>% modeltime_accuracy()


# * Test Forecast ----

forecast_test_tbl <- calibration_ensemble_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = worship_evaluation_tbl
    )

forecast_test_tbl %>%
    plot_modeltime_forecast()

# * Refit Forecast ----

# Not updating the super learner / updating submodels only 

refit_ensemble_submodel_tbl <- calibration_ensemble_tbl %>%
    modeltime_refit(data = worship_evaluation_tbl)

forecast_submodels_tbl <- refit_ensemble_submodel_tbl %>%
    modeltime_forecast(
        h    = "3 years",
        actual_data = worship_evaluation_tbl
    )

forecast_submodels_tbl %>%
    plot_modeltime_forecast()

# Updating the superlearner and the submodels

set.seed(123)
refit_ensemble_superlearner_tbl <- calibration_ensemble_tbl %>%
    modeltime_refit(
        data = worship_evaluation_tbl,
        resamples = worship_evaluation_tbl %>%
            drop_na() %>%
            vfold_cv(v = 10)
    )

forecast_superlearner_tbl <- refit_ensemble_superlearner_tbl %>%
    modeltime_forecast(
        h    = "3 years",
        actual_data = worship_evaluation_tbl
    )

forecast_superlearner_tbl %>%
    plot_modeltime_forecast()

# * Turn off Parallelization ----
plan(sequential)

# 22.0 SAVE ----

model_stack_level_3_tbl %>%
    write_rds("00_models/model_stack_level_3_tbl.rds")

model_stack_level_3_tbl$.model[[1]]$model_tbl$.model[[2]]$model_tbl
