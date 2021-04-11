#Alistair So-Schoos
#Time Series Model Building---- 
#Domo-enhanced version
#04-08-2021

###############################################################################
#                                                                              #
#                   Modeltime Building                                         #
#                                                                              #
# Dependencies: Parochial Data in Long Form for Time Series                    #
#                                                                              #
###############################################################################


#Modeltime Deployment 

# LIBRARIES ----

# Time Series Machine Learning
library(tidymodels)
library(modeltime)

# EDA
library(DataExplorer)

# Core
library(tidyverse)
library(timetk)
library(lubridate)
library(DomoR)
library(readxl)
library(rstan)

# DATA -----

#Initiate Domo User Settings----

DomoR::init('cpg-org-enterprise','1719a5d67fef134a723f764b31864e9156e17dffa853c597')

#Load data from Domo----

PR_data_year <- DomoR::fetch('fd8466e8-fe11-4f80-9cdd-7b7bbaf08a75') 

#PR_data_year$year <- as_date(PR_data_year$year, origin = lubridate::origin)


Summarized_Closed_Parishes2019 <- read_excel("~/Desktop/Education_Assistance/Data606/Summarized_Closed_Parishes2019.xlsx")
View(Summarized_Closed_Parishes2019)

# 1.0 EDA & DATA PREP ----
# * Attendane and finances vs closed_indicator

Summarized_Closed_Parishes2019  %>% glimpse()


# * Summary Diagnostics ----

Summarized_Closed_Parishes2019 %>% tk_summary_diagnostics(.date_var = year_end)

# * Pad the Time Series ----

PR_by_year <- PR_by_year %>% 
  pad_by_time(.date_var = year,
              .by = "year",
              .pad_value = 0)


# * Visualizations ----

worship_trend <-  Summarized_Closed_Parishes2019 %>% 
  plot_time_series(year_end, Average_worship_by_members)

worship_index_trend <-  Summarized_Closed_Parishes2019 %>% 
  plot_time_series(year_end, Median_attendance)

inc_exp_trend <-  Summarized_Closed_Parishes2019 %>% 
  plot_time_series(year_end, Average_inc_exp_ratio)

# 2.0 EVALUATION PERIOD ----

# * Filtering ----
worship_evaluation_tbl <- Summarized_Closed_Parishes2019  %>% 
  filter_by_time(
    .date_var = year_end, 
    .start_date = "2006-12-31" , 
    .end_date = "2019-12-31")

worship_evaluation_tbl %>% 
  plot_time_series(year_end, Average_worship_by_members)

# * Train/Test ----

splits <- worship_evaluation_tbl %>% 
  time_series_split(
    date_var = year_end,
    initial = "11 years",
    assess = "3 years",
    cumulative = TRUE,
    lag = "1 year"
  ) 

splits %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(year_end, Average_worship_by_members)

# 3.0 PROPHET FORECASTING ----

# * Prophet Model using Modeltime/Parsnip ----

model_prophet_fit <- prophet_reg() %>% 
  set_engine("prophet") %>% 
  fit(Average_worship_by_members ~ year_end, data = training(splits))

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
  plot_seasonal_diagnostics(year_end, log(Average_worship_by_members))

# * Recipes Spec ----

training(splits)

recipe_spec <- recipe(Average_worship_by_members ~ ., data = training(splits)) %>% 
  
  # Time Series Signature
  step_timeseries_signature(year_end) %>% 
  
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

# 5.0 SUMMARY & NEXT STEPS ----

# * What you've learned ----
# - You've been exposed to:
#   - Tidymodels / Modeltime Framework
# - You've seen 2 modeling approaches:
#   - Prophet - Univariate, Automatic
#   - Linear Regression Model - Many recipe steps
# - You've experienced Feature Engineering
#   - Visualizations: ACF, Seasonality
#   - Feature Engineering from Date Variables
#
# * Where you are going! ----
# - You still need to learn:
#   - New algorithms
#   - Machine Learning - How to tune parameters
#   - Feature Engineering Strategies
#   - Ensembling - Competition winning strategy
#   - and a lot more!

