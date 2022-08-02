# Glm
surveyc <- survey %>% mutate_if(is_character,as_factor)

survey.l <- survey %>%
  filter(seed_source == 'Improved seed' & variety == 'Local')

glm.1 <- surveyc %>%
  glm(formula = znl ~ past_fert_usage + seed_source + gender, family = binomial)

glm.1 %>%
  tidy(conf.int = T) %>%
  mutate_if(is.numeric, ~ round(.,3)) %>%
  kable(align = c("l", rep("c", 4)))

glm.1 %>%
  tidy(exponentiate = T, conf.int = T) %>%
  mutate_if(is.numeric, ~ round(.,3)) %>%
  kable(align = c("l", rep("c", 6)))

# Drop disctrict since it has one level
survey0 <- surveyc %>%
  select(plot, village, gender, edulevel, plot_ha, period_cultivated, fertility, seed_source, seed_quality, variety,varietyother, used_fert, znl)
# We will build a model to predict which actual plots had low Zn levels and which did not.
survey0 %>% 
  count(znl) %>%
  mutate(prop = round(n/sum(n),3)*100)

set.seed(372)
splits  <- initial_split(survey0, strata = znl)

train <- training(splits)
test <- testing(splits)

# train set proportions by znl
train %>% 
  count(znl) %>%
  mutate(prop = round(n/sum(n),3)*100)

# test set proportions by znl
test %>% 
  count(znl) %>%
  mutate(prop = round(n/sum(n),3)*100)

# From the train set get 20% as the validation set
set.seed(2123)
val_set <- validation_split(train,
                            strata = znl,
                            prop = 0.80)

# Penalized logistic regression
#Since znl is categorical, logistic regression would be a good first model.

lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") ## Mixture = 1 removes all irrelevent predictors to give a simpler model

# Create the recipe
lr_recipe <-
  recipe(znl ~ ., data = train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

# Create the workflow
lr_workflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(lr_recipe)

#
lr_reg_grid <- tibble(penalty = 10^seq(-0.1, -1, length.out = 30))

lr_reg_grid %>% top_n(-5) # lowest penalty values

lr_reg_grid %>% top_n(5)  # highest penalty values

# Train and tune model
lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

# Visualize validation metrics
lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

top_models <-
  lr_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(30)
lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(znl, .pred_znl) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)

# A second type of model: Tree based ensemble
cores <- parallel::detectCores()
cores

rf_mod <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 15) %>%
  set_engine('ranger', num.threads = cores) %>%
  set_mode('classification')

# Create the recipe and workflow
rf_recipe <-
  recipe(znl~., data =  survey0)

# Add this recipe to our parsnip model
rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>%
  add_recipe(rf_recipe)

# Train and tune the model
rf_mod

# Show what will be tuned
rf_mod %>%
  parameters()

set.seed(3205)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

# Lets select the best model
rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc")
rf_best

rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(znl, .pred_low) %>% 
  mutate(model = "Random Forest")

# The last fit
# the last model
last_rf_mod <- 
  rand_forest(mtry = 8, min_n = 7, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit

last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>% 
  pluck(".workflow", 1) %>%   
  pull_workflow_fit() %>% 
  vip(num_features = 20)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(znl, .pred_ok) %>% 
  autoplot()