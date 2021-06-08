### Analysis of Opioids as Predictors of Insomnia Medications ###
## Load Packages ##
# For Inferential Modeling
library(tidyverse)
library(ggpubr)
library(survey)
library(gtsummary)
library(car)
library(cobalt)
library(WeightIt)
# For Predictive Modeling
library(tidymodels)
library(themis)
library(VIM)
library(haven)
library(yardstick)
library(foreach)
library(parallel)
library(doParallel)
library(workflowsets)
library(discrim)
library(corrr)
library(xgboost)
library(glmnet)
library(kernlab)
library(naniar)
library(klaR)
library(finetune)
options(tidymodels.dark=T) # Set predictive modeling text to dark so it is visible in the console

## Prepare the data for analysis ##
# Load the 2015-2018 full dataset
zdrug <- read_rds("c:/users/atala/sync/research/projects/to publish/nsduh/bzra.rdata")
# Subset data to select only variables of use/interest
zdrug.data <- zdrug %>%
  dplyr::select("PSU" = verep, "Strata" = vestr,"ID" = QUESTID2, "Weights" = ANALWC5,"Age" = CATAG6,"Race" = NEWRACE2,
         "Marital1" = irmarit,"Marital2" = irmaritstat,"Sex" = irsex,"Income" = income,"Education" = eduhighcat,
         "BMI" = BMI2,"PYRMICat" = MI_CAT_U,"MetroStatus" = COUTYP4,
         "PYRZolpidem" = zolppdapyu,"PYREszopiclone" = eszopdapyu,"PYRZaleplon" = zalepdapyu,
         "PYRZolpidemMU" = zolppdpymu, "PYREszopicloneMU" = eszopdpymu,
         "PYRTriazolam" = triapdapyu,"PYRTemazepam" = temapdapyu,"PYRFlurazepam" = flurpdapyu,
         "PYRHeroin" = heryr,"PYRHydrocodone" = hydcpdapyu,"PYRHydrocodoneMU" = hydcpdpymu,
         "PYROxycodone" = oxcopdapyu, "PYROxycodoneMU" = oxcopdpymu,"PYRTramadol" = trampdapyu, "PYRTramadolMU" = trampdpymu,
         "PYRMorphine" = morppdapyu,"PYRMorphineMU" = morppdpymu,
         "PYRFentanyl" = fentpdapyu,"PYRFentanylMU" = fentpdpymu,"PYRBuprenorphine" = buprpdapyu,"PYRBuprenorphineMU" = buprpdpymu,
         "PYROxymorphone" = oxympdapyu,"PYROxymorphoneMU" = oxympdpymu,"PYRDemerol" = demepdapyu, "PYRDemerolMU" = demepdpymu,
         "PYRHydromorphone" = hydmpdapyu,"PYRHydromorphoneMU" = hydmpdpymu,"PYRMethadone" = mtdnpdapyu,"PYRMethadoneMU" = mtdnpdpymu) %>% 
  # Remove individuals under 18 and remove formatted labels
  filter(Age!=1) %>% zap_labels()
rm(zdrug) # Remove full dataset from memory.
# Format variables for analyses
zdrug.data <- zdrug.data %>% 
  mutate("ID" = factor(ID),
         "Age" = factor(Age, levels = c(2,3,4,5,6), exclude = c(-9),labels = c(1,2,3,4,5)), # "18-25","26-34","35-49","50-64",">65"
         "Race" = factor(Race, levels = c(1,2,7,3,4,5,6),labels = c(1:7)), # "White","Black","Hispanic","Native American","Pacific Islander","Asian","Other"
         "Marital" = factor(case_when(Marital1==1 | Marital2==1 ~ 2, # Married
                               Marital1==2 | Marital2==2 ~ 3, # Widowed
                               Marital1==3 | Marital2==3 ~ 4, # Divorced/Separated
                               Marital1==4 | Marital2==4 ~ 1), # Never Married
                            levels = 1:4),
         "Sex" = factor(Sex, levels = c(1,2)), # Male, Female
         "Income" = factor(Income, levels = c(4,3,2,1)), #">75K","50-75K","20-50K","<20K"
         "Education" = factor(Education, levels = c(4,3,2,1), exclude = c(5,-9)), #"College","Some College","High School","Less than High School"
         "BMI" = factor(case_when(BMI<18.5 ~ 2, # Underweight
                                  BMI>=18.5 & BMI<25 ~ 1, # Normal weight
                                  BMI>=25 & BMI<30 ~ 3, # Overweight
                                  BMI>=30 & BMI<40 ~ 4, # Obese
                                  BMI>=40 ~ 5), # Morbidly Obese
                        levels = 1:5),
         "PYRMICat" = factor(PYRMICat, levels = 0:3), # "No MI","Mild MI","Moderate MI","Serious MI"
         "MetroStatus" = factor(MetroStatus, levels = c(1,2,3)), # "Large Metro","Small Metro","Nonmetro"
         "PYRZdrug" = factor(case_when(PYRZolpidem==1|PYREszopiclone==1|PYRZaleplon==1&PYRZolpidemMU==0&PYREszopicloneMU==0 ~ 1, # PYRZdrug Yes
                                       PYRZolpidem==0&PYREszopiclone==0&PYRZaleplon==0 ~ 0), # PYRZdrug No
                             levels = 0:1),
         "PYRZdrugMU" = factor(case_when(PYRZolpidemMU==1|PYREszopicloneMU==1 ~ 1, # PYRZdrugMU = Yes
                                         PYRZolpidemMU==0&PYREszopicloneMU==0 ~ 0),# PYRZdrugMU = No
                                           levels = 0:1),
         "PYRZdrug2" = factor(case_when(PYRZolpidemMU==1|PYREszopicloneMU==1 ~ 2, # Zdrug Misuse
                                        PYRZolpidem==1|PYREszopiclone==1|PYRZaleplon==1&PYRZolpidemMU==0&PYREszopicloneMU==0 ~ 1, # Zdrug Use
                                        PYRZolpidem==0&PYREszopiclone==0&PYRZaleplon==0 ~ 0), #No Zdrug Use
                                          levels = 0:2),
         "SedBenzo" = factor(case_when(PYRTriazolam==1|PYRTemazepam==1|PYRFlurazepam==1~1, # SedBenzo Use
                                       PYRTriazolam==0&PYRTemazepam==0&PYRFlurazepam==0~0),# No SedBenzo Use
                             levels=0:1),    
         "PYRHeroin" = factor(PYRHeroin, levels = c("0","1"), labels = c("No","Yes")),
         "PYRHydrocodone" = factor(PYRHydrocodone, levels = c("0","1"), labels = c("No","Yes")),
         "PYRHydrocodoneMU" = factor(PYRHydrocodoneMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYROxycodone" = factor(PYROxycodone, levels = c("0","1"), labels = c("No","Yes")),
         "PYROxycodoneMU" = factor(PYROxycodoneMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRTramadol" = factor(PYRTramadol, levels = c("0","1"), labels = c("No","Yes")),
         "PYRTramadolMU" = factor(PYRTramadolMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRMorphine" = factor(PYRMorphine, levels = c("0","1"), labels = c("No","Yes")),
         "PYRMorphineMU" = factor(PYRMorphineMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRFentanyl" = factor(PYRFentanyl, levels = c("0","1"), labels = c("No","Yes")),
         "PYRFentanylMU" = factor(PYRFentanylMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRBuprenorphine" = factor(PYRBuprenorphine, levels = c("0","1"), labels = c("No","Yes")),
         "PYRBuprenorphineMU" = factor(PYRBuprenorphineMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYROxymorphone" = factor(PYROxymorphone, levels = c("0","1"), labels = c("No","Yes")),
         "PYROxymorphoneMU" = factor(PYROxymorphoneMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRDemerol" = factor(PYRDemerol, levels = c("0","1"), labels = c("No","Yes")),
         "PYRDemerolMU" = factor(PYRDemerolMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRHydromorphone" = factor(PYRHydromorphone, levels = c("0","1"), labels = c("No","Yes")),
         "PYRHydromorphoneMU" = factor(PYRHydromorphoneMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYRMethadone" = factor(PYRMethadone, levels = c("0","1"), labels = c("No","Yes")),
         "PYRMethadoneMU" = factor(PYRMethadoneMU, levels = c("0","1"), labels = c("No","Yes")),
         "PYROpioid" = factor(case_when(PYRHydrocodone=="Yes"|PYROxycodone=="Yes"|PYRTramadol=="Yes"|PYRHeroin=="Yes"|
                                   PYRMorphine=="Yes"|PYRFentanyl=="Yes"|PYRBuprenorphine=="Yes"|PYROxymorphone=="Yes"|
                                   PYRDemerol=="Yes"|PYRHydromorphone=="Yes"|PYRMethadone=="Yes"~1, # Opioid Use Yes
                                 PYRHydrocodone=="No"&PYROxycodone=="No"&PYRTramadol=="No"&PYRHeroin=="No"&
                                   PYRMorphine=="No"&PYRFentanyl=="No"&PYRBuprenorphine=="No"&PYROxymorphone=="No"&
                                   PYRDemerol=="No"&PYRHydromorphone=="No"&PYRMethadone=="No"~0), levels=0:1), # Opioid Use No
         "PYROpioidMU" = factor(case_when(PYRHydrocodoneMU=="Yes"|PYROxycodoneMU=="Yes"|PYRTramadolMU=="Yes"|
                                     PYRMorphineMU=="Yes"|PYRFentanylMU=="Yes"|PYRBuprenorphineMU=="Yes"|PYROxymorphoneMU=="Yes"|
                                     PYRDemerolMU=="Yes"|PYRHydromorphoneMU=="Yes"|PYRMethadoneMU=="Yes"|PYRHeroin=="Yes"~1, # Opioid Misuse Yes
                                     PYRHydrocodoneMU=="No"&PYROxycodoneMU=="No"&PYRTramadolMU=="No"&
                                     PYRMorphineMU=="No"&PYRFentanylMU=="No"&PYRBuprenorphineMU=="No"&PYROxymorphoneMU=="No"&
                                     PYRDemerolMU=="No"&PYRHydromorphoneMU=="No"&PYRMethadoneMU=="No"&PYRHeroin=="No"~0), levels = 0:1), # Opioid Misuse No
         "PYROpioid2" = factor(case_when(PYROpioid==1 & PYROpioidMU==1 ~ 2, # Opioid Misuse
                                         PYROpioid==1 & PYROpioidMU==0 ~ 1, # Opioid Use
                                         PYROpioid==0 & PYROpioidMU==0 ~ 0), # No Opioid Use
                               levels = 0:2)) %>% 
  filter(is.na(PYRZdrug2)==F) # Remove missing data for primary outcome

## Inferential Modeling Analysis: Does Opioid use predict use of Z-drugs or sedative benzodiazepines? ##
# Create Survey Design Object
zdrug.design <- svydesign(id= ~PSU, strata=~Strata,  weights=~Weights, data = zdrug.data,nest=T)
# Summarize the Data
tbl_svysummary(zdrug.design,by=PYROpioid2,include=c("Age","Sex","Race","BMI","Education","Income","Marital","MetroStatus","PYRMICat","PYROpioid2"),
               statistic = list(all_continuous()~"{mean} ({sd})", all_categorical() ~ "{n_unweighted} ({p})")) %>% 
  add_p(test = list(all_continuous()~"svy.kruskal.test",all_categorical()~"svy.chisq.test"))
# Generate Propensity Score Weighting variables for PYROpioid2
set.seed(627)
# Propensity Score Model 1: PYROpioid2~Age+Race+Sex+BMI
op.scores1 <- weightit(PYROpioid2~Age+Race+Sex,zdrug.data,
                       method="cbps",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
# Propensity Score Model 2: PYROpioid2~Age+Race+Sex+BMI+Income+Education+Marital+MetroStatus
op.scores2 <- weightit(PYROpioid2~Age+Sex+Race+Income+Education+Marital+MetroStatus,zdrug.data,
                       method="cbps",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
# Propensity Score Model 3: PYROpioid2~Age+Race+Sex+BMI+Income+Education+Marital+MetroStatus+PYRMICat
op.scores3 <- weightit(PYROpioid2~Age+Sex+Race+Income+Education+Marital+MetroStatus+PYRMICat,zdrug.data,
                       method="cbps",stabilize=T,s.weights = "Weights",estimand = "ATE",stop.method="es.mean")
# Assign IPTW weights to main dataset variables
zdrug.data$Oweights1 <- op.scores1$weights
zdrug.data$Oweights2 <- op.scores2$weights
zdrug.data$Oweights3 <- op.scores3$weights
# Make survey design object using propensity scores weighted on PYROpioid2
zdrug.design1 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Oweights1, data = zdrug.data,nest=T)
zdrug.design2 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Oweights2, data = zdrug.data,nest=T)
zdrug.design3 <-  svydesign(id= ~PSU, strata = ~Strata, weights=~Oweights3, data = zdrug.data,nest=T)
# Build Inferential Modeling Loop
outcomes <- c("PYRZdrug","PYRZdrugMU","SedBenzo")
predictors <- c(
  "PYROpioid2",
  "PYROpioid2+Age+Sex+Race",
  "PYROpioid2+Age+Sex+Race+Income+Education+Marital+MetroStatus",
  "PYROpioid2+Age+Sex+Race+Income+Education+Marital+MetroStatus+PYRMICat"
  )
modelnames <- c("Model 1","Model 2","Model 3","Model 4")
modeldesigns <- list(zdrug.design,zdrug.design1,zdrug.design2,zdrug.design3)
zdrug.m <- list()
for(idx in 1:length(outcomes)){
  modellist <- list()
  for(jdx in 1:length(predictors)){
    modellist[[jdx]] <- svyglm(as.formula(paste(outcomes[idx],"~",predictors[jdx],sep="")),modeldesigns[[jdx]],family="quasibinomial")
  }
  zdrug.m[[idx]] <- modellist
  print(paste("Completed",outcomes[idx]))
}
# Extract model coefficients and create model summary table
tbl_stack(group_header = outcomes, list(
  tbl_merge(tab_spanner = modelnames, list(
    tbl_regression(zdrug.m[[1]][[1]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[1]][[2]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[1]][[3]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[1]][[4]],exponentiate = T,include="PYROpioid2"))),
  tbl_merge(list(
    tbl_regression(zdrug.m[[2]][[1]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[2]][[2]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[2]][[3]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[2]][[4]],exponentiate = T,include="PYROpioid2"))),
  tbl_merge(list(
    tbl_regression(zdrug.m[[3]][[1]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[3]][[2]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[3]][[3]],exponentiate = T,include="PYROpioid2"),
    tbl_regression(zdrug.m[[3]][[4]],exponentiate = T,include="PYROpioid2")))
))

## Predictive Modeling Analysis: How well do these data help classify users of Zdrugs and Sedative Benzodiazepines? ##
# Setup for Parallel Processing
cl <- makeCluster(4)
registerDoParallel(cl)
# Initialize all models, including identifying hyperparameters needing tuning
naivebayes.m <- naive_Bayes(smoothness = tune(), Laplace = tune()) %>% set_engine("klaR")
logistic.m <- logistic_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")
decisiontree.m <- decision_tree(cost_complexity = tune(), min_n = tune()) %>% set_engine("rpart") %>% set_mode("classification")
randomforest.m <- rand_forest(mtry=tune(),min_n=tune(),trees=1000) %>% set_engine("ranger") %>% set_mode("classification")
xgboost.m <- boost_tree(tree_depth=tune(), learn_rate=tune(), loss_reduction=tune(),
                        min_n=tune(),sample_size=tune(),trees=tune()) %>% set_engine("xgboost") %>% set_mode("classification")
svm.m <- svm_rbf(cost= tune(), rbf_sigma=tune()) %>% set_engine("kernlab") %>% set_mode("classification")
# Outcome 1: PYRZdrug use predicted by PYROpioid2 + Covariates
# Select data 
zdrug.op <- zdrug.data %>% 
  dplyr::select(PYRZdrug,PYROpioid2,Age,Race,Sex,
                Income,Education,Marital,PYRMICat,MetroStatus) %>%
  mutate("PYRZdrug" = factor(PYRZdrug, levels = 0:1, labels = 2:1))
# Split the data into test and train datasets and create cross validation folds
set.seed(123)
zdrug.op.split <- initial_split(zdrug.op,strata=PYRZdrug)
zdrug.op.train <- training(zdrug.op.split)
zdrug.op.test <- testing(zdrug.op.split)
set.seed(321)
zdrug.op.cv <- vfold_cv(zdrug.op.train, v=10, repeats=5,strata=PYRZdrug)
# Generate preprocessing recipe
set.seed(111)
zdrug.op.rec <- 
  recipe(PYRZdrug~PYROpioid2+Age+Race+Sex+Income+Education+Marital+PYRMICat+MetroStatus,zdrug.op.train) %>%
  step_downsample(PYRZdrug) %>% # Downsample training data to match outcome class probability
  step_dummy(all_nominal_predictors()) # Convert factors to multiple dummy variables
# Create Workflow Set
set.seed(222)
zdrug.op.wflow <- 
  workflow_set(
    preproc = list("downsample.impute" = zdrug.op.rec),
    models = list("NB" = naivebayes.m, "Logistic" = logistic.m, "Tree" = decisiontree.m, "RF" = randomforest.m, "XGB" = xgboost.m, "SVM" = svm.m),
    cross = T
    )
# Set verbose printing for eliminating models in the tuning race
race_ctrl <- control_race(verbose_elim = T)
# Optimize hyperparameter tuning using the ANOVA racing method
zdrug.op.race.results <- 
  zdrug.op.wflow %>%
  workflow_map(
    fn = "tune_race_anova",
    seed = 312,
    resamples = zdrug.op.cv,
    grid = 20,
    control = race_ctrl,
    verbose = T,
    metrics = metric_set(mcc,bal_accuracy,kap,j_index,f_meas,roc_auc)
  )
# Review model results and pick best model based on MCC
zdrug.op.rankresults <- rank_results(zdrug.op.race.results, rank_metric = "mcc", select_best = T) %>% # Save table of model metrics data
  mutate("lower" = mean-std_err, "upper" = mean+std_err, 
         ".metric" = factor(.metric, levels = c("mcc","roc_auc","bal_accuracy","kap","j_index","f_meas"),
                            labels = c("MCC","ROC_AUC","Balanced Accuracy","Kappa","J Index","F Score")),
         "model" = factor(model, levels = c("logistic_reg","naive_Bayes","rand_forest","decision_tree","boost_tree","svm_rbf"),
                          labels = c("Regularized Logistic Regression","Naive Bayes","Random Forest","Decision Tree","Gradient Boosting","Support Vector Machine")))
zdrug.op.bestmodel <- zdrug.op.rankresults %>% filter(rank==1,.metric=="MCC") %>% dplyr::select(wflow_id) %>% flatten_chr() # Identify best model by MCC
zdrug.op.modelplot <- ggscatter(filter(zdrug.op.rankresults, .metric=="MCC" | .metric=="Balanced Accuracy" | .metric=="ROC_AUC"), x="model",y="mean",color="model") + 
  geom_errorbar(aes(ymin=lower, ymax=upper),width = 0.15) + facet_wrap(~.metric,scales="free",strip.position = "top")+
  ylab("Metric") + xlab("Z-drug use") + scale_x_discrete(labels = c("","","","","",""))+
  theme(legend.title = element_blank())
# Finalize model
zdrug.op.best.results <- 
  zdrug.op.race.results %>%
  pull_workflow_set_result("downsample.impute_Logistic") %>% 
  select_best(metric = "mcc")
zdrug.op.final <- 
  zdrug.op.race.results %>%
  pull_workflow("downsample.impute_Logistic") %>%
  finalize_workflow(zdrug.op.best.results) %>%
  last_fit(split = zdrug.op.split, metrics = metric_set(mcc, roc_auc, bal_accuracy))
# Collect Metrics and Predictions
zdrug.op.final.metrics <- collect_metrics(zdrug.op.final)
zdrug.op.final.preds <- collect_predictions(zdrug.op.final)
# Plot ROC Curve
zdrug.op.roc <- autoplot(roc_curve(zdrug.op.final.preds,truth = PYRZdrug,".pred_2"))
# Close unnecessary environment objects
rm(list = c("zdrug.op", "zdrug.op.cv", "zdrug.op.rec", "zdrug.op.test", "zdrug.op.train", "zdrug.op.split", "zdrug.op.wflow"))

# Outcome 2: PYRZdrugMU predicted by PYROpioid2 + Covariates  
# Select data 
zdrug.mu.op <- zdrug.data %>% 
  dplyr::select(PYRZdrugMU,PYROpioid2,Age,Race,Sex,
                Income,Education,Marital,PYRMICat,MetroStatus)%>%
  mutate("PYRZdrugMU" = factor(PYRZdrugMU, levels = 0:1, labels = 2:1))
# Split the data into test and train datasets and create cross validation folds
set.seed(123)
zdrug.mu.op.split <- initial_split(zdrug.mu.op,strata=PYRZdrugMU)
zdrug.mu.op.train <- training(zdrug.mu.op.split)
zdrug.mu.op.test <- testing(zdrug.mu.op.split)
set.seed(321)
zdrug.mu.op.cv <- vfold_cv(zdrug.mu.op.train, v=10, repeats=5,strata=PYRZdrugMU)
# Generate preprocessing recipe
set.seed(111)
zdrug.mu.op.rec <- 
  recipe(PYRZdrugMU~PYROpioid2+Age+Race+Sex+Income+Education+Marital+PYRMICat+MetroStatus,zdrug.mu.op.train) %>%
  step_downsample(PYRZdrugMU) %>%
  step_dummy(all_nominal_predictors())
# Create Workflow Set
set.seed(222)
zdrug.mu.op.wflow <- 
  workflow_set(
    preproc = list("downsample.impute" = zdrug.mu.op.rec),
    models = list("NB" = naivebayes.m, "Logistic" = logistic.m, "Tree" = decisiontree.m, "RF" = randomforest.m, "XGB" = xgboost.m, "SVM" = svm.m),
    cross = T
    )
# Tune model hyperparameters using ANOVA race
zdrug.mu.op.race.results <- 
  zdrug.mu.op.wflow %>%
  workflow_map(
    fn = "tune_race_anova",
    seed = 312,
    resamples = zdrug.mu.op.cv,
    grid = 20,
    control = race_ctrl,
    verbose = T,
    metrics = metric_set(mcc,bal_accuracy,kap,j_index,f_meas,roc_auc)
  )
# Review model results and pick best model approach using MCC
zdrug.mu.op.rankresults <- rank_results(zdrug.mu.op.race.results, rank_metric = "mcc", select_best = T) %>% # Save table of model metrics data
  mutate("lower" = mean-std_err, "upper" = mean+std_err, 
         ".metric" = factor(.metric, levels = c("mcc","roc_auc","bal_accuracy","kap","j_index","f_meas"),
                            labels = c("MCC","ROC_AUC","Balanced Accuracy","Kappa","J Index","F Score")),
         "model" = factor(model, levels = c("logistic_reg","naive_Bayes","rand_forest","decision_tree","boost_tree","svm_rbf"),
                          labels = c("Regularized Logistic Regression","Naive Bayes","Random Forest","Decision Tree","Gradient Boosting","Support Vector Machine")))
zdrug.mu.op.bestmodel <- zdrug.mu.op.rankresults %>% filter(rank==1,.metric=="MCC") %>% dplyr::select(wflow_id) %>% flatten_chr()
zdrug.mu.op.modelplot <- ggscatter(filter(zdrug.mu.op.rankresults, .metric=="MCC" | .metric=="Balanced Accuracy" | .metric=="ROC_AUC"), x="model",y="mean",color="model") + 
  geom_errorbar(aes(ymin=lower, ymax=upper),width = 0.15) + facet_wrap(~.metric,scales="free",strip.position = "top")+
  ylab("Metric") + xlab("Z-drug misuse") + scale_x_discrete(labels = c("","","","","",""))+
  theme(legend.title = element_blank())
# Finalize model
zdrug.mu.op.best.results <- 
  zdrug.mu.op.race.results %>%
  pull_workflow_set_result("downsample.impute_XGB") %>% 
  select_best(metric = "mcc")
zdrug.mu.op.final <- 
  zdrug.mu.op.race.results %>%
  pull_workflow("downsample.impute_XGB") %>%
  finalize_workflow(zdrug.mu.op.best.results) %>%
  last_fit(split = zdrug.mu.op.split, metrics = metric_set(mcc, roc_auc, bal_accuracy))
# Collect Metrics and Predictions
zdrug.mu.op.final.metrics <- collect_metrics(zdrug.mu.op.final)
zdrug.mu.op.final.preds <- collect_predictions(zdrug.mu.op.final)
# Plot ROC Curve
zdrug.mu.op.roc <- autoplot(roc_curve(zdrug.mu.op.final.preds,truth = PYRZdrugMU,".pred_2"))
# Close unnecessary environment objects
rm(list = c("zdrug.mu.op", "zdrug.mu.op.cv", "zdrug.mu.op.rec", "zdrug.mu.op.test", "zdrug.mu.op.train", "zdrug.mu.op.split", "zdrug.mu.op.wflow"))

# Outcome 3: Sedative Benzodiazepines predicted by PYROpioid2 + Covariates
# Select data 
sedbenzo.op <- zdrug.data %>% 
  dplyr::select(SedBenzo,PYROpioid2,Age,Race,Sex,
                Income,Education,Marital,PYRMICat,MetroStatus)%>%
  mutate("SedBenzo" = factor(SedBenzo, levels = 0:1, labels = 2:1))
# Split the data into test and train datasets and create cross validation folds
set.seed(123)
sedbenzo.op.split <- initial_split(sedbenzo.op,strata=SedBenzo)
sedbenzo.op.train <- training(sedbenzo.op.split)
sedbenzo.op.test <- testing(sedbenzo.op.split)
set.seed(321)
sedbenzo.op.cv <- vfold_cv(sedbenzo.op.train, v=10, repeats=5,strata=SedBenzo)
# Generate preprocessing recipe
set.seed(111)
sedbenzo.op.rec <- recipe(SedBenzo~PYROpioid2+Age+Race+Sex+Income+Education+Marital+PYRMICat+MetroStatus,sedbenzo.op.train) %>%
  step_downsample(SedBenzo) %>%
  step_dummy(all_nominal_predictors())
# Create Workflow Set
set.seed(222)
sedbenzo.op.wflow <- 
  workflow_set(
    preproc = list("downsample.impute" = sedbenzo.op.rec),
    models = list("NB" = naivebayes.m, "Logistic" = logistic.m, "Tree" = decisiontree.m, "RF" = randomforest.m, "XGB" = xgboost.m, "SVM" = svm.m),
    cross = T
    )
# Tune model hyperparameters using ANOVA race
sedbenzo.op.race.results <- 
  sedbenzo.op.wflow %>%
  workflow_map(
    fn = "tune_race_anova",
    seed = 312,
    resamples = sedbenzo.op.cv,
    grid = 20,
    control = race_ctrl,
    verbose = T,
    metrics = metric_set(mcc,bal_accuracy,kap,j_index,f_meas,roc_auc)
  )
# Review model results and pick best model approach using MCC
sedbenzo.op.rankresults <- rank_results(sedbenzo.op.race.results, rank_metric = "mcc", select_best = T) %>% # Save table of model metrics data
  mutate("lower" = mean-std_err, "upper" = mean+std_err, 
         ".metric" = factor(.metric, levels = c("mcc","roc_auc","bal_accuracy","kap","j_index","f_meas"),
                            labels = c("MCC","ROC_AUC","Balanced Accuracy","Kappa","J Index","F Score")),
         "model" = factor(model, levels = c("logistic_reg","naive_Bayes","rand_forest","decision_tree","boost_tree","svm_rbf"),
                          labels = c("Regularized Logistic Regression","Naive Bayes","Random Forest","Decision Tree","Gradient Boosting","Support Vector Machine")))
sedbenzo.op.bestmodel <- sedbenzo.op.rankresults %>% filter(rank==1,.metric=="MCC") %>% dplyr::select(wflow_id) %>% flatten_chr()
sedbenzo.op.modelplot <- ggscatter(filter(sedbenzo.op.rankresults, .metric=="MCC" | .metric=="Balanced Accuracy" | .metric=="ROC_AUC"), x="model",y="mean",color="model") + 
  geom_errorbar(aes(ymin=lower, ymax=upper),width = 0.15) + facet_wrap(~.metric,scales="free",strip.position = "top")+
  ylab("Metric") + xlab("Sedative benzodiazepine use") + scale_x_discrete(labels = c("","","","","",""))+
  theme(legend.title = element_blank())
# Finalize model
sedbenzo.op.best.results <- 
  sedbenzo.op.race.results %>%
  pull_workflow_set_result("downsample.impute_Logistic") %>% 
  select_best(metric = "mcc")
sedbenzo.op.final <- 
  sedbenzo.op.race.results %>%
  pull_workflow("downsample.impute_Logistic") %>%
  finalize_workflow(sedbenzo.op.best.results) %>%
  last_fit(split = sedbenzo.op.split, metrics = metric_set(mcc, roc_auc, bal_accuracy))
# Collect Metrics and Predictions
sedbenzo.op.final.metrics <- collect_metrics(sedbenzo.op.final)
sedbenzo.op.final.preds <- collect_predictions(sedbenzo.op.final)
# Plot ROC Curve
sedbenzo.op.roc <- autoplot(roc_curve(sedbenzo.op.final.preds,truth = SedBenzo,".pred_2"))
## Close unnecessary environment objects
rm(list = c("sedbenzo.op", "sedbenzo.op.cv", "sedbenzo.op.rec", "sedbenzo.op.test", "sedbenzo.op.train", "sedbenzo.op.split", "sedbenzo.op.wflow"))
stopCluster(cl)

## Create model performance plot
modelperf.p <- ggarrange(zdrug.op.modelplot,sedbenzo.op.modelplot,nrow=2,ncol=1,common.legend = T)
modelroc.p <- ggarrange(zdrug.op.roc+ ggtitle("Z-drug use"),sedbenzo.op.roc+ ggtitle("Sedative benzodiazepine use"),nrow=1,ncol=2)

tiff("~/fig1.tiff",width=2000, height=1800,res = 300)
modelperf.p
dev.off()
tiff("~/fig2.tiff",width=2000, height=1200,res = 300)
modelroc.p
dev.off()

