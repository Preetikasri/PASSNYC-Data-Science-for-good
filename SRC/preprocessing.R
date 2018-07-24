
library(Metrics)
library(glmnet)

path = "C:\\UTA_Acads\\Predictive_modelling\\GroupProject\\PASSNYC"
filename = '2016 School Explorer.csv'

## Function to parse the CSV
parse_csv = function(path, filename)
{
  #new_path = gsub("\","\\" , path)
  full_path = paste(path,filename, sep = "\\")
  print(full_path)
  df_expl = read.csv(full_path, header = T)
  
  return (df_expl)
}

## Function to change the column names
col_names_clean = function(df){
  names(df) = gsub('\\.', "", names(df))
  print("successfully cleaned column names")
  
  return (df)
}

## cleaning the data
clean_the_data = function(df, whatcolumns)
{
  ##whatcolumns is a vector of all the columns which need cleaning
  
  for (j in whatcolumns){
    if(j %in% names(df)) {
      print(j)
      df[,j] = (as.character(df[,j])) ## Converting from factor to character
      df[,j] = as.numeric(gsub("[^0-9\\.]", "", df[,j])) #Regular expression
      df[, j]= as.numeric(sub("%","",df[,j]))/100
      df[, j] = df[(df[,j] != 'N/A'), j]
    }
    else
    {
      print(j)
      return("Wrong columns entered!! %s")
    }
    
  }
  return (df)
}

##
## df[, "SEDCode"] = as.factor(df[, "SEDCode"]) 
Remove_NA = function(df)
{
  
  #df = df[(df != 'N/A'),]
  df = df[complete.cases(df), ]
  
  return (df)
}

## Considering only major cities
filter_top_cities = function(df, num_cities =4){
  tb = table(df[, "City"])
  tb = as.data.frame(tb)
  tb = tb[order(tb$Freq, decreasing = T), ]
  v = tb[c(1:num_cities), 1]
  ## filtering dataframe based on cities ##
  df = df[((df$City) %in% v), ]
  df$City = factor(df$City)
  return (df)
}

## Keep only one of the ENI or ELA
keep_only_oneY = function(df, y_variable_str = "AverageELAProficiency")
{
  if(y_variable_str == "AverageELAProficiency")
    remove_var = "EconomicNeedIndex"
  else
    remove_var = "AverageELAProficiency"
  df = df[, -grep(remove_var, colnames(df))]
  
  return (df)
}





## data Preperation for models
data_prep = function(traindata, testdata, formula_y){
  train.mat = model.matrix(formula_y, data = train)
  test.mat = model.matrix(formula_y, data = test)
  
  data_list = list("tr" = train.mat, "tst" = test.mat)
  
  return (data_list)
  
}

## Run LASSO or RIG
regularised_regression = function(input_train_modelmatrix,train_data_y, Type = "LASSO")
{
  if(Type == "LASSO")
    alpha_taken = 1
  else
    alpha_taken = 0
  
  reg.fit = glmnet(input_train_modelmatrix, train_data_y, alpha = alpha_taken )
  cv.reg = cv.glmnet(input_train_modelmatrix, train_data_y, alpha = alpha_taken)
  
  objs = list(reg.fit, cv.reg)
  return (objs)
}

## Predicting values and output RMSE
Fit_regularised_reg = function(input_train_modelmatrix, input_test_modelmatrix,train_data_y, Type_taken="LASSO"){
  mod = regularised_regression(input_train_modelmatrix, train_data_y ,  Type = Type_taken)
  best.lam = mod$lambda.min
  
  yht = predict(mod, s= best.lam, newx = input_test_modelmatrix )
  return (yht)
}

## Fit regression after choosing vars from LASSO/Ridge

run_regression_variables_regularised = function(object, traindata, testdata, formulataken){
  
  lasso.fit = object[[1]]
  lasso.cv = object[[2]]
  best.lambda = lasso.cv$lambda.min
  
  vars_lasso = coef(lasso.fit, s = best.lambda)
  d_coefs = data.frame(name = vars_lasso@Dimnames[[1]][vars_lasso@i + 1], coefficient = vars_lasso@x)
  lassRegr = lm(formulataken, data = traindata[, d_coefs$name])
  yhat = predict(lassRegr, testdata[, d_coefs$name])
  rmse = RMSE_calculator(yhat, testdata$AverageELAProficiency)
  print(paste("RMSE for Lasso is ", rmse, sep = ":"))
  return (lassRegr)
  
}

## Calculating RMSE
RMSE_calculator = function(predicted_y, True_y){
    rmse_cal = rmse(predicted_y, True_y)
    return (rmse_cal)
}

## Data prep for forward regr
#vars_going_Nan =
stepwise_regr = function(train_data, train_data_matrix, dir = "both", target_name = "AverageELAProficiency", scaling = F){
  if(scaling){
  scaled_data = scale(train_data_matrix)
  badcols = vector()
  for (i in 1:ncol(scaled_data))
  {
    
    s = sum(is.na(scaled_data[, i]))
    if (s> 0)
    {
      badcols = c(badcols, i)
    }
  }
   mod_df = data.frame(scaled_data[, -badcols], train_data$AverageELAProficiency) ## TO DO
  }
  else
  {
    mod_df = data.frame(train_data_matrix, train_data$AverageELAProficiency)
  }
   
  colnames(mod_df)[ncol(mod_df)] = target_name
   
   null = lm(AverageELAProficiency~1, data= mod_df)
   full = lm(AverageELAProficiency~., data= mod_df)
   
   regForward = step(null, scope=formula(full), direction= dir, k=log(length(tr)))  ## To Do
   return (regForward)
}

Fit_stepwise = function(train_y, test_y,test_data_matrix, train_data_matrix, model, scaling = F)
{
  if(scaling) {
  scaled_data = scale(train_data_matrix)
  badcols = vector()
  for (i in 1:ncol(scaled_data))
  {
    
    s = sum(is.na(scaled_data[, i]))
    if (s> 0)
    {
      badcols = c(badcols, i)
    }
  }
  mod_df = data.frame(scaled_data[, -badcols], train_data$AverageELAProficiency)
  }
  else
  {
    mod_df = data.frame(train_data_matrix, train_y)
  }
  colnames(mod_df)[ncol(mod_df)] = "AverageELAProficiency"
  
  formula_used = formula(model)
  
  ForwRegr = lm(formula_used, data = mod_df)
  yhat = predict(ForwRegr, data.frame(test_data_matrix))
  rmse = RMSE_calculator(yhat,test_y )
  
  print(paste("RMSE for stepwise is ", rmse, sep = ":"))
  return (ForwRegr)
}

### End of Script ###

