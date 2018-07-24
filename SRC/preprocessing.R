
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
  return (cv.reg)
}

## Predicting values and output RMSE
predict_regularised_reg = function(input_train_modelmatrix, input_test_modelmatrix, Type_taken="LASSO"){
  mod = regularised_regression(input_train_modelmatrix, Type = Type_taken)
  best.lam = mod$lambda.min
  
  yht = predict(mod, s= best.lam, newx = input_test_modelmatrix )
  return (yht)
}

## Calculating RMSE
RMSE_calculator = function(predicted_y, True_y){
    rmse_cal = rmse(predicted_y, True_y)
    return (rmse_cal)
}

## Data prep for forward regr
#vars_going_Nan =
stepwise_regr = function(train_data, train_data_matrix, dir = "both", target_name = "AverageELAProficiency"){
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
   colnames(mod_df)[ncol(mod_df)] = target_name
   
   null = lm(AverageELAProficiency~1, data= mod_df)
   full = lm(AverageELAProficiency~., data= mod_df)
   
   regForward = step(null, scope=formula(full), direction= dir, k=log(length(tr)))  ## To Do
   return (regForward)
}

Fit_stepwise = function(train_data, train_data_matrix, model)
{
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
  colnames(mod_df)[ncol(mod_df)] = "AverageELAProficiency"
  
  formula_used = formula(model)
  
  ForwRegr = lm(formula_used, data = mod_df)
  return (ForwRegr)
}


### Executing each function defined above
df_exp = parse_csv(path, filename)
df_exp = col_names_clean(df_exp)
#cols_to_clean : yet to define
cols_to_clean = c( "PercentELL",
"PercentAsian",
"PercentBlack",
"PercentHispanic",
"PercentBlackHispanic",
"PercentWhite",
"RigorousInstruction", "CollaborativeTeachers" ,
"SupportiveEnvironment", "Trust",
"StrongFamilyCommunityTies", "EffectiveSchoolLeadership",
"StudentAttendanceRate",
"PercentofStudentsChronicallyAbsent", "SchoolIncomeEstimate"
)
df_exp = clean_the_data(df_exp, cols_to_clean)
df_exp = Remove_NA(df_exp)


## Cleaning for Non-String NAs 
df = as.data.frame(df_exp)
df_exp= df_exp[(df_exp$AverageELAProficiency != 'N/A'), ] ## Remove NA values written as 'N/A'
df_exp= df_exp[(df_exp$AverageMathProficiency != 'N/A'), ] ## Remove NA values written as 'N/A'
df_exp= df_exp[(df_exp$SchoolIncomeEstimate != 'N/A'), ]

df_exp$EconomicNeedIndex = as.numeric(as.character(df_exp$EconomicNeedIndex))
df_exp$AverageELAProficiency= as.numeric(as.character(df_exp$AverageELAProficiency))
df_exp$AverageMathProficiency= as.numeric(as.character(df_exp$AverageMathProficiency))

df_exp = filter_top_cities(df_exp)

## Creating train and test
sample_size = 500
tr = sample(1:nrow(df_exp), sample_size)
train = df_exp[tr, ]
test = df_exp[-tr,]

#predictors = c(1,2,3....)

data_list = data_prep(train, test, formula(AverageELAProficiency~.))
train.mat = as.data.frame(data_list[[1]])
test.mat = as.data.frame(data_list[[2]])


















