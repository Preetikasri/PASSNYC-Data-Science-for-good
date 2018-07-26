## Calling each of the functions defined before to execute the tasks

df_exp = parse_csv(path, filename)
df_exp = col_names_clean(df_exp)
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

df_exp= df_exp[(df_exp$AverageELAProficiency != 'N/A'), ] ## Remove NA values written as 'N/A'
df_exp= df_exp[(df_exp$AverageMathProficiency != 'N/A'), ] ## Remove NA values written as 'N/A'
df_exp= df_exp[(df_exp$SchoolIncomeEstimate != 'N/A'), ]

df_exp$EconomicNeedIndex = as.numeric(as.character(df_exp$EconomicNeedIndex))
df_exp$AverageELAProficiency= as.numeric(as.character(df_exp$AverageELAProficiency))
df_exp$AverageMathProficiency= as.numeric(as.character(df_exp$AverageMathProficiency))

df_exp = filter_top_cities(df_exp)
## Till here it's cleaned data
## Keeping only AverageELA as y 
df_exp = keep_only_oneY(df_exp, y_variable_str = "EconomicNeedIndex")

## Some extra cleaning required on inspection
# df_exp = df_exp[, -c(1:3)]
# df_exp =df_exp[, -2] # Remove SED Code

## Creating train data and test data
## Splitting data in train and test
## Removing the variables which aren't needed in the model
cols_not_needed = c("AdjustedGrade", "New", "OtherLocationCodeinLCGMS","SEDCode", "SchoolName","LocationCode", "District", "AddressFull", "Grades", "Zip", "GradeLow", 
                    "GradeHigh", "RigorousInstructionRating", "CollaborativeTeachersRating", 
                    "SupportiveEnvironmentRating", "EffectiveSchoolLeadershipRating", 
                    "StrongFamilyCommunityTiesRating", "TrustRating","StudentAchievementRating" ,"AverageMathProficiency")

vec = vector()
for (col in 1:length(cols_not_needed)){
  
  idx = grep(cols_not_needed[col], colnames(df_exp))
  vec = c(vec, idx)
}

df_model = df_exp[, -vec]
set.seed(222)
tr = sample(1:nrow(df_model), 500)
train = df_model[tr, ]
test = df_model[-tr,]

data_list = data_prep(train, test, formula(EconomicNeedIndex~.))
train.mat = (data_list[[1]])
test.mat = (data_list[[2]])

#lasso_fit = regularised_regression(train.mat, train$AverageELAProficiency)
Lasso_ob = regularised_regression(train.mat, train$AverageELAProficiency)


lass_final_model = run_regression_variables_regularised(Lasso_ob, train, test, formula(AverageELAProficiency~.))


# Fitting Forward stepwise regression
Forward_ob = stepwise_regr(train, train.mat)
stepwise_final_model = Fit_stepwise(train$AverageELAProficiency, test$AverageELAProficiency, test.mat, train.mat, model = Forward_ob)



