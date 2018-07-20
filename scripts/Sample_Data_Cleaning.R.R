## set your working directory to the local difrectory where you have PASSNYC data
## Calculating correlation btw first 10 columns of the dataset. 

df_regis = read.csv('D5 SHSAT Registrations and Testers.csv', header = T)
df_expl = read.csv('2016 School Explorer.csv', header = T)

## names of al the columns
names_of_cols = names(df_expl)

## Converting categorical variables to factors
cat_cols = c('SED.Code','Location.Code', 'District', 'Zip', 'Grades', 'Grade.Low', 'Grade.High','Community.School.')

for (i in cat_cols)
{
    #print(i)
    df_expl[, i] = as.factor(df_expl[, i])
  
}

## Columns selected for first round of estimating correlation

col_for_corr = c("Economic.Need.Index", "School.Income.Estimate", "Percent.ELL", "Percent.Asian",
                 "Percent.Black", "Percent.Hispanic", "Percent.Black...Hispanic", "Percent.White",
                 "Student.Attendance.Rate", "Percent.of.Students.Chronically.Absent")
df_for_corr = df_expl[, col_for_corr]

df_for_corr = df_for_corr[complete.cases(df_for_corr), ]  ## Removes NA values
df_for_corr= df_for_corr[(df_for_corr$School.Income.Estimate != 'N/A'), ] ## Remove NA values written as 'N/A'
df_for_corr= df_for_corr[(df_for_corr$Economic.Need.Index != 'N/A'), ] ## Remove NA values written as 'N/A'

df_for_corr$School.Income.Estimate = (as.character(df_for_corr$School.Income.Estimate)) ## COnverting from factor to character
#Removing any non-numeric character from 'School Income Estimate' column
df_for_corr$School.Income.Estimate = as.numeric(gsub("[^0-9\\.]", "", df_for_corr$School.Income.Estimate) ) #Regular expression
df_for_corr$Economic.Need.Index= as.numeric(as.character(df_for_corr$Economic.Need.Index))

## Removing '%' symbol from the end of cell value and converting it to numeric

for ( j in 3:10)
{
  df_for_corr[, j] = as.numeric(sub("%","",df_for_corr[,j]))/100
}

summary(df_for_corr)
corr_mat = cor(df_for_corr)


