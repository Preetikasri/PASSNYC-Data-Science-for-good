
## set your working directory to the local difrectory where you have PASSNYC data
## Calculating correlation btw first 10 columns of the dataset. 

getwd()
setwd("C:\\Users\\vsm397\\Desktop\\Vir's Personal documents\\Pred Modeling (MSBA)\\NYC Schools Data")

df_regis = read.csv('D5 SHSAT Registrations and Testers.csv', header = T)
df_expl = read.csv('2016 School Explorer.csv', header = T)

## names of all the columns
names_of_cols <- names(df_expl)

## Converting categorical variables to factors
cat_cols <- c('SED.Code','Location.Code', 'District', 'Zip', 'Grades', 'Grade.Low', 'Grade.High', 'Community.School')

for (i in cat_cols)
{
  #print(i)
  df_expl[, i] = as.factor(df_expl[, i])
  
}

## Columns selected for first round of estimating correlation

col_for_corr = c("Economic.Need.Index", "School.Income.Estimate", "Percent.ELL", "Percent.Asian",
                 "Percent.Black", "Percent.Hispanic", "Percent.Black...Hispanic", "Percent.White",
                 "Student.Attendance.Rate", "Percent.of.Students.Chronically.Absent", "Rigorous.Instruction..", "Collaborative.Teachers..", "Supportive.Environment..", "Effective.School.Leadership..", "Trust..")
df_for_corr_2 = df_expl[ , col_for_corr]

df_for_corr_2 = df_for_corr_2[complete.cases(df_for_corr_2), ]  ## Removes NA values
df_for_corr_2= df_for_corr_2[(df_for_corr_2$School.Income.Estimate != 'N/A'), ] ## Remove NA values written as 'N/A'
df_for_corr_2= df_for_corr_2[(df_for_corr_2$Economic.Need.Index != 'N/A'), ] ## Remove NA values written as 'N/A'

df_for_corr_2$School.Income.Estimate = (as.character(df_for_corr_2$School.Income.Estimate)) ## Converting from factor to character
#Removing any non-numeric character from 'School Income Estimate' column
df_for_corr_2$School.Income.Estimate = as.numeric(gsub("[^0-9\\.]", "", df_for_corr_2$School.Income.Estimate) ) #Regular expression
df_for_corr_2$Economic.Need.Index= as.numeric(as.character(df_for_corr_2$Economic.Need.Index))

## Removing '%' symbol from the end of cell value and converting it to numeric

for ( j in 3:15)
{
  df_for_corr_2[, j] = as.numeric(sub("%","",df_for_corr_2[,j]))/100
}


corr_mat = cor(df_for_corr_2)
View(corr_mat)
