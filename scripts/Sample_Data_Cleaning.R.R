
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


### new addition

## cleaning the major dataframe
major_cities = c("NEW YORK" ,"BRONX","BROOKLYN","STATEN ISLAND")
df_major_cities = df_expl[(df_expl$City %in% major_cities),] ## Main dataframe '2016 explorer... '

cols_to_clean = c( "Rigorous.Instruction..", "Collaborative.Teachers.." ,
                  "Supportive.Environment..", "Trust..",
                  "Strong.Family.Community.Ties..", "Effective.School.Leadership.."
                  )

for (j in cols_to_clean)
{
  df_major_cities[,j] = (as.character(df_major_cities[,j])) ## Converting from factor to character
  #Removing any non-numeric character from 'School Income Estimate' column
  df_major_cities[,j] = as.numeric(gsub("[^0-9\\.]", "", df_major_cities[,j]) ) #Regular expression
  df_major_cities[, j]= as.numeric(sub("%","",df_major_cities[,j]))/100
}


#plot(df_major_cities$Effective.School.Leadership.., df_major_cities$Economic.Need.Index)


idx = ((df_major_cities$Economic.Need.Index >= 0.7)&(df_major_cities$Collaborative.Teachers.. >= 0.85)
  &(df_major_cities$Supportive.Environment.. >= 0.85)&(df_major_cities$Trust.. >= 0.85)
  &(df_major_cities$Strong.Family.Community.Ties.. >= 0.85)&(df_major_cities$Effective.School.Leadership.. >= 0.85)&(df_major_cities$Rigorous.Instruction.. >= 0.85))
 #sum(idx)


df_high_ENI = df_major_cities[idx, ]

ggplot(data = melt_test, aes(x=Percentage, y= value),
       main ="Ethinicities distributed across ") + geom_boxplot(aes())