## Cherry's test Script
## EDA based on various grades and student performance in them

##getwd()
##setwd('C:\\Users\\cherr\\Desktop\\UT Austin\\Summer Courses\\Predictive Modelling\\PassNYC')

#Loading the file in a dataframe
df_2016_expl = read.csv('2016 School Explorer.csv', header = T)
name_of_cols = names(df_2016_expl)

#Taking columns of interest
df_2016_grade1 = df_2016_expl[,c(17,18,40:161)]

#Remove NA and N/A values + Cleaning data

df_2016_grade1 = df_2016_grade1[complete.cases(df_2016_grade1), ]  ## Removes NA values
df_2016_grade1= df_2016_grade1[(df_2016_grade1$School.Income.Estimate != 'N/A'), ] ## Remove NA values written as 'N/A'
df_2016_grade1= df_2016_grade1[(df_2016_grade1$Economic.Need.Index != 'N/A'), ] ## Remove NA values written as 'N/A'
df_2016_grade1= df_2016_grade1[(df_2016_grade1$Average.ELA.Proficiency != 'N/A'), ] ## Remove NA values written as 'N/A'
df_2016_grade1= df_2016_grade1[(df_2016_grade1$Average.Math.Proficiency != 'N/A'), ] ## Remove NA values written as 'N/A'

df_2016_grade1$School.Income.Estimate = (as.character(df_2016_grade1$School.Income.Estimate)) ## COnverting from factor to character
#Removing any non-numeric character from 'School Income Estimate' column
df_2016_grade1$School.Income.Estimate = as.numeric(gsub("[^0-9\\.]", "", df_2016_grade1$School.Income.Estimate) ) #Regular expression
df_2016_grade1$Economic.Need.Index= as.numeric(as.character(df_2016_grade1$Economic.Need.Index))
df_2016_grade1$Average.ELA.Proficiency= as.numeric(as.character(df_2016_grade1$Average.ELA.Proficiency))
df_2016_grade1$Average.Math.Proficiency= as.numeric(as.character(df_2016_grade1$Average.Math.Proficiency))


# Rounding up Average ELA and Maths proficiency
df_2016_grade1$Average.ELA.Proficiency = round(df_2016_grade1$Average.ELA.Proficiency,0)
df_2016_grade1$Average.Math.Proficiency = round(df_2016_grade1$Average.Math.Proficiency,0)



# Calculating ratio/percent for all Grade columns
for (i in c(6,16,26,36,46,56,66,76,86,96,106,116)){
for (j in 1:8){
  df_2016_grade1[,i+j]= df_2016_grade1[,i+j]/df_2016_grade1[,i]
  }
df_2016_grade1[,i]=df_2016_grade1[,i]/df_2016_grade1[,i-1]
}

# Taking data columns with NaN values
df_2016_grade2 = df_2016_grade1[,1:4]
df_2016_grade2 = df_2016_grade2[complete.cases(df_2016_grade2),]

# Taking different data combinations
df_2016_grade3els = df_2016_grade1[,c(1:4,5:14)]
df_2016_grade3els = df_2016_grade3els[complete.cases(df_2016_grade3els),]
df_2016_grade3m = df_2016_grade1[,c(1:4,15:24)]
df_2016_grade3m = df_2016_grade3m[complete.cases(df_2016_grade3m),]


df_2016_grade4els = df_2016_grade1[,c(1:4,25:34)]
df_2016_grade4els = df_2016_grade4els[complete.cases(df_2016_grade4els),]
df_2016_grade4m = df_2016_grade1[,c(1:4,35:44)]
df_2016_grade4m = df_2016_grade4m[complete.cases(df_2016_grade4m),]

df_2016_grade5els = df_2016_grade1[,c(1:4,45:54)]
df_2016_grade5els = df_2016_grade5els[complete.cases(df_2016_grade5els),]
df_2016_grade5m = df_2016_grade1[,c(1:4,55:64)]
df_2016_grade5m = df_2016_grade5m[complete.cases(df_2016_grade5m),]

df_2016_grade6els = df_2016_grade1[,c(1:4,65:74)]
df_2016_grade6els = df_2016_grade6els[complete.cases(df_2016_grade6els),]
df_2016_grade6m = df_2016_grade1[,c(1:4,75:84)]
df_2016_grade6m = df_2016_grade6m[complete.cases(df_2016_grade6m),]

df_2016_grade7els = df_2016_grade1[,c(1:4,85:94)]
df_2016_grade7els = df_2016_grade7els[complete.cases(df_2016_grade7els),]
df_2016_grade7m = df_2016_grade1[,c(1:4,95:104)]
df_2016_grade7m = df_2016_grade7m[complete.cases(df_2016_grade7m),]

df_2016_grade8els = df_2016_grade1[,c(1:4,105:114)]
df_2016_grade8els = df_2016_grade8els[complete.cases(df_2016_grade8els),]
df_2016_grade8m = df_2016_grade1[,c(1:4,115:124)]
df_2016_grade8m = df_2016_grade8m[complete.cases(df_2016_grade8m),]


# Correlation btw ENI and Proficiency in ELA and Maths

cor_mat = cor(df_2016_grade2[,c(1:4)])

# Only with grade 3 els & maths
cor_mat_g9 = cor(df_2016_grade3els)
cor_mat_g10 = cor(df_2016_grade3m)
pairs(cor_mat_g9)
pairs(cor_mat_g10)

# Only with grade 4 els & maths
cor_mat_g11 = cor(df_2016_grade4els)
cor_mat_g12 = cor(df_2016_grade4m)
pairs(cor_mat_g11)
pairs(cor_mat_g12)

# Only with grade 5 els & maths
cor_mat_g13 = cor(df_2016_grade5els)
cor_mat_g14 = cor(df_2016_grade5m)
pairs(cor_mat_g13)
pairs(cor_mat_g14)

# Only with grade 6 els & maths
cor_mat_g15 = cor(df_2016_grade6els)
cor_mat_g16 = cor(df_2016_grade6m)
pairs(cor_mat_g15)
pairs(cor_mat_g16)

# Only with grade 7 els & maths
cor_mat_g17 = cor(df_2016_grade7els)
cor_mat_g18 = cor(df_2016_grade7m)
pairs(cor_mat_g17)
pairs(cor_mat_g18)

# Only with grade 8 els & maths
cor_mat_g19 = cor(df_2016_grade8els)
cor_mat_g20 = cor(df_2016_grade8m)
pairs(cor_mat_g19)
pairs(cor_mat_g20)