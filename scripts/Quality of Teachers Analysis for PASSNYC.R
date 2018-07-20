## set your working directory
getwd()
setwd("C:\\Users\\vsm397\\Desktop\\Vir's Personal documents\\Pred Modeling (MSBA)\\NYC Schools Data")

## import the data and create a large data frame
main_df <- read.csv("2016 School Explorer.csv",header = T)

##narrow down your data frame to the specific columns of interest 
df_for_corr <- data.frame(main_df$Economic.Need.Index, main_df$Rigorous.Instruction.., main_df$Rigorous.Instruction.Rating, main_df$Collaborative.Teachers.., main_df$Collaborative.Teachers.Rating, main_df$Supportive.Environment.., main_df$Supportive.Environment.Rating)
names(df_for_corr) <- c("Economic Need Index", "Rigorous Instruction %", "Rigorous Instruction Rating", "Collaborative Teachers %", "Collaborative Teachers Rating", "Supportive Environment %", "Supportive Environment Rating")

## clean the data by removing 'N/A' values
df_for_corr <- df_for_corr[complete.cases(df_for_corr), ]
df_for_corr <- df_for_corr[(df_for_corr$`Economic Need Index` != 'N/A'), ]
df_for_corr <- df_for_corr[(df_for_corr$`Rigorous Instruction Rating` != 'N/A'), ]
df_for_corr <- df_for_corr[(df_for_corr$`Collaborative Teachers Rating` != 'N/A'), ]
df_for_corr <- df_for_corr[(df_for_corr$`Supportive Environment Rating` != 'N/A'), ]

## convert percents to decimals
df_for_corr$`Rigorous Instruction %` <- as.numeric(sub("%","",df_for_corr$`Rigorous Instruction %`))/100
df_for_corr$`Collaborative Teachers %` <- as.numeric(sub("%","",df_for_corr$`Collaborative Teachers %`))/100
df_for_corr$`Supportive Environment %` <- as.numeric(sub("%","",df_for_corr$`Supportive Environment %`))/100

## convert categorical variables to numeric (Rigorous Instruction)
temporary_vector_ri <- as.character(df_for_corr$`Rigorous Instruction Rating`)
temporary_vector_ri[ temporary_vector_ri=="Meeting Target"] <- 3
temporary_vector_ri[ temporary_vector_ri=="Approaching Target"] <- 2
temporary_vector_ri[ temporary_vector_ri=="Exceeding Target"] <- 4
temporary_vector_ri[ temporary_vector_ri=="Not Meeting Target"] <- 1
df_for_corr$Rigorous_Instruction_Numeric = temporary_vector_ri

## convert categorical variables to numeric (Collaborative Teachers)
temporary_vector_ct <- as.character(df_for_corr$`Collaborative Teachers Rating`)
temporary_vector_ct[ temporary_vector_ct=="Meeting Target"] <- 3
temporary_vector_ct[ temporary_vector_ct=="Approaching Target"] <- 2
temporary_vector_ct[ temporary_vector_ct=="Exceeding Target"] <- 4
temporary_vector_ct[ temporary_vector_ct=="Not Meeting Target"] <- 1
df_for_corr$Colaborative_Teachers_Numeric = temporary_vector_ct

## convert categorical variables to numeric (Supportive Environment)
temporary_vector_se <- as.character(df_for_corr$`Supportive Environment Rating`)
temporary_vector_se[ temporary_vector_se=="Meeting Target"] <- 3
temporary_vector_se[ temporary_vector_se=="Approaching Target"] <- 2
temporary_vector_se[ temporary_vector_se=="Exceeding Target"] <- 4
temporary_vector_se[ temporary_vector_se=="Not Meeting Target"] <- 1
df_for_corr$Supportive_Environment_Numeric = temporary_vector_se

## create new data frame to remove non-numeric variables
TEST_df <- df_for_corr
TEST_df$`Rigorous Instruction Rating` <- NULL
TEST_df$`Collaborative Teachers Rating` <- NULL
TEST_df$`Supportive Environment Rating` <- NULL

plot(TEST_df)
plot(TEST_df$`Rigorous Instruction %`, TEST_df$`Economic Need Index`)
TEST_df$`Economic Need Index`= as.numeric(as.character(TEST_df$`Economic Need Index`))
plot(TEST_df$`Rigorous Instruction %`, TEST_df$`Economic Need Index`)
plot(TEST_df$`Collaborative Teachers %`, TEST_df$`Economic Need Index`)
plot(TEST_df$`Supportive Environment %`, TEST_df$`Economic Need Index`)

good_schools__high_ENI_df <- TEST_df[TEST_df$`Rigorous Instruction %`>=0.85 & TEST_df$`Collaborative Teachers %`>=0.85 & TEST_df$`Supportive Environment %`>=0.85 & TEST_df$`Economic Need Index`>= 0.70,]
