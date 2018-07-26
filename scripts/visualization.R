## Geographical Analysis of two target variable
## Data preparation first! 

library(reshape2)
library(ggplot2)
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

## 1. boxplot cities vs ENI and ELA
boxplot(EconomicNeedIndex ~ City, data = df_exp, col = 'orange', border ="blue"
        , main = "Variation of Economic index vs Cities", 
        xlab = "Cities", ylab = "Economic need Index")

boxplot(AverageELAProficiency ~ City, data = df_exp, col = 'orange', border ="blue"
        , main = "Average English Learning Proficiency vs Cities", 
        xlab = "Cities", ylab = "Average English Learning Proficiency" )


## 2. Community Schools

boxplot(AverageELAProficiency ~ CommunitySchool, data = df_exp, col = 'orange', border ="blue"
        , main = "Average English Learning Proficiency vs Community School", 
        xlab = "Is a Community School", ylab = "Average English Learning Proficiency" )

boxplot(EconomicNeedIndex ~ CommunitySchool, data = df_exp, col = 'orange', border ="blue"
        , main = "Variation of Economic index vs Community School", 
        xlab = "Is a Community School", ylab = "Variation of Economic index" )

## 3. City vs Community School

counts = table(df_exp$CommunitySchool, df_exp$City)
barplot(counts, main = "City vs Community Schools in them", legend = rownames(counts))

## 4. Ethnicity vs Community School
d1.melt = melt(df_exp, id.vars = "SchoolName", measure.vars= "CommunitySchool",
               variable.name = "CommunitySch")

d2.melt = melt(df_exp, id.vars = "SchoolName", measure.vars= c("PercentELL", 
                                                                             "PercentAsian",
                                                                             "PercentBlack",
                                                                             "PercentHispanic",
                                                                             "PercentBlackHispanic",
                                                                             "PercentWhite"),
                                                              variable.name = "Ethnicity")

dm = merge(d1.melt, d2.melt, by.x = "SchoolName", by.y = "SchoolName")
ggplot(data = dm, aes(x=Ethnicity, y=value.y),
       main ="Ethinicities distributed across comm/non-comm schools") + geom_boxplot(aes(fill=value.x))

## 5. Ethnicity vs Cities

d1.melt = melt(df_exp, id.vars = "SchoolName", measure.vars= "City",
               variable.name = "Cities")

dm = merge(d1.melt, d2.melt, by.x = "SchoolName", by.y = "SchoolName")
ggplot(data = dm, aes(x=Ethnicity, y=value.y),
       main ="Ethinicities distributed across Cities of NYC") + geom_boxplot(aes(fill=value.x))+ labs(x = 'Ethnicities',
                                                                                                      y = 'Percentage of ethnicity',
                                                                                                      title = 'Ethinicities distributed across Cities of NYC',
                                                                                                      fill = "City")
                                                                            






