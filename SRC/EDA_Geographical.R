## To find the pattern based on geographical location 
## Based on Latitude and Longitude and Zip codes

nyc_base <- ggmap::get_map("New York City", zoom = 13)
ggmap(nyc_base) + geom_point(data=df_expl, aes(x=as.numeric(Longitude), y=as.numeric(Latitude)), color="red", size=5, alpha=0.5)
 #+ coord_cartesian(ylim = c(40,42))

library(ggmap)

## Seeing if there's a correlation btw lat long and ethinicities

cols = c('Latitude', 'Longitude', "Percent.ELL", "Percent.Asian", "Percent.Black", "Percent.Hispanic",
         "Percent.Black...Hispanic" , "Percent.White")
df_for_corr_latlong = df_expl[, cols]
for (j in 3:8)
{
df_for_corr_latlong[,j] = (as.character(df_for_corr_latlong[,j])) ## COnverting from factor to character
#Removing any non-numeric character from 'School Income Estimate' column
df_for_corr_latlong[,j] = as.numeric(gsub("[^0-9\\.]", "", df_for_corr_latlong[,j]) ) #Regular expression
df_for_corr_latlong[, j]= as.numeric(sub("%","",df_for_corr_latlong[,j]))/100
#df_for_corr_latlong[,j] = as.numeric(as.character(df_for_corr_latlong[,j]))
}

corr_mat_latlong =  cor(df_for_corr_latlong)

View(corr_mat_latlong)

## Seeing how schools are distributed across cities

counts = table(df_expl$City)

counts = counts[order(counts[,2]), ascending=FALSE]
View(counts)
counts = as.data.frame(counts)
counts = counts[order(counts$Freq, decreasing = T), ]
counts[(counts$Freq < 6), 1] = 'OTHER'
counts[(18:40), 1] = 'OTHER'
class(counts[1,1])

counts$Var1 = as.character(counts$Var1)
counts[(18:40), 1] = 'OTHER'

barplot(counts, main = "School distribution by cities")
counts = as.table(counts)

counts$Freq = counts$Freq/(sum(counts$Freq))
major_cities = c('BROOKLYN', 'BRONX', 'NEW YORK', 'STATEN ISLAND')
nrow(df_expl)

sum(df_expl$City %in% major_cities)
df_expl$City[993]

## create a dataframe with major cities only
df_major_cities = df_expl[(df_expl$City %in% major_cities),]



class(df_major_cities$Community.School.)
df_major_cities$Community.School.[(df_major_cities$Community.School. == 'Yes')] = 1
df_major_cities$Community.School.[(df_major_cities$Community.School. == 'No')] = 2

boxplot(df_major_cities$Economic.Need.Index[(df_major_cities$Community.School. == 'Yes')])
boxplot(df_major_cities$Economic.Need.Index)

class(df_major_cities$Economic.Need.Index)
 df_major_cities$Economic.Need.Index = as.numeric(as.character(df_major_cities$Economic.Need.Index))

 df_major_cities = df_major_cities[complete.cases(df_major_cities), ]
 df_major_cities$Economic.Need.Index = as.numeric(as.character(df_major_cities$Economic.Need.Index))
 boxplot(Economic.Need.Index ~ Community.School., data = df_major_cities, col = 'lightgray', border ="blue"
         , main = "Variation of Economic index vs Community schools/Non community school", 
         xlab = "Community School", ylab = "Economic need Index")

 
 
 
 ## Boxplot btw cities and EcoIndex
 df_major_cities$City = factor(df_major_cities$City)
 unique(df_major_cities$City)
 boxplot(Economic.Need.Index ~ City, data = df_major_cities, col = 'green', border ="blue"
         , main = "Variation of Economic index vs Cities", 
         xlab = "Cities", ylab = "Economic need Index")
 
 
unique(df_major_cities$City)

## exploring ethnicities across community schools

for (j in 19:24)
{
  df_major_cities[,j] = (as.character(df_major_cities[,j])) ## COnverting from factor to character
  #Removing any non-numeric character from 'School Income Estimate' column
  df_major_cities[,j] = as.numeric(gsub("[^0-9\\.]", "", df_major_cities[,j]) ) #Regular expression
  df_major_cities[, j]= as.numeric(sub("%","",df_major_cities[,j]))/100
  #df_for_corr_latlong[,j] = as.numeric(as.character(df_for_corr_latlong[,j]))
}

boxplot(Percent.ELL ~ Community.School., data = df_major_cities, col = 'green', border ="blue"
        , main = "Variation of Economic index vs ELL", 
        xlab = "CommunitySchool", ylab = "Percent ELL")


boxplot(Percent.ELL ~ Community.School., data = df_major_cities, col = 'pink', border ="black"
        , main = "Variation of Economic index vs ELL", 
        xlab = "CommunitySchool", ylab = "Percent ELL")


boxplot(Percent.ELL ~ Community.School., data = df_major_cities, col = 'green', border ="blue"
        , main = "Variation of Economic index vs ELL", 
        xlab = "CommunitySchool", ylab = "Percent ELL")


boxplot(Percent.ELL ~ Community.School., data = df_major_cities, col = 'green', border ="blue"
        , main = "Variation of Economic index vs ELL", 
        xlab = "CommunitySchool", ylab = "Percent ELL")


boxplot(Percent.ELL ~ Community.School., data = df_major_cities, col = 'green', border ="blue"
        , main = "Variation of Economic index vs ELL", 
        xlab = "CommunitySchool", ylab = "Percent ELL")


## melt
melt_test = melt(df_high_ENI, id.vars = "School.Name", measure.vars= c("Percent.ELL", 
                                                                           "Percent.Asian",
                                                                           "Percent.Black",
                                                                           "Percent.Hispanic",
                                                                           "Percent.Black...Hispanic",
                                                                           "Percent.White"),
                                                                           variable.name = "Percentage")
library(ggplot2)



melt_test2 = melt(df_major_cities, id.vars = "School.Name", measure.vars= "Community.School.",
                 variable.name = "CommunitySch")
dm = merge(melt_test, melt_test2, by.x ="School.Name", by.y = "School.Name"  )

ggplot(data = dm, aes(x=Ethnicity, y=ValuePerc),
       main ="Ethinicities distributed across comm/non-comm schools") + geom_boxplot(aes(fill=Idx))


dm_location_code = merge(df_expl, df_regis, by.x = "Location.Code", by.y = "DBN")


## Checking for special characteristics in Bronx as it performs the worst in terms of EII

nyc_base <- ggmap::get_map("New York City", zoom = 9)
ggmap(nyc_base) + geom_point(data=df_expl[df_expl$City == 'BRONX', ], aes(x=as.numeric(Longitude), y=as.numeric(Latitude)), color="red", size=5, alpha=0.5)
#+ coord_cartesian(ylim = c(40,42))

## ethniities divided across major cities
 dm_melt = melt(df_major_cities, id.vars = "School.Name", measure.vars = "City", variable.name = "Cityis")
 
dm = merge(melt_test, dm_melt, by.x ="School.Name", by.y = "School.Name" )

ggplot(data = dm, aes(x=Percentage, y=value.x),
       main ="Ethinicities distributed across Cities") + geom_boxplot(aes(fill=value.y))


