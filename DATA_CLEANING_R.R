
library(readxl)

data <- read_excel("name.xlsx")

dim(data)

# To check the missing values

summary(data) #in summary we can view NA's value
 
# Check for missing values
# Find the total number of missing values
total = sum(is.na(data))
print(total)

colSums(is.na(data))

# to fill the missing values
# replace the missing values with the median

New_df = data[,2:12]

New_df$Presentation  = ifelse(is.na(New_df$Presentation), median(New_df$Presentation,
                                                                 na.rm = TRUE),New_df$Presentation)

New_df$Influencing.and.Convincing  = ifelse(is.na(New_df$Influencing.and.Convincing), 
                                            median(New_df$Influencing.and.Convincing, na.rm = TRUE),
                                            New_df$Influencing.and.Convincing)


New_df$Stress.Tolerance = ifelse(is.na(New_df$Stress.Tolerance), median(New_df$Stress.Tolerance, 
                                                                         na.rm = TRUE),New_df$Stress.Tolerance)

New_df$Achievement.Orientation  = ifelse(is.na(New_df$Achievement.Orientation), 
                                         median(New_df$Achievement.Orientation,
                                        na.rm = TRUE),New_df$Achievement.Orientation )

# again to check for missing values
total = sum(is.na(New_df))
print(total)

summary(New_df)

# to check for outliers
boxplot(New_df)

col = c('Presentation','Influencing.and.Convincing','Stress.Tolerance','Achievement.Orientation')
boxplot(New_df[,c('Presentation','Influencing.and.Convincing','Stress.Tolerance','Achievement.Orientation')])

for (x in c('Presentation','Influencing.and.Convincing','Stress.Tolerance','Achievement.Orientation'))
{
  value =New_df[,x][New_df[,x] %in% boxplot.stats(New_df[,x])$out]
  New_df[,x][New_df[,x] %in% value] = NA
} 

# Checking whether the outliers in the above defined columns are replaced by NULL or not
as.data.frame(colSums(is.na(New_df)))

# Removing the null values
library(tidyr)
New_df = drop_na(New_df)
as.data.frame(colSums(is.na(New_df)))
