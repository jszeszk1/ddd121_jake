#load co2 dataset
dataset <- read.csv("C:\\Users\\jakes\\Desktop\\DDD-I21\\co2.emissions\\co2.emissions.csv")

#put dataset into object
co2 <- dataset

#load & install  dplyr
install.packages("dplyr")
library(dplyr)

#load & install purrr
install.packages("purrr")
library(purrr)

#load magrittr
install.packages("magrittr")
library(magrittr)

#load in tidyverse
install.packages("tidyverse")
library(tidyverse)

#finding out country names and other rows
co2$country

#Using a binary flag to detect if there are NAs, if 0s then
#there are values that must be dealt with; testing first 4 countries
missing.emissions.by.column <- ifelse(is.na(co2[1:4,]), 0, 1)


#using t() to flip data
co2.flipped <- t(co2)

#assigning row 1 to column names
colnames(co2.flipped) <- co2.flipped[c(1),]

#getting rid of template of colnames in data
co2.flipped <- co2.flipped[-c(1),]


#creating data frame
co2.flipped.dataframe <- data.frame(co2.flipped)

#transfering over previous names for columns and rows
rownames(co2.flipped.dataframe) <- rownames(co2.flipped)
colnames(co2.flipped.dataframe) <- colnames(co2.flipped)

#creating a mirror dataframe with true false values for NAs
first.year <- co2.flipped.dataframe %>% 
              purrr::map_dfr(is.na)  

#assigning the rownames (with the years) to 
#the mirrored df
rownames(first.year) <- years

#creating a new object with the first year
#that the column did not have a missing value
first.year.2 <- rownames(co2.flipped.dataframe)[apply(first.year, 2, which.min)]

#binding first year values into new column
co2.first.year <- cbind(co2, first.year.2)


#turning the transposed data into numerical values with apply
co2.flipped.applied.numeric <- apply(co2.flipped, 2, as.numeric)

#creating an index of all the NA values to replace
index <- is.na(co2.flipped.applied.numeric)

#replacing NAs with super small value
co2.flipped.applied.numeric[index] <- c(0.00000001)

#with NAs gone and numerical values, means of each column can 
#be found using apply
co2.flipped.applied.numeric.means <- apply(X = co2.flipped.applied.numeric, MARGIN = 2, FUN = mean)

#binding mean values into new column
co2.and.first.year.and.co2.flipped.applied.numeric.means <- cbind(co2, first.year.2, co2.flipped.applied.numeric.means)


#turning the first year reports data into character values with 
#apply 
co2.first.year.as.character <- apply(co2.first.year[c(221)], 2, as.character)

#str_sub can be used to remove the xs now since the values are
#characters
co2.flipped.xless.year <- stringr::str_sub(co2.first.year.as.character,2,5)

#binding x-less years into new column
co2.xless.year <- cbind(co2, first.year.2, co2.flipped.applied.numeric.means, 
                        co2.flipped.xless.year)


#using apply to convert character values into numerical values
co2.flipped.prep.mean.replace.numeric <- apply(co2.flipped, 2, 
                                               as.numeric)

#running for loop to replace all the NA values with the means of
#the respective column for later calculation
for (i in 1:ncol(co2.flipped.prep.mean.replace.numeric)){
  co2.flipped.prep.mean.replace.numeric[is.na(co2.flipped.prep.mean.replace.numeric[,i]), i] <- co2.flipped.applied.numeric.means[i]
}

#renaming variable for clarification
co2.flipped.mean.replace.numeric <- co2.flipped.prep.mean.replace.numeric


#binding  the first year reported column and transposed dataset
#for following calculations
co2.flipped.final.bind <- rbind(co2.flipped.mean.replace.numeric, 
                                co2.flipped.xless.year)

#transposing to make renaming easier
co2.final <- t(co2.flipped.final.bind)

#create vector for desired rownames
Years <- as.vector(rownames(co2.flipped))

#carrying out one more bind
co2.flipped.final <- rbind(co2.final, Years)

#assigning the previously deisred names into the colnames 
colnames(co2.flipped.final) <- co2.flipped.final[c(193),]

#getting rid of template of names in data
co2.flipped.final <- co2.flipped.final[-c(193),]

#renaming singular column in preparation for table
colnames(co2.flipped.final)[c(220)] <- "first year reported"


#installing and loading in pander
install.packages("pander")
library(pander)

#installing and loading in ggplot2
install.packages("ggplot2")
library(ggplot2)

#converting first final dataframe values into numerical values
co2.flipped.final.numerical <- apply(co2.flipped.final, 2, 
                                     as.numeric)

#signaling out first year reported values
co2.flipped.final.numerical.vector <- as.vector(co2.flipped.final.numerical[,220])

#creating a count for 1800 to 1850 using a binary flag
co2.flipped.final.numerical.vector.count.1800 <- as.numeric(ifelse(co2.flipped.final.numerical.vector<1850.1 & co2.flipped.final.numerical.vector>1799.9,1,0))
co2.flipped.final.count.1800 <- sum(co2.flipped.final.numerical.vector.count.1800)

#creating a count for 1851 to 1900 using a binary flag
co2.flipped.final.numerical.vector.count.1850 <- as.numeric(ifelse(co2.flipped.final.numerical.vector<1900.1 & co2.flipped.final.numerical.vector>1849.1,1,0))
co2.flipped.final.count.1850 <- sum(co2.flipped.final.numerical.vector.count.1850)

#creating a count for 1901 to 1950
co2.flipped.final.numerical.vector.count.1900 <- as.numeric(ifelse(co2.flipped.final.numerical.vector<1950.1 & co2.flipped.final.numerical.vector>1899.9,1,0))
co2.flipped.final.count.1900 <- sum(co2.flipped.final.numerical.vector.count.1900)

#creating a count for 1951 to 2000 using a binary flag
co2.flipped.final.numerical.vector.count.1950 <- as.numeric(ifelse(co2.flipped.final.numerical.vector<2000.1 & co2.flipped.final.numerical.vector>1949.9,1,0))
co2.flipped.final.count.1950 <- sum(co2.flipped.final.numerical.vector.count.1950)

#creating a count for 2001 to 2018 using a binary flag
co2.flipped.final.numerical.vector.count.2000 <- as.numeric(ifelse(co2.flipped.final.numerical.vector<2018.1 & co2.flipped.final.numerical.vector>2009.9,1,0))
co2.flipped.final.count.2000 <- sum(co2.flipped.final.numerical.vector.count.2000)

#attaching all the previous vectors into a sigular dataframe
co2.report.table <- as.data.frame(cbind(co2.flipped.final.count.1800,
                                        co2.flipped.final.count.1850,
                                        co2.flipped.final.count.1900,
                                        co2.flipped.final.count.1950,
                                        co2.flipped.final.count.2000))

#replacing rowname for clairty in table
rownames(co2.report.table)[c(1)] <- c("Countries Reporting for the First Time")

#fixing column names using rename function
co2.report.table.fixed <- co2.report.table %>% 
  dplyr::rename(`1800 to 1850` = co2.flipped.final.count.1800,
                `1851 to 1900` = co2.flipped.final.count.1850,
                `1901 to 1950` = co2.flipped.final.count.1900,
                `1951 to 2000` = co2.flipped.final.count.1950,
                `2001 to 2018` = co2.flipped.final.count.2000   )

#creating table of the first year reported values
pander(co2.report.table.fixed, style = "rmarkdown", split.table = 120)

#converting numerical vector of the means into a dataframe
co2.flipped.applied.numeric.means.dataframe <- as.data.frame(co2.flipped.applied.numeric.means)

#creating dataframes for 8 OECD countries and 8 non-OECD countries
co2.flipped.OECD.countries <- co2.flipped.applied.numeric.means.dataframe[c(9:10,76,121,31,153,157,184),1]
co2.flipped.non.OECD.countries <- co2.flipped.applied.numeric.means.dataframe[c(32,1,62,80,98,111,68,123),1]

#assigning means to objects for table
co2.flipped.OECD.countries.means <- mean(co2.flipped.OECD.countries)
co2.flipped.non.OECD.countries.means <- mean(co2.flipped.non.OECD.countries)

#creating dataframe of all the previous vectors
co2.mean.table <- as.data.frame(cbind(co2.flipped.non.OECD.countries.means, co2.flipped.OECD.countries.means))

#renaming rowname for clairty in table
rownames(co2.mean.table)[c(1)] <- c("Average CO2 Emissions")

#using rename to fix names for table
co2.mean.table.fixed <- co2.mean.table %>% 
  dplyr::rename(`non-OECD Countries` = co2.flipped.non.OECD.countries.means,
                `OECD COuntries` = co2.flipped.OECD.countries.means,   )

#creating table of the means for each country type
pander(co2.mean.table.fixed, style = "rmarkdown", split.table = 120)


#creating object for x-axis values
country.type <- c(1,2)

#vectorizing the previous object
country.type.vector <- as.vector(country.type)

#getting means of each country type and putting them into object
mean.of.country.type <- c(co2.mean.table.fixed[1,1], 
                          co2.mean.table.fixed[1,2])

#vectorizing the previous object
mean.of.country.type.vector <- as.vector(mean.of.country.type)

#Creating acutal x-axis values in object
country.type.name <- c("non-OECD","OECD")

#vectorizing previous object
country.type.name.vector <- country.type.name

#creating dataframe of previous vectors
co2.mean.graph <- as.data.frame(cbind(country.type.vector, 
                                      mean.of.country.type.vector, 
                                      country.type.name.vector))

#creating bar graph of average emissions by country type
ggplot(data = co2.mean.graph, aes(x = country.type.name.vector, 
                                  y = mean.of.country.type.vector)) + 
                                  geom_bar(stat = "identity", fill = "purple") + 
                                  xlab("Type of Country") + 
                                  ylab("Average CO2 Emissions in Tonnes per Person") + 
                                  ggtitle("non-OECD vs OECD countries on CO2 Emissions")
