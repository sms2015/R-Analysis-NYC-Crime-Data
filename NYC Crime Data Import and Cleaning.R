# Import Data ####

# set the directory
setwd('D:/GitHub/R-Analysis-NYC-Crime-Data')

#disable scientific notation
options(scipen=999)

# About the data
# This dataset includes all valid felony, misdemeanor, and violation crimes reported to the New York City Police Department (NYPD) from 2006 to the end of 2016.

# Read the data into a dataframe
filename <- "D:/Data/crime_data/NYPD_Complaint_Data_Historic.csv"

# view the headers and the first row of data
df_data_view <- read.csv(file = filename,stringsAsFactors = F,nrows=1)
View(df_data_view)
# get the column names
colnames(df_data_view)
# get the data types of each column
sapply(df_data_view, class)
# get detailed information about the data
str(df_data_view)

# set up a function to convert the data fields to date types
setClass('myDate')
setAs("character","myDate", function(from) as.Date(from, format="%m/%d/%Y") )

df_test_date_type <- read.csv(file = filename,stringsAsFactors = F,
               colClasses = c(CMPLNT_NUM = 'character',CMPLNT_FR_DT = 'myDate',CMPLNT_TO_DT = 'myDate',RPT_DT = 'myDate'),nrows=1)
str(df_test_date_type)

df_test_rows <- read.csv(file = filename,stringsAsFactors = F,
                              colClasses = c(CMPLNT_NUM = 'character',CMPLNT_FR_DT = 'myDate',CMPLNT_TO_DT = 'myDate',RPT_DT = 'myDate'),nrows=10)
View(df_test_rows)

# import full dataset check import time
start_time <- Sys.time()
df <- read.csv(file <- filename,stringsAsFactors = F,
                         colClasses = c(CMPLNT_NUM = 'character',CMPLNT_FR_DT = 'myDate',CMPLNT_TO_DT = 'myDate',RPT_DT = 'myDate'))
end_time <- Sys.time()
View(df)
# time to load data
end_time - start_time

# Data Cleaning ####
# check for errors, deal with special values, convert data into different formats, and perform calculations

# check if the complaint number is unique
nrow(df)
length(unique(df$CMPLNT_NUM))
dup_index <- anyDuplicated(df$CMPLNT_NUM)
# find the duplicate
dup_cmplnt_num <- df[dup_index,]$CMPLNT_NUM
View(df[df$CMPLNT_NUM == dup_cmplnt_num,])

# There is one duplicate ID - but they are different years so it shouldn't affect the results

# Examine dates reported

# add a year reported column
df$RPT_DT_Y <- format(df$RPT_DT,"%Y")

# add a month reported column
df$RPT_DT_M <- format(df$RPT_DT,"%m")

agg_by_yr <- aggregate(CMPLNT_NUM ~ RPT_DT_Y, FUN = length, data=df)
agg_by_yr
#compare totals
sum(agg_by_yr$CMPLNT_NUM)
nrow(df)

# RPT_DT appears to be clean

# Check Precinct by borough - this should be unique
prec_by_boro <- aggregate(CMPLNT_NUM ~ ADDR_PCT_CD+BORO_NM, FUN = length, data=df)
prec_by_boro <- prec_by_boro[order(prec_by_boro$ADDR_PCT_CD),]
prec_by_boro

# create an accurate list of precinct by boro, using the boro with the most observations per precinct
# keep precinct/boro pair where the count is greater than 5
prec_boro <- prec_by_boro[(prec_by_boro$CMPLNT_NUM >5) & (prec_by_boro$BORO_NM != ""),]
prec_boro
#change the boro column name for later merge
colnames(prec_boro) <- c('ADDR_PCT_CD','BORO_NM_2','CMPLNT_NUM')

# There are either errors in the borough or in the precinct for some entries as a precinct can only be in one borough
# Precinct is missing for some observations
# We can check for the correct precinct and boro for the lat/lon, and correct these values

# Use geocoding to correctly locate the data ####

# shortcut ####

# to continue drop the rows where the precinct or boro is blank
df2 <- df[(df$BORO_NM !="") & (df$ADDR_PCT_CD !=""),]
nrow(df)
nrow(df2)

# get the correct boro based on precinct and compare to actual boro

# subset prec_boro columns to just precinct and boro
prec_boro_2 <- prec_boro[,c('ADDR_PCT_CD','BORO_NM_2')]
View(prec_boro_2)

# merge the dataframes to get the correct boro as a column
# df3 <- merge(df2,prec_boro_2, by = 'ADDR_PCT_CD', all.x=TRUE)

# plyr join is more efficient than merge
# import plyr library
library(plyr)

# join the dataframes
df3 <- plyr::join(df2, prec_boro_2, by = 'ADDR_PCT_CD', type = "left")
View(df3[0:20,])

# remove rows where BORO_NM does not match BORO_NM_2 (the correct boro)
df4 <- df3[df3$BORO_NM == df3$BORO_NM_2,]
View(df4[0:20,])
# check number of rows
nrow(df)
nrow(df3)
nrow(df4)

# remove rows with NA values in BORO_NM and BORO_NM_2 - complete.cases is too slow

# import data.table library to use the more efficient na.omit function in data.table
library(data.table)

# convert to a data.table
setDT(df4)
# check conversion
class(df4)

# remove rows where boro or precinct is NA
df5 = na.omit(df4, cols=c("BORO_NM","ADDR_PCT_CD"))
nrow(df4)
nrow(df5)
# 3 rows were removed

#write the clean dataframe to csv
outfile = "D:/Data/crime_data/NYPD_Complaint_Data_clean.csv"
fwrite(df5, file = outfile,row.names=FALSE)

