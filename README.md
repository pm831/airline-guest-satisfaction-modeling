![airline](https://user-images.githubusercontent.com/19572673/62335817-56297980-b49b-11e9-803f-113578df428c.jpg)

# Airline Guest Satisfaction Analysis

## Abstract:
Every day, there are tens of thousands of people who fly across the country and the world, whether it is work-related or for a vacation.
In addition to looking for the best and most convenient fares, customers tend to focus on their airline experience which plays a big part in customer satisfaction. In the Airline Guest Satisfaction Analysis, I look to determine the key drivers from guest satisfaction surveys that translates to a particular customer recommending Airline X. Furthermore, in the analysis, the following questions arise. How much do cleanliness ratings play a part in the overall customer satisfaction?  Which airports are the best at cleaning aircraft? Which airports are the worst? What are factors causing cleanliness ratings to be low in certain airports? Is it overnight stay? What major steps can Airline X take in the airports with the worst cleanliness ratings? All of these questions/problems are being analyzed below.

## Use Case(s):
### Determine the key drivers for likelihood to recommend Airline X
### Which airports are the best at cleaning aircraft?
### Do certain airports impact the aircraft cleanliness ratings more than others?
### What other factors could drive the cleanliness ratings to be low?

## Initial Dataset(s):
## Part 1
### Guest_Satisfaction_Survey_Data.csv 

## Part 2
### Surveys.csv
### Tail_Number_Data.csv
### Overnight_Station_Data.csv
### Airport_Delivery_Dates.csv

## Resulting Dataset(s) - Part 2:
### After transforming/merging data via Surveys.csv, Tail_Number_Data.csv, Overnight_Station_Data.csv, Airport_Delivery_Dates.csv:
### Test.csv is final output dataset which is then visualized in PowerBI. 

## Software:
### PowerBI, R (ggplot2), Excel

## Techniques:
### RandomForest

## Basic Steps (Part 1):
##### Understand the process: Guest Satisfaction Survey Key Factors
##### Understand the airlines industry.
##### Understand/frame the business problem.
##### Explore the data & potential key factors with key business question in mind.
##### Look at all levels of the variables.
##### Transform all 'ordinal' columns into 'numerical columns'
##### Run histograms on transformed columns to see/understand data patterns.
##### Treat missing values by deleting columns that contained > 77% null values
##### Train/validate a Random Forest Regression Model
##### Look at the Variable Importance Plot in R for variable selection to predict the response variable Q1.
##### Upon that, select the top/three factors that best explain the response variable

## Part 1
![Q120](https://user-images.githubusercontent.com/19572673/62090877-7c9da980-b23c-11e9-830e-97b928ba22a0.PNG)
![Q127](https://user-images.githubusercontent.com/19572673/62090878-7c9da980-b23c-11e9-8ec6-fe931ad578eb.PNG)
![rf1model](https://user-images.githubusercontent.com/19572673/62091281-27fb2e00-b23e-11e9-9bbb-6cefcd300fb9.PNG)
![rf1stats](https://user-images.githubusercontent.com/19572673/62091282-27fb2e00-b23e-11e9-9d47-437cfde5addb.PNG)
![rf2model](https://user-images.githubusercontent.com/19572673/62091283-27fb2e00-b23e-11e9-9b48-147e0a89b80d.PNG)
![rf2stats](https://user-images.githubusercontent.com/19572673/62091284-2893c480-b23e-11e9-8a75-492829b76871.PNG)

## Part 1 Analysis

##### Random Forest Results – Key Drivers Interpretation
##### Potential Key Drivers: Q127 and Q120
##### Explain roughly 67.68% of the response variable ‘Q1’ among the training data set.
##### Then predict/validate on the test data set, Adjusted R-square = 0.6896.
##### Fairly consistent measures on both training/test set
##### Overall Goal: Find least number of key factors that explain highest variance of the response variable to avoid multicollinearity

## Basic Steps (Part 2):
##### 1. Copy paste Excel Test file into individual CSV Files and import them into R. Do basic data exploration.
##### 2. Left outer join Tail Number Data so that the specified Tail Numbers would have a cabin cleanliness rating.
##### 3. Concatenate Tail Number and Departure Date in Overnight Station Data for a unique ID.
##### 4. Concatenate Tail Number and Departure Date in Test so you can join Overnight Station Data on Test Data.
##### 5. Get rid of Tail Number, Departure Date in Overnight Station Data to get rid of duplicate columns (when they join)
##### 6. Change the name of "Airport Code Deport" to "Overnight Airport "in Overnight Station Data
##### 7. Left outer join Overnight Station Data on Test to get Overnight Airport matched up with cleanliness rating in Test Data.
##### 8. Left outer join Airport Delivery Dates on Test to get Aircraft Delivery Dates matched up with cleanliness rating in Test Data.
##### 9. Convert cleanliness rating where 0 = filthy and 5 = spotless.
##### 10. Plot histogram to for data exploration using ggplot2.
##### 11.Write to csv and do further analysis in PowerBI.

## Part 2
![CleanlinessRating](https://user-images.githubusercontent.com/19572673/62090876-7c051300-b23c-11e9-9bdb-57aa42d471e3.PNG)
![airlines_dashboard](https://user-images.githubusercontent.com/19572673/62089884-c08eaf80-b238-11e9-8113-083115c46fc0.PNG)

## Summary:

The bottom-line goal is to figure out why certain airports have lower cleanliness ratings than others. Up on the User Interface, there is a 'Flight Time', 'Survey Date', 'Departure Date', 'Aircraft Delivery Date', and 'Date Diff' slicer to which the user can select the appropriate time frame. To drill down, there is a 'Origin' & 'Destination' & 'Overnight_Airport' slicer which location of airport.

When the end-user clicks on one of the options, the entire dashboard/UI drills down further to get into the point of interest, including the 'Stacked-Column-Chart', 'Scatter Chart', and a 'drill-through data table'. The 'Stacked-Column-Chart provides an ordinal structure of the highest average ratings (Overnight Airport) to the lowest. The color schemes 'green', 'yellow', and 'red' are displayed as well. The 'Scatter-Plot' provides the average ratings (Overnight Airport) vs. its volume. The color schemes 'green', 'yellow', and 'red' are displayed as well (by avg. rating). 'The drill-through data table' fixes itself once the user clicks on a point of interest. So from a simple 'business' perspective, the problem solving process to work on the highest volume overnight airport with the lowest avg rating would make the most sense to improve customer satisfaction. 

## Part 2 Analysis

#### Which airports are the best at cleaning aircraft?
##### PBI: 4.33
##### STI: 4.19
##### CAK: 4.13
##### EWR: 4.12
##### MSP: 4.09
##### BDL: 4.08
##### MSY: 4.07
##### RSW: 4.07
##### LGA: 4.04
##### LBE: 4.04

#### Do certain airports impact the aircraft cleanliness ratings more than others?
##### Yes, the four high-volume airports with the lowest aircraft cleaning ratings are MCO (3.92), ORD (3.85), LAS (3.86), BWI (3.88)

#### What other factors could drive the cleanliness ratings to be low?
##### Other factors would be time difference between the Aircraft Delivery Date and Departure Date.
##### Date Difference (in Days) between 0 Days and 1000 Days: 3.99 Rating
##### Date Difference (in Days) between 1000 Days To Max: 3.91 Rating
##### Obviously, further data analysis would allow us to look at certain thresholds with the worst ratings.
