#Data Preparation:
#_________________________________________________________________________
#1. remove na. (done)
#2. impute na  : (done)
#3. finding proportion of NAs (done sum(is.na(x))/nrow(object) with vapply or map FROM purrr)
#4. Diagraming using Amelia package (done in missmap from Amelia)
#5. feature plot (done with featurePlot function it consist of argument x= predictor variables, y=response variables, plot= "the type of plot which we want")
#6. VIF
#7. Transforming variables when needed. (done in preProcessing function in caret PACKAGE)
#8. Deciding on when to drop variable with many NA (not required)
#9. NA when variable is categorical (done with missMDA PACKAGE estim_ncpMCA and MIMCA)
#10. String extraction using rebus and stringr  (done practice to be done)
#11. Efficient use of various stringr functions
#12. Use of gsub for substituting
#13. Use of gather, spread, unite and separate for restructuring the data.
#14. Model that dont need much data preparation. (Check out the machine learning model)
#15. dplyr functionality specific functions
#16. use of map in dplyr
#17. date manipulation with lubridate
#18. How to work on dummy variables is it needed to know
#19. for loop in mutate
#20. use of sqldf in r and comparison of it with. (done)
#21. Imputation of the NA values for 1. Categorical 2. for numeric (done)
#22. Outlier treatment
#23. Scaling and Weighting in R (done with preProcess function in caret where we choose the requisite method)
#24. Oversampling, undersampling or both (using ovun function from ROSE PACKAGE)
#25. factor plot



#Commonly used Packages:
library(dplyr)  #select filter mutate  group_by summarise
library(sqldf)  #Running SQL Queries on R (SQL Codes equivalent to writing dplyr stuff can also be made)
library(stringr)#Contains a lot of string manipulating functions
library(rebus)  #This makes writing regular expressions very easy this is used with stringr package
library(Amelia) #This package contain missmap through which we can view where the missing values are
#its also used to imputation of the missing values

library(readr)  #Importing data from csv and txt into r as tibble dataframe
library(readxl)     #importing excel file
library(data.table) #fread

library(tidyr)  #Unite Separate Gather spread nest unnest to change the table orientation and uniting and separating columns.
library(broom)  #tidy-> used to get overall model parameters in a table format,augment-> used to get data point specific
#parameters like fit, leverage , cook distance
library(purrr)  #Functional programming can be written, map map2 , map_if, safely, invoke_map

library(lubridate) #different date format to ISO 8601 format

library(ggplot2)   #Plotting graphs
library(ggforce)   #Use to develop paginated graphs when using facets(When using facets in cases where the number of
#levels are more we can fix the number of rows, columns and number of pages by it)
library(corrplot)#We can draw correlation plot which is basically a heatmap where darker shade represent higher
#correlation and lighter represent lower order correlation
library(caret)   #When the output variables are numeric we can use it to draw feature plot which is nothing but factor plot
#preProcess
library(scales)  #used with ggplot to provide breaks in the axis






#1. Finding the number of NAs individually
#_________________________________________________________________

a<-c(rnorm(100,mean=200,sd=32),NA,NA,NA,NA,NA,NA) #creating random numbers in r
b<-c(runif(100,min=20,max=300),NA,NA,NA,NA,NA,NA)

a<-as.data.frame(a)
b<-as.data.frame(b)

c<-cbind(a,b)

sum(is.na(c$a)) #Finding the number of NAs
sum(is.na(c$b))/nrow(c) #Finding the proportion of NAs

#If I have 2 data object and I want to find the na in both of them then how to do it ?????



#2. Finding the number of NAs over the columns
#___________________________________________________________________
map(c,function(x)sum(is.na(x))) #Its anonymous function
map(c,~sum(is.na(.)))

#apply :  apply(X, MARGIN( 1 for row, 2 for column), FUN, ...)
#sapply : sapply(X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)

sapply(c,function(x)sum(is.na(x))) #sapply is a user-friendly version and wrapper of lapply by default returning a vector, matrix
apply(c,2,function(x)sum(is.na(x)))



##3 Finding proportion of NAs
map(c,function(x)sum(is.na(x))/nrow(c)*100)


#4. Diagraming using Amelia package
#____________________________________________________________________
#Seeing the graphic of the missing values in R using missmap function in Amelia package in R
missmap(c)


#5. Missing value imputation in R :mice for numeric and missMDA for categorical
#_____________________________________________________________________
#Missing data in the training data set can reduce the power/fit of the model or can lead to a biased
#model because we have not analysed the behavior and the relationship with other variables correctly
#It can lead to wrong prediction or classification.

#Why my data has missing values?
#1. Data extraction: Error at this stage are typically easy to find and corrected.
#2. Data Collection: a. Missing completely at random (probability of missing variable is same for all observation)
#                    b. Missing at random (missing ratio varies for different level and inout variables)
#                    c. Missing that depends on unobserved predictors (eg. if in medical particular diagnostic causes discomfort then there is higher chance of drop out from the study)
#                    d. Missing that depend on missing values itself (eg. People with higher or lower income are likely to provide non-response to their earning)



library(mlbench)
data("PimaIndiansDiabetes")

missmap(PimaIndiansDiabetes) #0 missing values




setwd("C:\\Users\\fz1775\\Desktop\\MACHINE LEARNING\\Testing ML\\Practice Dataset")
bank_data<-read_csv("Bank_data_with_missing.csv")

missmap(bank_data) #Very traces are missing

map(bank_data,function(x)sum(is.na(x))) #number of missing data points
map(bank_data,function(x)sum(is.na(x))/nrow(bank_data)*100) #proportion of missing data points

summary(bank_data)

#For Categorical data
#Based on the problem at hand , we can try to do one of the following
#1. Mode is one of the option which can be used
#2. Missing values can be treated as a separate category by itself, We can create another category
#   for missing values and use them as a different level
#3. If the number of missing values are lesser compared to the number of samples and also number of
#   samples are high, we can also choose to remove those rows in our analysis
#4. We can run model to predict the missing values using the other variables as input

#R provides MICE(Multiple imputation by chained equation) package and Amelia package for handling
#missing values
#for MICE follow the steps below
#1. Change variable(with missing values) into factors by as.factor()
#2. Create a data set of all the known variable and the missing values
#3. Read about the complete() command from the MICE package and apply to the new data set.

#How to treat the missing values in R?
#1. Deletion
#2. Mean Median Mode
#3. Prediction Model
#4. KNN Imputation

#Imputation of Numerical data:

library(mice) # Multivariate Imputation via chained equations
md.pattern(bank_data)  #This can also be used togather with missmap

bank<-mice(bank_data,m=5,maxit=50,meth='pmm',seed=500)
#m -> refers to number of imputed datasets (Number of multiple imputations. The default is m=5.)
#m -> A scalar giving the number of iterations. The default is 5.
#meth="pmm" refers to the imputation method (Predictive mean matching)

summary(bank)

completedata<-complete(bank,1)
map(completedata,function(x)sum(is.na(x))) #We can see the numeric variables are no more empty


#What is MICE?
#Missing data are a common problem in psychiatric research. Multivariate imputation
#by chained equations (MICE), sometimes called “fully conditional specification” or
#“sequential regression multiple imputation”
#While complete case analysis may be easy to implement it relies upon stronger missing data assumptions than multiple imputation and it can result in biased estimates and a reduction in power
# Single imputation procedures, such as mean imputation, are an improvement but do not account for the uncertainty in the imputations; once the imputation is completed, analyses proceed as if the imputed values were the known, true values rather than imputed. This will lead to overly precise results and the potential for incorrect conclusions.
#Maximum likelihood methods are sometimes a viable approach for dealing with missing data (Graham, 2009); however, these methods are primarily available only for certain types of models

#mice package in R:
# The mice package implements a method to deal with missing data. The package creates multiple imputations
# (replacement values) for multivariate missing data. The method is based on Fully Conditional Specification,
# where each incomplete variable is imputed by a separate model. The MICE algorithm can impute mixes of
# continuous, binary, unordered categorical and ordered categorical data. In addition, MICE can impute
# continuous two-level data, and maintain consistency between imputations by means of passive imputation.
# Many diagnostic plots are implemented to inspect the quality of the imputations.




#Multiple imputation has a number of advantages over these other missing data approaches. Multiple imputation involves filling in the missing values multiple times, creating multiple “complete” datasets. Described in detail by Schafer and Graham (2002), the missing values are imputed based on the observed values for a given individual and the relations observed in the data for other participants, assuming the observed variables are included in the imputation model.
#Because multiple imputation involves creating multiple predictions for each missing value, the analyses of multiply imputed data take into account the uncertainty in the imputations and yield accurate standard errors. On a simple level, if there is not much information in the observed data (used in the imputation model) regarding the missing values, the imputations will be very variable, leading to high standard errors in the analyses.

library(missMDA)

#What is missMDA?
#The missMDA package quickly generates several imputed datasets with quantitative variables
#and/or categorical variables. It is based on the
#1. dimentionality reduction method such as PCA for continuous variables or
#2. multiple correspondance analysis for categorical variables.
#Compared to the Amelia and mice, it better handles cases where the number of variables
#is larger than the number of units and cases where regularization is needed. For categorical
#variables, it is particularly interesting with many variables and many levels but also
#with rare level

#Partition the data to categorical only

bank_data_cat<-bank_data%>%select(2:5,7:9,11,16:17)%>%map(.,as.factor)

nb<-estim_ncpMCA(bank_data_cat,ncp.max=5) #Time consuming, nb=4 (Better to convert the data to factor)
#                                          Takes almost 2 hours

res<-MIMCA(bank_data_cat,ncp=4,nboot=1) #MIMCA performs multiple imputations for categorical data using Multiple Correspondence Analysis.
#nboot: the number of imputed datasets it should be 1                                  
a1<-as.data.frame(res$res.MI)  #we can get the imputed data by this step

#We can finally merge the numeric and the categorical togather.

?estim_ncpMCA
?MIMCA

#Numeric Data: mice function then completedata function on the output of mice function.
#              The principle followed with Multiple Imputation with chained equations.
#Categorical Data: select only the categorical data then estim_ncpMCA(categorical_object,ncp.max=5)
#              MCA Stands for multiple correspondance analysis then use MIMCA(bank_data_cat,ncp=4,nboot=10)



#6. Preprocessing of data in R using caret
#_____________________________________________________________________
#library(caret)
preProcessValues_scale<-preProcess(bank_data_num,method="scale") #scale means (x- mean(X)/sd of X)
#Check out which variable to scale and center
preProcessValues_center<-preProcess(bank_data_num,method="center") # center means X-mean(X)

#There are a number of preprocessing methods available in R
#1. BoxCox
#2. YeoJohnson
#3. expoTrans
#4. center
#5. scale
#6. range
#7. pca
#8. ica
#9. corr


#7. Feature Plot in R : This the factor plot
#___________________________________________________________________________-
# Here we first split the data to the response variable (Y) and the predictor variable (X)
# split input and output
y <- bank_data[,14]
x <- bank_data[,c(1:13,15:17)]

# scatterplot matrix / feature plot is same as factor analysis plot
library(ellipse)
featurePlot(x=x, y=y, plot="ellipse")  #ellipse package required


# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")


# density plots for each attribute
featurePlot(x=x, y=y, plot="density")


# pairs plot for each attribute
featurePlot(x=x, y=y, plot="pairs")  #just like ellipse plot with ellipse not present, more like scatter plot




#8. Correlation Matrix and correlation plot in R.
#__________________________________________________________________
correlationMatrix <- cor(bank_data[,c(1,6,10,12:15)]) #Taking only the numeric variables

# summarize the correlation matrix
print(correlationMatrix)

#Plotting the corrplot to remove trhe redundant factors
library(corrplot)
corrplot(correlationMatrix,method="color")


#9. String Manipulation in R using stringr package
#_________________________________________________________
# rebus provides START and END shortcuts to specify regular expressions
# that match the start and end of the string. These are also known as anchors

library(rebus)

#gsub("Patten which we now want to keep","old pattern",Variable_of_interest)


## Join Multiple Strings Into A Single String.+++++++++++++++++++++++
#str_c()   str_c(..., sep = "", collapse = NULL)

a2<-"x"
b2<-c("y",NA)

str_c(a2,b2,sep="~")

#Missing values are contagious so convert NA to "NA" by
str_replace_na(b2) #and then use str_c which essentially means string concatenate



#str_detect(variable,pattern=)  Result: TRUE FALSE    IMPORTANT
#str_subset (variable, pattern=) Result: Only those variable having match with the pattern  IMPORTANT
#str_count (variable,pattern=)  Result: 0 1   IMPORTANT
#str_split(variable,pattern=) Result: split the variable into two parts
#str_replace (variable,pattern="khk", replacement="vjjb")  IMPORTANT


#Like the dplyr the rebus package also help to write text pattern using pipe
# %R%
# START %R% ANY_CHAR %R% one_or_more(DGT)
# %R% END
# optional()
# zero_or_more()
# one_or_more()
# repeated()
# DGT
# WRD
# SPC
# DOLLAR %R% DGT %R% optional(DGT) %R% DOT %R% dgt(2)



#10. sqldf PACKAGE functionalities in R for data manipulation
#______________________________________________________________________

bank_sql1<-sqldf("SELECT * from bank_data")

bank_sql1


CP_State_Lookup<-read_csv("CP_State_Lookup.csv")
CP_Scenario_Lookup<-read_csv("CP_Scenario_Lookup.csv")
CP_Product_Lookup<-read_csv("CP_Product_Lookup.csv")
CP_Industry_Lookup<-read_csv("CP_Industry_Lookup.csv")
CP_Executive_Lookup<-read_csv("CP_Executive_Lookup.csv")
CP_Date_Lookup<-read_csv("CP_Date_Lookup.csv")
CP_Customer_Lookup<-read_csv("CP_Customer_Lookup.csv")
CP_BU_Lookup<-read_csv("CP_BU_Lookup.csv")
CP_RevenueTaxData_Fact<-read_csv("CP_Revenue Tax Data_Fact.csv")

#INNER JOIN: USED TO COMPARE MULTIPLE TABLES, REPORT MATCHING DATA.(Matching data with respect to the variable in ON)
CP_BU_Lookup_1_9<-CP_BU_Lookup%>%filter(`BU Key`>=1&`BU Key`<=9)  
CP_Innerjoin<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact INNER JOIN CP_BU_Lookup_1_9 ON CP_RevenueTaxData_Fact.`BU Key`=CP_BU_Lookup_1_9.`BU Key`")


#OUTER JOINS  :   USED TO COMPARE MULTIPLE TABLES,  REPORT MATCHING  &  MISSING DATA

# LEFT OUTER JOIN :  All Left Table Data + Matching Right Table Data.
#                     Non match Right table data is reported as null.
CP_LeftOuter<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact LEFT JOIN CP_BU_Lookup_1_9 ON CP_RevenueTaxData_Fact.`BU Key`=CP_BU_Lookup_1_9.`BU Key`")

# FULL OUTER JOIN :  Combined output of LEFT OUTER JOIN + RIGHT OUTER JOIN


#CROSS JOIN => all type of combination
CP_CrossJoin<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact CROSS JOIN CP_BU_Lookup_1_9")



#GROUP BY: by using group by alone we get the last data only
CP_Groupby_alone<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact GROUP BY `BU Key`")

#GROUP BY SUM
CP_Groupby_sum_Revenue<-sqldf("SELECT `BU Key`, SUM(Revenue) FROM CP_RevenueTaxData_Fact GROUP BY `BU Key`")


#USING GROUP BY TO GET ONLY UNIQUE VALUES
CP_Groupby_Unique_Values<-sqldf("SELECT `BU Key` FROM CP_RevenueTaxData_Fact GROUP BY `BU Key`")


#RULE :  WHENEVER WE USE GROUP BY THEN COLUMNS USED IN SELECT SHOULD ALSO BE IN GROUP BY
CP_Groupby_Multiple_variable<-sqldf("SELECT `BU Key`, `Customer Key`, SUM(Revenue)
                                    from CP_RevenueTaxData_Fact
                                    GROUP BY `BU Key`, `Customer Key`")

#WHEN WE WANT TO USE FILTER ON AGGREGATE OF COLUMN IN GROUP BY WE USE HAVING
CP_Groupby_Multiple_variable_having<-sqldf("SELECT `BU Key`, `Customer Key`, SUM(Revenue)
                                    from CP_RevenueTaxData_Fact
                                    GROUP BY `BU Key`, `Customer Key`
                                    HAVING SUM(Revenue)<3336145.89")


#UNION MEANS The UNION command combines the result set of two or more SELECT statements (only distinct values)
#The UNION ALL command combines the result set of two or more SELECT statements (allows duplicate values).

CP_RevenueTaxData_Fact_BU1_9<-CP_RevenueTaxData_Fact%>%filter(`BU Key`>=1 & `BU Key` <10)
CP_RevenueTaxData_Fact_BU5_end<-CP_RevenueTaxData_Fact%>%filter(`BU Key`>=5)

#UNION
CP_Revenue_Union<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact_BU1_9 UNION SELECT * FROM CP_RevenueTaxData_Fact_BU5_end")

#UNION ALL
CP_Revenue_Unionall<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact_BU1_9 UNION ALL SELECT * FROM CP_RevenueTaxData_Fact_BU5_end")


#WHERE FOR NON AGGREGATE COLUMN 1st + GROUP BY for AGGREGATED COLUMN 2nd +HAVING
CP_Revenue_where_groupby<-sqldf("SELECT `BU Key`, `Customer Key`, SUM(Revenue)
                                from CP_RevenueTaxData_Fact
                                    WHERE `BU KEY` BETWEEN 1 AND 10
                                    GROUP BY `BU Key`, `Customer Key`
                                    HAVING SUM(Revenue)<3336145.89")


#SUBQUERIES IN SQLDF (SEMI JOIN)
CP_Revenue_Subq1<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact
                        WHERE `BU KEY` IN (SELECT `BU KEY` FROM CP_BU_Lookup_1_9)")


#SUBQUERIES IN SQLDF (ANTI JOIN)
CP_Revenue_Subq12<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact
                        WHERE `BU KEY` NOT IN (SELECT `BU KEY` FROM CP_BU_Lookup_1_9)")

#MORE COMPLICATED SUBQUERIES IN SQLDF
CP_Revenue_Subq123<-sqldf("SELECT * FROM CP_RevenueTaxData_Fact
                        WHERE `BU KEY` IN (SELECT `BU KEY` FROM CP_BU_Lookup_1_9 WHERE Executive_id < 5)")

#cASE ( mindful of `Variable` and 'Text')
CP_Revenue_Case<-sqldf("SELECT `Customer Key`,
                        CASE
                                      WHEN `Customer Key` < 5000 THEN 'CK < 5000'
                                      WHEN `Customer Key` BETWEEN 5000 AND 10000 THEN 'CK BETWEEN 5000 AND 10000'
                                      WHEN `Customer Key` > 10000 THEN 'CK >10000'
                                      ELSE 'CK not in list'
                                      END Ransingh
                                      FROM CP_RevenueTaxData_Fact")







# REQ 1 : HOW TO REPORT ALL COURSES & RESPECTIVE STUDENTS IN EACH COURSE ?
#   SELECT * FROM COURSES
# INNER JOIN
# tblStudents
# ON tblStudents.StdCourse_ID = COURSES.COURSE_ID
#
#
# REQ 2 : HOW TO REPORT ALL COURSES WITH AND WITHOUT STUDENTS ?
#   SELECT * FROM COURSES
# LEFT OUTER JOIN
# tblStudents
# ON COURSES.COURSE_ID = tblStudents.StdCourse_ID
#
# REQ 3 : HOW TO REPORT ALL COURSES WITH AND WITHOUT STUDENTS ?
#   SELECT * FROM  tblStudents
# RIGHT OUTER JOIN
# COURSES
# ON
# COURSES.COURSE_ID = tblStudents.StdCourse_ID
#
#
# -- REQ 4 : HOW TO REPORT LIST OF ALL COURSES WITHOUT STUDENTS?
#   SELECT * FROM COURSES
# LEFT OUTER JOIN
# tblStudents
# ON
# COURSES.COURSE_ID = tblStudents.StdCourse_ID
# WHERE
# tblStudents.StdCourse_ID IS NULL
#
# -- REQ 5 : HOW TO REPORT LIST OF ALL COURSES AND STUDENTS?
#   SELECT * FROM COURSES CROSS JOIN  tblStudents
# SELECT * FROM COURSES CROSS APPLY tblStudents
#
# -- REQ 6 : HOW TO TUNE QUERIES WITH JOINS [FOR BIG TABLES] ?
#   SELECT * FROM COURSES
# INNER MERGE JOIN
# tblStudents
# ON COURSES.COURSE_ID = tblStudents.StdCourse_ID
#
# -- REQ 7 : HOW TO TUNE QUERIES WITH JOINS [FOR SMALL TABLES] ?
#   SELECT * FROM COURSES
# LEFT OUTER LOOP JOIN
# tblStudents
# ON COURSES.COURSE_ID = tblStudents.StdCourse_ID
#
# -- REQ 8 : HOW TO TUNE QUERIES WITH JOINS [FOR HEAP TABLES] ?
#   SELECT * FROM COURSES
# FULL OUTER LOOP JOIN
# tblStudents
# ON
# COURSES.COURSE_ID = tblStudents.StdCourse_ID


# -- QUERY 1: HOW TO REPORT LIST OF ALL POPULATION DETAILS?
#   SELECT * FROM tblPopulation
#
# -- QUERY 2: HOW TO REPORT LIST OF ALL COUNTRY NAMES?
#   SELECT Country FROM tblPopulation
#
# -- QUERY 3: HOW TO REPORT LIST OF ALL UNIQUE COUNTRIES DETAILS?
#   SELECT Country FROM tblPopulation
# GROUP BY Country
#
# -- QUERY 4: HOW TO REPORT TOTAL POPULATION DETAILS?
#   SELECT sum(Population) AS TOTAL_POP FROM tblPopulation
#
# -- QUERY 5: HOW TO REPORT COUNTRY WISE TOTAL POPULATION DETAILS?
#   SELECT COUNTRY, sum(Population) AS TOTAL_POP FROM tblPopulation
# GROUP BY COUNTRY
#
# -- RULE :  WHENEVER WE USE GROUP BY THEN COLUMNS USED IN SELECT SHOULD ALSO BE IN GROUP BY
# SELECT COUNTRY, STATE, sum(Population) AS TOTAL_POP FROM tblPopulation
# GROUP BY COUNTRY
#
# -- RULE : WHENEVER WE USE GROUP BY THEN COLUMNS USED IN SELECT SHOULD ALSO BE INCLUDED IN GROUP BY
#
# -- QUERY 6: HOW TO REPORT COUNTRY WISE, STATE WISE  TOTAL POPULATION?
#   SELECT COUNTRY, STATE, sum(Population) AS TOTAL_POP FROM tblPopulation
# GROUP BY COUNTRY, STATE
#
# -- QUERY 7: HOW TO REPORT COUNTRY WISE, STATE WISE, CITY WISE TOTALS?
#   SELECT COUNTRY, STATE, CITY, sum(Population) AS TOTAL_POP FROM tblPopulation
# GROUP BY COUNTRY, STATE, CITY
#
# -- QUERY 8: HOW TO APPLY CONDITIONS ON GROUP BY DATA ?
#   SELECT COUNTRY, STATE, CITY, sum(Population) AS TOTAL_POP FROM tblPopulation
# GROUP BY COUNTRY , STATE, CITY
# HAVING sum(Population) > 15
#
# -- QUERY 9: HOW TO APPLY CONDITIONS BEFORE AND AFTER GROUP BY ?
#   SELECT COUNTRY, STATE, sum(Population) AS TOTAL_POP FROM tblPopulation
# WHERE  COUNTRY = 'COUNTRY1' -- USED TO SPECIFY CONDITIONS ON NON-AGGREGATE VALUES
# GROUP BY COUNTRY , STATE
# HAVING sum(Population) > 5 -- USED TO SPECIFY CONDITIONS ON AGGREGATE VALUES
#
#
# -- QUERY 10: HOW TO REPORT TOTAL POPULATION USING ROLLUP  ?
#   SELECT  COUNTRY,  SUM(Population) AS TOTAL_POPULATION FROM tblPopulation
# GROUP BY COUNTRY
#
# SELECT  COUNTRY,  SUM(Population) AS TOTAL_POPULATION FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY)
#

#NOT POSSIBLE IN R
# SELECT  COUNTRY,  SUM(Population) AS TOTAL_POPULATION, GROUPING(COUNTRY) FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY)
#
#
# SELECT
# COUNTRY,
# SUM(Population) AS TOTAL_POPULATION, GROUPING(COUNTRY) FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY)
# HAVING GROUPING(COUNTRY) = 0
# UNION ALL
# SELECT
# ISNULL(COUNTRY, 'GRAND TOTAL') AS COUNTRY,
# SUM(Population) AS TOTAL_POPULATION, GROUPING(COUNTRY) FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY)
# HAVING GROUPING(COUNTRY) = 1
#
#
# SELECT
# COUNTRY,
# SUM(Population) AS TOTAL_POPULATION, GROUPING(COUNTRY) FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY)
# HAVING GROUPING(COUNTRY) = 0
# UNION ALL
# SELECT
# COALESCE(COUNTRY, 'GRAND TOTAL') AS COUNTRY,
# SUM(Population) AS TOTAL_POPULATION, GROUPING(COUNTRY) FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY)
# HAVING GROUPING(COUNTRY) = 1
#
# -- IS NULL ISNULL
#
#
#
# SELECT  COUNTRY, STATE,  SUM(Population) AS TOTAL_POPULATION FROM tblPopulation
# GROUP BY ROLLUP(COUNTRY,STATE) -- 11 ROWS
# -- COUNTRY WISE TOTAL + COUNTRY WISE STATE WISE TOTAL
#
#
# SELECT  COUNTRY, STATE,  SUM(Population) AS TOTAL_POPULATION FROM tblPopulation
# GROUP BY CUBE(COUNTRY,STATE) -- 13 ROWS
# -- COUNTRY WISE TOTAL + COUNTRY WISE STATE WISE TOTAL
# -- STATE WISE TOTAL
#

#
# SELECT * FROM CUSTOMERS_DATA
# SELECT * FROM PRODUCTS_DATA
# SELECT * FROM TIME_DATA
# SELECT * FROM SALES_DATA
#
#
# -- QUERY  #1: HOW TO REPORT PRODUCT WISE TOTAL SALES?
# SELECT *
#   FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
#
#
# -- QUERY #2
# SELECT EnglishProductName, SalesAmount
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
#
#
# -- QUERY #3
# SELECT EnglishProductName, SUM(SalesAmount)
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# GROUP BY EnglishProductName
#
#
# -- QUERY #4
# SELECT EnglishProductName, SUM(SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# GROUP BY EnglishProductName
#
#
#
# -- QUERY #5 : HOW TO REPORT PRODUCT WISE TOTAL SALES ABOVE 1000 USD?
# SELECT EnglishProductName, SUM(SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# GROUP BY EnglishProductName
# HAVING SUM(SalesAmount) > 1000  
#
#
# -- QUERY #6 : HOW TO REPORT PRODUCT WISE TOTAL SALES AND TOTAL TAX ABOVE 1000 USD?
# SELECT EnglishProductName,
# SUM(SalesAmount) AS TOTAL_SALES, SUM(TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# GROUP BY EnglishProductName
# HAVING
# SUM(SalesAmount) > 1000  AND SUM(TAXAMT)  > 1000
#
#
#
#
#
# -- QUERY #7 : HOW TO REPORT PRODUCT WISE TOTAL SALES AND TOTAL TAX ABOVE 1000 USD?
# SELECT EnglishProductName, SUM(SalesAmount) AS TOTAL_SALES, SUM(TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# GROUP BY EnglishProductName
# HAVING
# SUM(SalesAmount) > 1000  AND SUM(TAXAMT)  > 1000
# ORDER BY TOTAL_SALES DESC
#
#
# -- QUERY #8 : HOW TO REPORT PRODUCT WISE TOTAL SALES AND TOTAL TAX ABOVE 1000 USD?
# SELECT EnglishProductName, SUM(SalesAmount) AS TOTAL_SALES, SUM(TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# GROUP BY EnglishProductName
# HAVING
# SUM(SalesAmount) > 1000  AND SUM(TAXAMT)  > 1000
# ORDER BY 2 DESC -- ORDERING THE DATA BY USING COLUMN CARDINAL POSITION.
#
#
#
#
#
#
#
# -- QUERY 9:  WRITE A QUERY TO REPORT SUM OF SALES AND TAX FOR PRODUCTS WITH MAXIMUM DEALER PRICE ?
#   SELECT EnglishProductName, SUM(SalesAmount) AS TOTAL_SALES, SUM(TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# WHERE -- FOR CONDITIONS ON NON-AGGREGATE COLUMNS
# PRODUCTS_DATA.DealerPrice
# IN ( SELECT MAX(DealerPrice) FROM PRODUCTS_DATA)
# GROUP BY EnglishProductName
#
#
#
#
# -- QUERY 10: HOW TO REPORT SUM OF SALES FOR PRODUCTS WITH MAXIMUM DEALER PRICE BUT NOT FOR MINIMAL LIST PRICE ?
#   -- NESTED SUB QUERY
# SELECT EnglishProductName, SUM(SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# WHERE
# PRODUCTS_DATA.DealerPrice
# IN ( SELECT MAX(DealerPrice) FROM PRODUCTS_DATA
#      WHERE  LISTPRICE
#      NOT IN ( SELECT MIN(LISTPRICE) FROM PRODUCTS_DATA ) )
# GROUP BY EnglishProductName
#
#
#
# -- EXAMPLES TO JOIN MORE THAN TWO TABLES:
#   SELECT * FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
#
#
#
# SELECT * FROM SALES_DATA
# INNER JOIN
# PRODUCTS_DATA
# ON
# SALES_DATA.ProductKey = PRODUCTS_DATA.ProductKey
# INNER JOIN
# TIME_DATA
# ON
# SALES_DATA.ORDERDATEKEY = TIME_DATA.TIMEKEY
#
#
# -- Q1: HOW TO REPORT YEAR WISE TOTAL SALES?
#   -- Q2:  HOW TO REPORT YEAR WISE, QUARTER WISE TOTAL SALES AND TOTAL TAX?
#   -- Q3:  HOW TO REPORT YEAR WISE, QUARTER WISE, MONTH WISE TOTAL SALES AND TOTAL TAX?
#   -- Q4:  HOW TO REPORT YEAR WISE, QUARTER WISE TOTAL SALES AND TOTAL TAX FOR JUNE MONTH ?
#   -- Q5:  HOW TO REPORT CLASS WISE, COLOR WISE PRODUCTS FOR EACH YEAR BASED ON ASC ORDER OF SALES?
#   -- Q6: HOW TO REPORT TOTAL SALES FOR SUCH PRODUCTS WITH MAXIMUM NUMBER OF SALES?
#   -- Q7: HOW TO REPORT TOTAL SALES FOR SUCH PRODUCTS EXCEPT WITH MINIMUM NUMBER OF SALES?
#   -- Q8: HOW TO COMBINE THE RESULTS FROM ABOVE TWO QUERIES.
# -- Q9: HOW TO ADDRESS POSSIBLE BLOCKING ISSUES FROM ABOVE TWO QUERIES?
#   -- Q10: HOW TO REPORT YEAR WISE, CUSTOMER WISE, PRODUCT WISE TOTAL SALES AND TOTAL TAX ABOVE 1000 USD?
#  
#  
#  
#   -- Q1: HOW TO REPORT YEAR WISE TOTAL SALES?
#   SELECT T.CalendarYear, SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN TIME_DATA AS T
# ON
# S.OrderDateKey = T.TimeKey
# GROUP BY T.CalendarYear
#
#
# -- Q2:  HOW TO REPORT YEAR WISE, QUARTER WISE TOTAL SALES AND TOTAL TAX?
#   SELECT T.CalendarYear, T.CalendarQuarter,
# SUM(S.SalesAmount) AS TOTAL_SALES, SUM(S.TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA  AS S
# INNER JOIN TIME_DATA AS T
# ON
# S.OrderDateKey = T.TimeKey
# GROUP BY T.CalendarYear, T.CalendarQuarter
#
#
#
# -- Q3:  HOW TO REPORT YEAR WISE, QUARTER WISE, MONTH WISE TOTAL SALES AND TOTAL TAX?
#   SELECT T.CalendarYear, T.CalendarQuarter, T.EnglishMonthName,
# SUM(S.SalesAmount) AS TOTAL_SALES, SUM(S.TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA  AS S
# INNER JOIN TIME_DATA AS T
# ON
# S.OrderDateKey = T.TimeKey
# GROUP BY T.CalendarYear, T.CalendarQuarter,  T.EnglishMonthName
#
# -- Q4:  HOW TO REPORT YEAR WISE, QUARTER WISE TOTAL SALES AND TOTAL TAX FOR JUNE MONTH ?
#   SELECT T.CalendarYear, T.CalendarQuarter,
# SUM(S.SalesAmount) AS TOTAL_SALES, SUM(S.TAXAMT) AS TOTAL_TAX
# FROM SALES_DATA  AS S
# INNER JOIN TIME_DATA AS T
# ON
# S.OrderDateKey = T.TimeKey
# WHERE T.EnglishMonthName = 'JUNE'
# GROUP BY T.CalendarYear, T.CalendarQuarter
#
#
# -- Q5:  HOW TO REPORT CLASS WISE, COLOR WISE PRODUCTS FOR EACH YEAR BASED ON ASC ORDER OF SALES?
#   SELECT
# P.Class, P.Color, T.CalendarYear,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN TIME_DATA AS T
# ON
# S.OrderDateKey = T.TimeKey
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# GROUP BY P.Class, P.Color, T.CalendarYear
#
#
# -- Q6: HOW TO REPORT TOTAL SALES FOR SUCH PRODUCTS WITH MAXIMUM NUMBER OF SALES?
#  
#   -- STEP 1: IDENTIFY THE PRODUCTS THAT HAVE MAX SALE VALUE:
#   SELECT
# P.EnglishProductName,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# GROUP BY P.EnglishProductName
#
# CREATE VIEW VW_SALE_PROIDUCTS
# AS
# SELECT
# P.EnglishProductName,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# GROUP BY P.EnglishProductName
#
#
# SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
# WHERE TOTAL_SALES = (SELECT MAX(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
#
# -- STEP 2:
#   SELECT
# P.EnglishProductName, P.Color, P.Class,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# WHERE
# P.EnglishProductName IN (
#   SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
#   WHERE TOTAL_SALES = (SELECT MAX(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
# )
# GROUP BY P.EnglishProductName, P.Color, P.Class
#
#
#
#
# -- Q7: HOW TO REPORT TOTAL SALES FOR SUCH PRODUCTS EXCEPT WITH MINIMUM NUMBER OF SALES?
#   SELECT
# P.EnglishProductName, P.Color, P.Class,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# WHERE
# P.EnglishProductName NOT IN (
#   SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
#   WHERE TOTAL_SALES = (SELECT MIN(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
# )
# GROUP BY P.EnglishProductName, P.Color, P.Class
#
#
#
# -- Q8: HOW TO COMBINE THE RESULTS FROM ABOVE TWO QUERIES ?
#   SELECT
# P.EnglishProductName, P.Color, P.Class,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# WHERE
# P.EnglishProductName NOT IN (
#   SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
#   WHERE TOTAL_SALES = (SELECT MIN(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
# )
# OR
# P.EnglishProductName IN (
#   SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
#   WHERE TOTAL_SALES = (SELECT MAX(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
# )
# GROUP BY P.EnglishProductName, P.Color, P.Class
#
#
#
# -- Q9: HOW TO ADDRESS POSSIBLE BLOCKING ISSUES FROM ABOVE TWO QUERIES?
#   SELECT
# P.EnglishProductName, P.Color, P.Class,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S (READPAST)
# INNER JOIN PRODUCTS_DATA AS P (READPAST)
# ON
# P.ProductKey = S.ProductKey
# WHERE
# P.EnglishProductName NOT IN (
#   SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
#   WHERE TOTAL_SALES = (SELECT MIN(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
# )
# OR
# P.EnglishProductName IN (
#   SELECT EnglishProductName FROM VW_SALE_PROIDUCTS
#   WHERE TOTAL_SALES = (SELECT MAX(TOTAL_SALES) FROM VW_SALE_PROIDUCTS)
# )
# GROUP BY P.EnglishProductName, P.Color, P.Class
#
#
#
# -- Q10: HOW TO REPORT YEAR WISE, CUSTOMER WISE, PRODUCT WISE TOTAL SALES AND TOTAL TAX ABOVE 1000 USD?
#   SELECT
# T.CalendarYear,  C.FirstName + '  ' + C.LastName AS FULLNAME, P.EnglishProductName,
# SUM(S.SalesAmount) AS TOTAL_SALES
# FROM SALES_DATA  AS S
# INNER JOIN TIME_DATA AS T
# ON
# S.OrderDateKey = T.TimeKey
# INNER JOIN CUSTOMERS_DATA AS C
# ON
# C.CustomerKey = S.CustomerKey
# INNER JOIN PRODUCTS_DATA AS P
# ON
# P.ProductKey = S.ProductKey
# GROUP BY T.CalendarYear,  C.FirstName + '  ' + C.LastName, P.EnglishProductName
# HAVING SUM(S.SalesAmount) > 1000
#
#
# /*
#   NORMAL FORMS : A MECHANISM TO IDENTIFY THE TABLES, RELATIONS AND DATA TYPES.
# ENSURE PROPER DIVSION OF BUSINESS DATA INTO MULTIPLE TABLES.
#
# 1 NF : FIRST NORMAL FORM. EVERY COLUMN SHOULD BE ATOMIC. MEANS, STORES SINGLE VALUE.
#
# 2 NF : SECOND NORMAL FORM. EVERY TABLE SHOULD BE IN FIRST NORMAL FORM
# EVERY TABLE SHOULD BE HAVING A CANDIDATE KEY. USED FOR FUNCTIONAL DEPENDANCY.
#
# 3 NF : THIRD NORMAL FORM. EVERY TABLE SHOULD BE IN SECOND NORMAL FORM
# EVERY TABLE SHOULD BE HAVING A FOREIGN KEY. USED FOR MULTI-VALUED DEPENDANCY.
#
# BCNF NF : BOYCE-CODD NORMAL FORM. EVERY TABLE SHOULD BE IN THIRD NORMAL FORM
# EVERY TABLE SHOULD BE HAVING MORE THAN ONE FOREIGN KEY. USED FOR MULTI-VALUED DEPENDANCY.
# AND MANY TO ONE RELATION.
#
# 4 NF : FOURTH NORMAL FORM. EVERY TABLE SHOULD BE IN THIRD NORMAL FORM
# AND ATLEAST ONE SELF REFERENCE. MEANS A TABLE REFERENCING ITSELF. */



# SELECT * FROM tblPopulation WHERE COUNTRY = 'COUNTRY1'
#
# CREATE VIEW VW_COUNTRY1
# AS
# SELECT * FROM tblPopulation WHERE COUNTRY = 'COUNTRY1'
#
# SELECT * FROM VW_COUNTRY1
#
# -- MAIN PURPOSE OF VIEWS :  TO STORE QUERIES FOR EASY END USER ACCESS.
#
# -- WHENEVER WE CREATE A DATABASE, SET OF PREDEFINED VIEWS ARE AUTO CREATED [SYSTEM VIEWS]
# -- HOW TO REPORT LIST OF DATABASES IN A SERVER?
#   SELECT * FROM SYS.DATABASES
#
# -- HOW TO REPORT LIST OF TABLES IN A DATABASE?
#   SELECT * FROM SYS.TABLES -- REPORTS TABLES IN THE CURRENT DATABASE  [2P]
# SELECT * FROM UNIVERSITY_DATABASE.SYS.TABLES -- REPORTS TABLES IN THE SPECIFIED DATABASE [3P]
#
# -- HOW TO REPORT LIST OF PRIMARY KEYS, FOREIGN KEYS, CHECK CONSTRAINTS, ETC IN A DATABASE?
#   SELECT * FROM SYS.OBJECTS
#
# -- HOW TO REPORT LIST OF COLUMNS FOR ALL TABLES & VIEWS & FUNCTIONS IN THE CURRENT DATABASE?
#   SELECT * FROM INFORMATION_SCHEMA.COLUMNS
#
#
# CREATE FUNCTION fn_ReportDetails ( @country varchar(30) )   -- @country is a PARAMETER. INPUT VALUE
# RETURNS table
# AS
# RETURN
# (
#   SELECT * FROM tblPopulation WHERE COUNTRY = @country  -- PARAMETERIZED QUERY
# )
#
# SELECT * FROM fn_ReportDetails('COUNTRY1')
# SELECT * FROM fn_ReportDetails('COUNTRY2')
#
# -- MAIN PURPOSE OF FUNCTIONS : COMPUTATIONS (CALCULATIONS), DYNAMIC REPORTING
#
# CREATE PROCEDURE usp_ReportDetails ( @country varchar(30) )   -- @country is a PARAMETER. INPUT VALUE
# AS
# SELECT * FROM tblPopulation WHERE COUNTRY = @country  -- PARAMETERIZED QUERY
#
# EXECUTE usp_ReportDetails 'COUNTRY1'
# EXEC usp_ReportDetails 'COUNTRY2'
#
# -- MAIN PURPOSE OF STORED PROCEDURES (SPROCs) :  PROGRAMMING, QUERY TUNING [QUERIES CAN EXECUTE BETTER]
#
# -- ADVANTAGE OF STORED PROCEDURES OVER FUNCTIONS: SPs ARE PRE-COMPILED AND STORED FOR READY EXECUTIONS.
# FUNCTIONS NEED TO GET COMPILED EVERY TIME WE EXECUTE.
# COMPILATION : CONVERT FROM HIGH LEVEL SQL TO MACHINE CODE.
#
# -- ADVANTAGE OF FUNCTIONS OVER STORED PROCEDURES: FUNCTIONS ARE EXECUTED WITH REGULAR SELECT STATEMENT
# HENCE FLEXIBLE FOR DATA ACCESS, REPORTING, CALCULATIONS.
#
#
# -- SYSTEM STORED PROCEDURES:
#   EXEC SP_HELPDB 'OBJECT_OVERVIEW' -- REPORTS DETAILS OF THE GIVEN DATABASE INCLUDING SIZE & FILES
# EXEC SP_HELP 'tblPopulation' -- REPORTS TABLE DEFINITION
# EXEC SP_HELPTEXT 'usp_ReportDetails' -- REPORTS VIEW, FUNCTION, PROCEDURE DEFINITION
# EXEC SP_DEPENDS 'tblPopulation' -- REPORTS THE OBJECT DEPENDANCIES ON THE TABLE
# EXEC SP_RENAME 'tblPopulation', 'tblPopulation_NEW' -- TO RENAME A PROCEDURE
# EXEC SP_RECOMPILE 'usp_ReportDetails' -- RECOMPILES THE STORED PROCEDURE NEXT TIME WE EXECUTE IT
# -- RECOMPILATION REQUIRED IF UNDERLYING TABLE STRUCUTRE CHANGES.
#
#
#
# SELECT @@VERSION
# SELECT @@SERVERNAME