---
title: "Untitled"
output: 
  html_document:
    keep_md: true
---



## Read Dataset


```r
#read dataset
df <- read.csv("BankChurners.csv", stringsAsFactors = F)
```

## Including Plots

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.


```r
names(df)
```

```
##  [1] "CLIENTNUM"                                                                                                                         
##  [2] "Attrition_Flag"                                                                                                                    
##  [3] "Customer_Age"                                                                                                                      
##  [4] "Gender"                                                                                                                            
##  [5] "Dependent_count"                                                                                                                   
##  [6] "Education_Level"                                                                                                                   
##  [7] "Marital_Status"                                                                                                                    
##  [8] "Income_Category"                                                                                                                   
##  [9] "Card_Category"                                                                                                                     
## [10] "Months_on_book"                                                                                                                    
## [11] "Total_Relationship_Count"                                                                                                          
## [12] "Months_Inactive_12_mon"                                                                                                            
## [13] "Contacts_Count_12_mon"                                                                                                             
## [14] "Credit_Limit"                                                                                                                      
## [15] "Total_Revolving_Bal"                                                                                                               
## [16] "Avg_Open_To_Buy"                                                                                                                   
## [17] "Total_Amt_Chng_Q4_Q1"                                                                                                              
## [18] "Total_Trans_Amt"                                                                                                                   
## [19] "Total_Trans_Ct"                                                                                                                    
## [20] "Total_Ct_Chng_Q4_Q1"                                                                                                               
## [21] "Avg_Utilization_Ratio"                                                                                                             
## [22] "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1"
## [23] "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2"
```

```r
head(df, 2)
```

```
##   CLIENTNUM    Attrition_Flag Customer_Age Gender Dependent_count
## 1 768805383 Existing Customer           45      M               3
## 2 818770008 Existing Customer           49      F               5
##   Education_Level Marital_Status Income_Category Card_Category Months_on_book
## 1     High School        Married     $60K - $80K          Blue             39
## 2        Graduate         Single  Less than $40K          Blue             44
##   Total_Relationship_Count Months_Inactive_12_mon Contacts_Count_12_mon
## 1                        5                      1                     3
## 2                        6                      1                     2
##   Credit_Limit Total_Revolving_Bal Avg_Open_To_Buy Total_Amt_Chng_Q4_Q1
## 1        12691                 777           11914                1.335
## 2         8256                 864            7392                1.541
##   Total_Trans_Amt Total_Trans_Ct Total_Ct_Chng_Q4_Q1 Avg_Utilization_Ratio
## 1            1144             42               1.625                 0.061
## 2            1291             33               3.714                 0.105
##   Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1
## 1                                                                                                                         9.3448e-05
## 2                                                                                                                         5.6861e-05
##   Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2
## 1                                                                                                                            0.99991
## 2                                                                                                                            0.99994
```

```r
df <- df %>%
  select(-contains('Naive'))
names(df)
```

```
##  [1] "CLIENTNUM"                "Attrition_Flag"          
##  [3] "Customer_Age"             "Gender"                  
##  [5] "Dependent_count"          "Education_Level"         
##  [7] "Marital_Status"           "Income_Category"         
##  [9] "Card_Category"            "Months_on_book"          
## [11] "Total_Relationship_Count" "Months_Inactive_12_mon"  
## [13] "Contacts_Count_12_mon"    "Credit_Limit"            
## [15] "Total_Revolving_Bal"      "Avg_Open_To_Buy"         
## [17] "Total_Amt_Chng_Q4_Q1"     "Total_Trans_Amt"         
## [19] "Total_Trans_Ct"           "Total_Ct_Chng_Q4_Q1"     
## [21] "Avg_Utilization_Ratio"
```

```r
#나이를 나이대로 바꾸기 43 -> 40대
agelabels <- c("20-29","30-39","40-49","50-59","60-69","70-79","80+")
dfage <- df %>%
  mutate(age = cut(Customer_Age, breaks = c(20,30,40,50,60,70,80,100), right = F, labels = agelabels)) %>%
  relocate(age, .after = Customer_Age)
dfage[1:5,]
```

```
##   CLIENTNUM    Attrition_Flag Customer_Age   age Gender Dependent_count
## 1 768805383 Existing Customer           45 40-49      M               3
## 2 818770008 Existing Customer           49 40-49      F               5
## 3 713982108 Existing Customer           51 50-59      M               3
## 4 769911858 Existing Customer           40 40-49      F               4
## 5 709106358 Existing Customer           40 40-49      M               3
##   Education_Level Marital_Status Income_Category Card_Category Months_on_book
## 1     High School        Married     $60K - $80K          Blue             39
## 2        Graduate         Single  Less than $40K          Blue             44
## 3        Graduate        Married    $80K - $120K          Blue             36
## 4     High School        Unknown  Less than $40K          Blue             34
## 5      Uneducated        Married     $60K - $80K          Blue             21
##   Total_Relationship_Count Months_Inactive_12_mon Contacts_Count_12_mon
## 1                        5                      1                     3
## 2                        6                      1                     2
## 3                        4                      1                     0
## 4                        3                      4                     1
## 5                        5                      1                     0
##   Credit_Limit Total_Revolving_Bal Avg_Open_To_Buy Total_Amt_Chng_Q4_Q1
## 1        12691                 777           11914                1.335
## 2         8256                 864            7392                1.541
## 3         3418                   0            3418                2.594
## 4         3313                2517             796                1.405
## 5         4716                   0            4716                2.175
##   Total_Trans_Amt Total_Trans_Ct Total_Ct_Chng_Q4_Q1 Avg_Utilization_Ratio
## 1            1144             42               1.625                 0.061
## 2            1291             33               3.714                 0.105
## 3            1887             20               2.333                 0.000
## 4            1171             20               2.333                 0.760
## 5             816             28               2.500                 0.000
```

```r
#
nrow(df[df$Avg_Utilization_Ratio == "NA",])
```

```
## [1] 0
```

```r
#group by biography info
summary <- dfage %>%
  group_by(Gender, age, Education_Level) %>%
  summarise(mean_util_ratio = mean(Avg_Utilization_Ratio))
```

```
## `summarise()` regrouping output by 'Gender', 'age' (override with `.groups` argument)
```

```r
summary
```

```
## # A tibble: 69 x 4
## # Groups:   Gender, age [11]
##    Gender age   Education_Level mean_util_ratio
##    <chr>  <fct> <chr>                     <dbl>
##  1 F      20-29 College                   0.377
##  2 F      20-29 Graduate                  0.373
##  3 F      20-29 High School               0.328
##  4 F      20-29 Post-Graduate             0.483
##  5 F      20-29 Uneducated                0.256
##  6 F      20-29 Unknown                   0.386
##  7 F      30-39 College                   0.357
##  8 F      30-39 Doctorate                 0.317
##  9 F      30-39 Graduate                  0.342
## 10 F      30-39 High School               0.333
## # … with 59 more rows
```

```r
#group by family info
summary2 <- df %>%
  group_by(Marital_Status, Dependent_count) %>%
  summarise(mean_util_ratio = mean(Avg_Utilization_Ratio))
```

```
## `summarise()` regrouping output by 'Marital_Status' (override with `.groups` argument)
```

```r
summary2
```

```
## # A tibble: 24 x 3
## # Groups:   Marital_Status [4]
##    Marital_Status Dependent_count mean_util_ratio
##    <chr>                    <int>           <dbl>
##  1 Divorced                     0           0.218
##  2 Divorced                     1           0.252
##  3 Divorced                     2           0.256
##  4 Divorced                     3           0.263
##  5 Divorced                     4           0.280
##  6 Divorced                     5           0.237
##  7 Married                      0           0.337
##  8 Married                      1           0.324
##  9 Married                      2           0.282
## 10 Married                      3           0.275
## # … with 14 more rows
```

```r
#group by card info
summary3 <- df %>%
  group_by(Card_Category, Credit_Limit, Total_Revolving_Bal) %>%
  summarise(mean_util_ratio = mean(Avg_Utilization_Ratio))
```

```
## `summarise()` regrouping output by 'Card_Category', 'Credit_Limit' (override with `.groups` argument)
```

```r
summary3
```

```
## # A tibble: 9,432 x 4
## # Groups:   Card_Category, Credit_Limit [6,251]
##    Card_Category Credit_Limit Total_Revolving_Bal mean_util_ratio
##    <chr>                <dbl>               <int>           <dbl>
##  1 Blue                 1438.                   0           0    
##  2 Blue                 1438.                 170           0.118
##  3 Blue                 1438.                 306           0.213
##  4 Blue                 1438.                 312           0.217
##  5 Blue                 1438.                 327           0.227
##  6 Blue                 1438.                 361           0.251
##  7 Blue                 1438.                 455           0.316
##  8 Blue                 1438.                 458           0.318
##  9 Blue                 1438.                 468           0.325
## 10 Blue                 1438.                 479           0.333
## # … with 9,422 more rows
```


