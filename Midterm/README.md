Analysis of Current Death Counts and Leading Causes in US, focusing on
Covid-19
================
Yumeng Gao
2022-10-23

# Introduction

# Methods

# Preliminary Results

# Conclusion

-   Mortality comparison among different causes by year, sex, age, and
    race.
-   Cause proportions?
-   The influence of Covid-19 causes (multiple & underlying)

> since allcause not equal to the sum of these individual causes,
> proportions were not calculated.

# DELETE

``` r
library(data.table)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(dtplyr)
library(tidyverse)
```

    ## ── Attaching packages
    ## ───────────────────────────────────────
    ## tidyverse 1.3.2 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ stringr 1.4.1
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.2
    ## ✔ readr   2.1.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::between()   masks data.table::between()
    ## ✖ dplyr::filter()    masks stats::filter()
    ## ✖ dplyr::first()     masks data.table::first()
    ## ✖ dplyr::lag()       masks stats::lag()
    ## ✖ dplyr::last()      masks data.table::last()
    ## ✖ purrr::transpose() masks data.table::transpose()

``` r
library(R.utils)
```

    ## Loading required package: R.oo
    ## Loading required package: R.methodsS3
    ## R.methodsS3 v1.8.2 (2022-06-13 22:00:14 UTC) successfully loaded. See ?R.methodsS3 for help.
    ## R.oo v1.25.0 (2022-06-12 02:20:02 UTC) successfully loaded. See ?R.oo for help.
    ## 
    ## Attaching package: 'R.oo'
    ## 
    ## The following object is masked from 'package:R.methodsS3':
    ## 
    ##     throw
    ## 
    ## The following objects are masked from 'package:methods':
    ## 
    ##     getClasses, getMethods
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     attach, detach, load, save
    ## 
    ## R.utils v2.12.0 (2022-06-28 03:20:05 UTC) successfully loaded. See ?R.utils for help.
    ## 
    ## Attaching package: 'R.utils'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract
    ## 
    ## The following object is masked from 'package:utils':
    ## 
    ##     timestamp
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     cat, commandArgs, getOption, isOpen, nullfile, parse, warnings

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:data.table':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(leaflet)
library(webshot)
library(cowplot)
```

    ## 
    ## Attaching package: 'cowplot'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     stamp

``` r
library(ggpubr)
```

    ## 
    ## Attaching package: 'ggpubr'
    ## 
    ## The following object is masked from 'package:cowplot':
    ## 
    ##     get_legend

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(RColorBrewer)
```

> Exploratory Data Analysis

1.  Formulate a question
2.  Read in the data
3.  Check the dimensions and headers and footers of the data
4.  Take a closer look at some/all of the variables
5.  Validate with an external source
6.  Conduct some summary statistics to answer the initial question -
    Make exploratory graphs

### Download dataset from CDC’s website and read it in.

AH Monthly Provisional Counts of Deaths for Select Causes of Death by
Sex, Age, and Race and Hispanic Origin
(<https://data.cdc.gov/NCHS/AH-Monthly-Provisional-Counts-of-Deaths-for-Select/65mz-jvh5>)

Provisional counts of deaths by the month the deaths occurred, by age
group, sex, and race/ethnicity, for select underlying causes of death
for 2020-2021. Final data are provided for 2019. The dataset also
includes monthly provisional counts of death for COVID-19, as an
underlying or multiple cause of death.

``` r
if (!file.exists("deaths.csv")) {
  download.file(
    url = "https://data.cdc.gov/api/views/65mz-jvh5/rows.csv?accessType=DOWNLOAD", "deaths.csv", method = "libcurl", timeout  = 60)
}
ah= data.table::fread("deaths.csv")
```

``` r
org= data.table::fread("deaths.csv")
```

## 

``` r
dim(ah)
```

    ## [1] 3960   24

``` r
head(ah)
```

    ##    AnalysisDate Date Of Death Year Date Of Death Month Start Date   End Date
    ## 1:   10/13/2021               2019                   7 07/01/2019 07/31/2019
    ## 2:   10/13/2021               2019                   9 09/01/2019 09/30/2019
    ## 3:   10/13/2021               2020                   3 03/01/2020 03/31/2020
    ## 4:   10/13/2021               2020                   3 03/01/2020 03/31/2020
    ## 5:   10/13/2021               2020                   3 03/01/2020 03/31/2020
    ## 6:   10/13/2021               2021                   4 04/01/2021 04/30/2021
    ##    Jurisdiction of Occurrence    Sex Race/Ethnicity    AgeGroup AllCause
    ## 1:              United States      M          Other   0-4 years       61
    ## 2:              United States      F          Other 25-34 years       26
    ## 3:              United States Female          Other   0-4 years       40
    ## 4:              United States Female          Other  5-14 years        6
    ## 5:              United States Female          Other 15-24 years       14
    ## 6:              United States   Male          Other   0-4 years       49
    ##    NaturalCause Septicemia (A40-A41) Malignant neoplasms (C00-C97)
    ## 1:           52                    0                             1
    ## 2:            8                    0                             1
    ## 3:           35                    0                             0
    ## 4:            4                    1                             0
    ## 5:            2                    0                             0
    ## 6:           42                    0                             0
    ##    Diabetes mellitus (E10-E14) Alzheimer disease (G30)
    ## 1:                           0                       0
    ## 2:                           0                       0
    ## 3:                           0                       0
    ## 4:                           0                       0
    ## 5:                           0                       0
    ## 6:                           0                       0
    ##    Influenza and pneumonia (J09-J18)
    ## 1:                                 1
    ## 2:                                 0
    ## 3:                                 1
    ## 4:                                 1
    ## 5:                                 0
    ## 6:                                 0
    ##    Chronic lower respiratory diseases (J40-J47)
    ## 1:                                            0
    ## 2:                                            0
    ## 3:                                            0
    ## 4:                                            0
    ## 5:                                            0
    ## 6:                                            0
    ##    Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)
    ## 1:                                                                  1
    ## 2:                                                                  0
    ## 3:                                                                  0
    ## 4:                                                                  0
    ## 5:                                                                  0
    ## 6:                                                                  0
    ##    Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)
    ## 1:                                                                     0
    ## 2:                                                                     0
    ## 3:                                                                     0
    ## 4:                                                                     0
    ## 5:                                                                     0
    ## 6:                                                                     0
    ##    Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)
    ## 1:                                                                                                 6
    ## 2:                                                                                                 0
    ## 3:                                                                                                 7
    ## 4:                                                                                                 0
    ## 5:                                                                                                 1
    ## 6:                                                                                                 6
    ##    Diseases of heart (I00-I09,I11,I13,I20-I51)
    ## 1:                                           0
    ## 2:                                           1
    ## 3:                                           0
    ## 4:                                           0
    ## 5:                                           0
    ## 6:                                           0
    ##    Cerebrovascular diseases (I60-I69) COVID-19 (U071, Multiple Cause of Death)
    ## 1:                                  1                                        0
    ## 2:                                  1                                        0
    ## 3:                                  0                                        0
    ## 4:                                  0                                        0
    ## 5:                                  0                                        0
    ## 6:                                  0                                        1
    ##    COVID-19 (U071, Underlying Cause of Death)
    ## 1:                                          0
    ## 2:                                          0
    ## 3:                                          0
    ## 4:                                          0
    ## 5:                                          0
    ## 6:                                          1

``` r
tail(ah)
```

    ##    AnalysisDate Date Of Death Year Date Of Death Month Start Date   End Date
    ## 1:   10/13/2021               2021                   9 09/01/2021 09/30/2021
    ## 2:   10/13/2021               2021                   9 09/01/2021 09/30/2021
    ## 3:   10/13/2021               2021                   9 09/01/2021 09/30/2021
    ## 4:   10/13/2021               2021                   9 09/01/2021 09/30/2021
    ## 5:   10/13/2021               2021                   9 09/01/2021 09/30/2021
    ## 6:   10/13/2021               2021                   9 09/01/2021 09/30/2021
    ##    Jurisdiction of Occurrence  Sex Race/Ethnicity          AgeGroup AllCause
    ## 1:              United States Male          Other       35-44 years      107
    ## 2:              United States Male          Other       45-54 years      179
    ## 3:              United States Male          Other       55-64 years      237
    ## 4:              United States Male          Other       65-74 years      263
    ## 5:              United States Male          Other       75-84 years      203
    ## 6:              United States Male          Other 85 years and over      116
    ##    NaturalCause Septicemia (A40-A41) Malignant neoplasms (C00-C97)
    ## 1:           95                    0                             6
    ## 2:          168                    1                            15
    ## 3:          225                    2                            34
    ## 4:          258                    2                            51
    ## 5:          200                    1                            35
    ## 6:          116                    3                            13
    ##    Diabetes mellitus (E10-E14) Alzheimer disease (G30)
    ## 1:                           2                       0
    ## 2:                           2                       1
    ## 3:                           7                       0
    ## 4:                          11                       3
    ## 5:                           9                       5
    ## 6:                           2                       7
    ##    Influenza and pneumonia (J09-J18)
    ## 1:                                 0
    ## 2:                                 1
    ## 3:                                 3
    ## 4:                                 4
    ## 5:                                 2
    ## 6:                                 1
    ##    Chronic lower respiratory diseases (J40-J47)
    ## 1:                                            0
    ## 2:                                            0
    ## 3:                                            8
    ## 4:                                            8
    ## 5:                                            6
    ## 6:                                            9
    ##    Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)
    ## 1:                                                                  0
    ## 2:                                                                  0
    ## 3:                                                                  3
    ## 4:                                                                  3
    ## 5:                                                                  4
    ## 6:                                                                  2
    ##    Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)
    ## 1:                                                                     2
    ## 2:                                                                     2
    ## 3:                                                                     2
    ## 4:                                                                     4
    ## 5:                                                                     4
    ## 6:                                                                     1
    ##    Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)
    ## 1:                                                                                                33
    ## 2:                                                                                                34
    ## 3:                                                                                                36
    ## 4:                                                                                                25
    ## 5:                                                                                                17
    ## 6:                                                                                                 1
    ##    Diseases of heart (I00-I09,I11,I13,I20-I51)
    ## 1:                                           9
    ## 2:                                          18
    ## 3:                                          40
    ## 4:                                          60
    ## 5:                                          37
    ## 6:                                          23
    ##    Cerebrovascular diseases (I60-I69) COVID-19 (U071, Multiple Cause of Death)
    ## 1:                                  2                                       27
    ## 2:                                  2                                       70
    ## 3:                                  8                                       55
    ## 4:                                  7                                       55
    ## 5:                                  8                                       36
    ## 6:                                  7                                       18
    ##    COVID-19 (U071, Underlying Cause of Death)
    ## 1:                                         24
    ## 2:                                         68
    ## 3:                                         55
    ## 4:                                         49
    ## 5:                                         34
    ## 6:                                         18

``` r
str(ah)
```

    ## Classes 'data.table' and 'data.frame':   3960 obs. of  24 variables:
    ##  $ AnalysisDate                                                                                     : chr  "10/13/2021" "10/13/2021" "10/13/2021" "10/13/2021" ...
    ##  $ Date Of Death Year                                                                               : int  2019 2019 2020 2020 2020 2021 2019 2019 2019 2019 ...
    ##  $ Date Of Death Month                                                                              : int  7 9 3 3 3 4 1 1 1 1 ...
    ##  $ Start Date                                                                                       : chr  "07/01/2019" "09/01/2019" "03/01/2020" "03/01/2020" ...
    ##  $ End Date                                                                                         : chr  "07/31/2019" "09/30/2019" "03/31/2020" "03/31/2020" ...
    ##  $ Jurisdiction of Occurrence                                                                       : chr  "United States" "United States" "United States" "United States" ...
    ##  $ Sex                                                                                              : chr  "M" "F" "Female" "Female" ...
    ##  $ Race/Ethnicity                                                                                   : chr  "Other" "Other" "Other" "Other" ...
    ##  $ AgeGroup                                                                                         : chr  "0-4 years" "25-34 years" "0-4 years" "5-14 years" ...
    ##  $ AllCause                                                                                         : int  61 26 40 6 14 49 182 44 122 198 ...
    ##  $ NaturalCause                                                                                     : int  52 8 35 4 2 42 162 28 45 100 ...
    ##  $ Septicemia (A40-A41)                                                                             : int  0 0 0 1 0 0 4 1 0 1 ...
    ##  $ Malignant neoplasms (C00-C97)                                                                    : int  1 1 0 0 0 0 2 8 7 29 ...
    ##  $ Diabetes mellitus (E10-E14)                                                                      : int  0 0 0 0 0 0 0 1 1 6 ...
    ##  $ Alzheimer disease (G30)                                                                          : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ Influenza and pneumonia (J09-J18)                                                                : int  1 0 1 1 0 0 4 4 0 5 ...
    ##  $ Chronic lower respiratory diseases (J40-J47)                                                     : int  0 0 0 0 0 0 0 1 2 2 ...
    ##  $ Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)                               : int  1 0 0 0 0 0 1 0 1 3 ...
    ##  $ Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)                            : int  0 0 0 0 0 0 0 0 2 0 ...
    ##  $ Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99): int  6 0 7 0 1 6 22 0 4 2 ...
    ##  $ Diseases of heart (I00-I09,I11,I13,I20-I51)                                                      : int  0 1 0 0 0 0 1 0 6 8 ...
    ##  $ Cerebrovascular diseases (I60-I69)                                                               : int  1 1 0 0 0 0 0 0 1 2 ...
    ##  $ COVID-19 (U071, Multiple Cause of Death)                                                         : int  0 0 0 0 0 1 0 0 0 0 ...
    ##  $ COVID-19 (U071, Underlying Cause of Death)                                                       : int  0 0 0 0 0 1 0 0 0 0 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

## 

Change the names of the key variables so that they are easier to refer
to in the code.

``` r
setnames(ah, old = c('Date Of Death Year', 'Date Of Death Month', 'Race/Ethnicity' ,'Septicemia (A40-A41)', 'Malignant neoplasms (C00-C97)', 'Diabetes mellitus (E10-E14)', 'Alzheimer disease (G30)', 'Influenza and pneumonia (J09-J18)', 'Chronic lower respiratory diseases (J40-J47)', 'Other diseases of respiratory system (J00-J06,J30-J39,J67,J70-J98)', 'Nephritis, nephrotic syndrome and nephrosis (N00-N07,N17-N19,N25-N27)','Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified (R00-R99)', 'Diseases of heart (I00-I09,I11,I13,I20-I51)' ,'Cerebrovascular diseases (I60-I69)' ,'COVID-19 (U071, Multiple Cause of Death)' ,'COVID-19 (U071, Underlying Cause of Death)'), new = c('Y', 'M', 'Race', 'Septicemia', 'Tumor', 'Diabetes', 'Alzheimer', 'FluPneumonia', 'Lower_R','Other_R' ,'Nephrosis', 'Abnormal', 'Heart', 'Cerebrovascular', 'Covid_Multi' ,'Covid_Under'))
```

Categorical Variables

``` r
ah$Year= format(ah$Y,format="%y")
ah$Month= format(ah$M,format="%m")
is.char= sapply(ah, is.character)
cate= ah[, ..is.char]
apply(cate, 2, table)
```

    ## $AnalysisDate
    ## 
    ## 10/13/2021 
    ##       3960 
    ## 
    ## $`Start Date`
    ## 
    ## 01/01/2019 01/01/2020 01/01/2021 02/01/2019 02/01/2020 02/01/2021 03/01/2019 
    ##        120        120        120        120        120        120        120 
    ## 03/01/2020 03/01/2021 04/01/2019 04/01/2020 04/01/2021 05/01/2019 05/01/2020 
    ##        120        120        120        120        120        120        120 
    ## 05/01/2021 06/01/2019 06/01/2020 06/01/2021 07/01/2019 07/01/2020 07/01/2021 
    ##        120        120        120        120        120        120        120 
    ## 08/01/2019 08/01/2020 08/01/2021 09/01/2019 09/01/2020 09/01/2021 10/01/2019 
    ##        120        120        120        120        120        120        120 
    ## 10/01/2020 11/01/2019 11/01/2020 12/01/2019 12/01/2020 
    ##        120        120        120        120        120 
    ## 
    ## $`End Date`
    ## 
    ## 01/31/2019 01/31/2020 01/31/2021 02/28/2019 02/28/2021 02/29/2020 03/31/2019 
    ##        120        120        120        120        120        120        120 
    ## 03/31/2020 03/31/2021 04/30/2019 04/30/2020 04/30/2021 05/31/2019 05/31/2020 
    ##        120        120        120        120        120        120        120 
    ## 05/31/2021 06/30/2019 06/30/2020 06/30/2021 07/31/2019 07/31/2020 07/31/2021 
    ##        120        120        120        120        120        120        120 
    ## 08/31/2019 08/31/2020 08/31/2021 09/30/2019 09/30/2020 09/30/2021 10/31/2019 
    ##        120        120        120        120        120        120        120 
    ## 10/31/2020 11/30/2019 11/30/2020 12/31/2019 12/31/2020 
    ##        120        120        120        120        120 
    ## 
    ## $`Jurisdiction of Occurrence`
    ## 
    ## United States 
    ##          3960 
    ## 
    ## $Sex
    ## 
    ##      F Female      M   Male 
    ##    720   1260    720   1260 
    ## 
    ## $Race
    ## 
    ##                                      Hispanic 
    ##                                           660 
    ## Non-Hispanic American Indian or Alaska Native 
    ##                                           660 
    ##                            Non-Hispanic Asian 
    ##                                           660 
    ##                            Non-Hispanic Black 
    ##                                           660 
    ##                            Non-Hispanic White 
    ##                                           660 
    ##                                         Other 
    ##                                           660 
    ## 
    ## $AgeGroup
    ## 
    ##         0-4 years       15-24 years       25-34 years       35-44 years 
    ##               396               396               396               396 
    ##       45-54 years        5-14 years       55-64 years       65-74 years 
    ##               396               396               396               396 
    ##       75-84 years 85 years and over 
    ##               396               396 
    ## 
    ## $Year
    ## 
    ## 2019 2020 2021 
    ## 1440 1440 1080 
    ## 
    ## $Month
    ## 
    ##   1   2   3   4   5   6   7   8   9  10  11  12 
    ## 360 360 360 360 360 360 360 360 360 240 240 240

Fix Sex’s problem.

``` r
ah= tibble::rowid_to_column(ah, "ID")

for (i in 1:length(ah$ID)){
  if (ah$Sex[i]== 'Female'){
    ah$Sex[i]= 'F'
  } else if (ah$Sex[i]== 'Male'){
    ah$Sex[i]= 'M'}
}

table(ah$Sex)
```

    ## 
    ##    F    M 
    ## 1980 1980

Rename race categories

``` r
for (i in 1:length(ah$ID)){
  if (ah$Race[i]== 'Non-Hispanic American Indian or Alaska Native'){
    ah$Race[i]= 'Indian/Alaska'
  } else if (ah$Race[i]== 'Non-Hispanic Asian'){
    ah$Race[i]= 'Asian'
    } else if (ah$Race[i]== 'Non-Hispanic Black'){
    ah$Race[i]= 'Black'
    } else if (ah$Race[i]== 'Non-Hispanic White'){
    ah$Race[i]= 'White'
}}

table(ah$Race)
```

    ## 
    ##         Asian         Black      Hispanic Indian/Alaska         Other 
    ##           660           660           660           660           660 
    ##         White 
    ##           660

Reorder age groups

``` r
is.factor(ah$AgeGroup)
```

    ## [1] FALSE

``` r
ah$AgeGroup= as.factor(ah$AgeGroup)

levels(ah$AgeGroup)
```

    ##  [1] "0-4 years"         "15-24 years"       "25-34 years"      
    ##  [4] "35-44 years"       "45-54 years"       "5-14 years"       
    ##  [7] "55-64 years"       "65-74 years"       "75-84 years"      
    ## [10] "85 years and over"

``` r
levels(ah$AgeGroup)= c('0-4', '15-24', '25-34', '35-44', '45-54', '5-14', '55-64', '65-74', '75-84', '>=85')

ah$AgeGroup= factor(ah$AgeGroup, levels=c('0-4', '5-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '75-84', '>=85'))

table(ah$AgeGroup)
```

    ## 
    ##   0-4  5-14 15-24 25-34 35-44 45-54 55-64 65-74 75-84  >=85 
    ##   396   396   396   396   396   396   396   396   396   396

-   Noted that sex, age, and race categories all have same sample size

Numerical Variables–\> meaningless?

``` r
summary(ah[,11:25])
```

    ##     AllCause      NaturalCause     Septicemia         Tumor       
    ##  Min.   :    0   Min.   :    0   Min.   :  0.00   Min.   :   0.0  
    ##  1st Qu.:   73   1st Qu.:   45   1st Qu.:  0.00   1st Qu.:   5.0  
    ##  Median :  247   Median :  187   Median :  2.00   Median :  30.0  
    ##  Mean   : 2198   Mean   : 2013   Mean   : 27.07   Mean   : 414.0  
    ##  3rd Qu.: 1424   3rd Qu.: 1111   3rd Qu.: 15.00   3rd Qu.: 212.2  
    ##  Max.   :55227   Max.   :53946   Max.   :484.00   Max.   :6541.0  
    ##     Diabetes         Alzheimer        FluPneumonia        Lower_R      
    ##  Min.   :   0.00   Min.   :   0.00   Min.   :   0.00   Min.   :   0.0  
    ##  1st Qu.:   0.00   1st Qu.:   0.00   1st Qu.:   0.00   1st Qu.:   0.0  
    ##  Median :   8.00   Median :   0.00   Median :   3.00   Median :   4.0  
    ##  Mean   :  66.22   Mean   :  86.25   Mean   :  33.35   Mean   : 103.6  
    ##  3rd Qu.:  50.00   3rd Qu.:   6.00   3rd Qu.:  19.00   3rd Qu.:  22.0  
    ##  Max.   :1039.00   Max.   :4844.00   Max.   :1216.00   Max.   :2408.0  
    ##     Other_R         Nephrosis         Abnormal           Heart        
    ##  Min.   :  0.00   Min.   :  0.00   Min.   :   0.00   Min.   :    0.0  
    ##  1st Qu.:  0.00   1st Qu.:  0.00   1st Qu.:   1.00   1st Qu.:    3.0  
    ##  Median :  2.00   Median :  3.00   Median :   5.00   Median :   29.0  
    ##  Mean   : 30.75   Mean   : 36.04   Mean   :  33.06   Mean   :  465.9  
    ##  3rd Qu.: 14.00   3rd Qu.: 20.00   3rd Qu.:  22.25   3rd Qu.:  212.0  
    ##  Max.   :665.00   Max.   :651.00   Max.   :1308.00   Max.   :11502.0  
    ##  Cerebrovascular   Covid_Multi       Covid_Under     
    ##  Min.   :   0.0   Min.   :    0.0   Min.   :    0.0  
    ##  1st Qu.:   1.0   1st Qu.:    0.0   1st Qu.:    0.0  
    ##  Median :   6.0   Median :    0.0   Median :    0.0  
    ##  Mean   : 107.7   Mean   :  179.2   Mean   :  162.5  
    ##  3rd Qu.:  56.0   3rd Qu.:   37.0   3rd Qu.:   33.0  
    ##  Max.   :3483.0   Max.   :15441.0   Max.   :13510.0

## Year–\> overall trends

Calculate average death counts among different causes by year

``` r
tab1= as.tibble(group_by(ah, Year) %>% 
                       summarize( AllCause= mean(AllCause), NaturalCause= mean(NaturalCause), Septicemia= mean(Septicemia), Tumor= mean(Tumor), Diabetes= mean(Diabetes), Alzheimer= mean(Alzheimer),FluPneumonia= mean(FluPneumonia), Lower_R= mean(Lower_R), Other_R= mean(Other_R) ,Nephrosis= mean(Nephrosis), Abnormal= mean(Abnormal), Heart= mean(Heart), Cerebrovascular= mean(Cerebrovascular), Covid_Multi= mean(Covid_Multi) , Covid_Under= mean(Covid_Under)))
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## Please use `as_tibble()` instead.
    ## The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was generated.

``` r
knitr::kable(tab1, caption= "Table 1. Summary of Average Death Counts for Leading Causes by Year")
```

| Year | AllCause | NaturalCause | Septicemia |    Tumor | Diabetes | Alzheimer | FluPneumonia |   Lower_R |  Other_R | Nephrosis | Abnormal |    Heart | Cerebrovascular | Covid_Multi | Covid_Under |
|:-----|---------:|-------------:|-----------:|---------:|---------:|----------:|-------------:|----------:|---------:|----------:|---------:|---------:|----------------:|------------:|------------:|
| 2019 | 1982.424 |     1807.874 |   26.68750 | 416.3813 | 60.86389 |  84.37431 |     34.56944 | 109.00972 | 30.63194 |  35.80903 | 22.48056 | 457.6410 |        104.1667 |      0.0000 |      0.0000 |
| 2020 | 2354.156 |     2155.908 |   27.86319 | 418.8139 | 71.03819 |  93.25069 |     37.26597 | 106.09444 | 31.37292 |  36.52708 | 23.76597 | 484.8611 |        111.4681 |    267.5396 |    244.0965 |
| 2021 | 2277.359 |     2097.527 |   26.53611 | 404.3130 | 66.94722 |  79.40093 |     26.49722 |  93.01481 | 30.06852 |  35.70741 | 59.57500 | 451.7287 |        107.3963 |    300.2824 |    270.3046 |

Table 1. Summary of Average Death Counts for Leading Causes by Year

Generate a new dataset of average death counts with cause category by
year

``` r
c1= tab1 %>% pivot_longer(cols = c(AllCause, NaturalCause, Septicemia, Tumor, Diabetes, Alzheimer, FluPneumonia, Lower_R, Other_R, Nephrosis, Abnormal, Heart, Cerebrovascular, Covid_Multi, Covid_Under),
                           names_to = "Cause")
```

``` r
f1= c1 %>%
  mutate(Cause= fct_reorder(Cause, desc(value))) %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = Cause, 
                     group = Cause)) +
  geom_line() + 
  geom_point() +
  ylab("Average Death Counts") + 
  theme_linedraw()

grid.arrange(f1, bottom="Figure 1. Trends in death counts by leading causes, from 2019 to 2021.")
```

![](README_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

Zoom in to present those causes with relatively small counts

``` r
z1= f1 +ylim(0, 500)

z1
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

    ## Warning: Removed 6 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

## Month_total–\> more details

Generate new variable for total months counts

``` r
ah[, Month_total := fifelse(Y== 2019, M,
                   fifelse(Y== 2020, M+12, M+24))
    ]

table(ah$Month_total)
```

    ## 
    ##   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20 
    ## 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 120 
    ##  21  22  23  24  25  26  27  28  29  30  31  32  33 
    ## 120 120 120 120 120 120 120 120 120 120 120 120 120

``` r
save= ah
```

Calculate average death counts among different causes by total month

``` r
tab2= as.tibble(group_by(ah, Month_total) %>% 
                       summarize( AllCause= mean(AllCause), NaturalCause= mean(NaturalCause), Septicemia= mean(Septicemia), Tumor= mean(Tumor), Diabetes= mean(Diabetes), Alzheimer= mean(Alzheimer),FluPneumonia= mean(FluPneumonia), Lower_R= mean(Lower_R), Other_R= mean(Other_R) ,Nephrosis= mean(Nephrosis), Abnormal= mean(Abnormal), Heart= mean(Heart), Cerebrovascular= mean(Cerebrovascular), Covid_Multi= mean(Covid_Multi) , Covid_Under= mean(Covid_Under)))
```

Generate a new dataset of average death counts with cause category by
total year

``` r
c2= tab2 %>% pivot_longer(cols = c(AllCause, NaturalCause, Septicemia, Tumor, Diabetes, Alzheimer, FluPneumonia, Lower_R, Other_R, Nephrosis, Abnormal, Heart, Cerebrovascular, Covid_Multi, Covid_Under),
                           names_to = "Cause")
```

``` r
f2= c2 %>%
  mutate(Cause= fct_reorder(Cause, desc(value))) %>%
  ggplot(mapping= aes(x = Month_total, 
                     y = value, 
                     col = Cause, 
                     group = Cause)) +
  geom_line() + 
  geom_point() +
  ylab("Average Death Counts") +
  scale_x_continuous(name= "Total Month", breaks = seq(1, 33, by = 1)) +
  theme_linedraw()

grid.arrange(f2, bottom="Figure 2. Trends in death counts by leading causes for totally 33 months (01/2019-09/2021).")
```

![](README_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

Zoom in to present those causes with relatively small counts

``` r
z2=f2 +ylim(0, 900)

z2
```

    ## Warning: Removed 66 row(s) containing missing values (geom_path).

    ## Warning: Removed 66 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

## delete

``` r
ggarrange(z1, z2, nrow=2, common.legend = TRUE, legend= "right")
```

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 6 row(s) containing missing values (geom_path).

    ## Warning: Removed 6 rows containing missing values (geom_point).

    ## Warning: Removed 66 row(s) containing missing values (geom_path).

    ## Warning: Removed 66 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

For Covid Causes

``` r
f3_1= subset(c1, Cause %in% "Covid_Multi" | Cause %in% "Covid_Under") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = Cause, 
                     group = Cause)) +
  geom_line() + 
  geom_point() +
  ylab("Average Death Counts") + 
  theme_linedraw()

f3_2= subset(c2, Cause %in% "Covid_Multi" | Cause %in% "Covid_Under") %>%
  ggplot(mapping= aes(x = Month_total, 
                     y = value, 
                     col = Cause, 
                     group = Cause)) +
  geom_line() + 
  geom_point() +
  ylab("Average Death Counts") +
  scale_x_continuous(name= "Total Month", breaks = seq(1, 33, by = 1)) +
  theme_linedraw()

f3= ggarrange(f3_1, f3_2, nrow=2, common.legend = TRUE, legend= "right")
grid.arrange(f3, bottom="Figure 3. Trends in death counts by Covid-19, from Jan 2019 to Sep 2021.")
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

## Now visualize Covid-caused death counts by sex, age, and race.

### By sex

``` r
s= as.tibble(group_by(ah, Year, Sex) %>% 
                       summarize( AllCause= mean(AllCause), Covid_Multi= mean(Covid_Multi) , Covid_Under= mean(Covid_Under)))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups`
    ## argument.

``` r
knitr::kable(s, caption= "Table 2. Summary of Covid-caused Average Death Counts by Sex")
```

| Year | Sex | AllCause | Covid_Multi | Covid_Under |
|:-----|:----|---------:|------------:|------------:|
| 2019 | F   | 1918.015 |      0.0000 |      0.0000 |
| 2019 | M   | 2046.833 |      0.0000 |      0.0000 |
| 2020 | F   | 2243.901 |    244.4514 |    220.1292 |
| 2020 | M   | 2464.411 |    290.6278 |    268.0639 |
| 2021 | F   | 2137.974 |    264.1630 |    234.0370 |
| 2021 | M   | 2416.744 |    336.4019 |    306.5722 |

Table 2. Summary of Covid-caused Average Death Counts by Sex

``` r
c_s= s %>% pivot_longer(cols = c(AllCause, Covid_Multi, Covid_Under),
                           names_to = "Cause")
```

``` r
f4_1= subset(c_s, Cause %in% "Covid_Multi") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = Sex, 
                     fill = Sex)) +
  geom_bar(stat='identity', position = "dodge", width= 0.5) +
  ylab("Average Death Counts") +
  scale_color_grey() +
  scale_fill_grey() +
  theme_linedraw() +
  coord_flip()

f4_2= subset(c_s, Cause %in% "Covid_Under") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = Sex, 
                     fill = Sex)) +
  geom_bar(stat='identity', position = "dodge", width= 0.5) +
  ylab("Average Death Counts") +
  scale_color_grey() +
  scale_fill_grey() +
  theme_linedraw() +
  coord_flip() 

f4= ggarrange(f4_1, f4_2, nrow=2, common.legend = TRUE, legend= "right")
grid.arrange(f4, bottom="Figure 4. Covid-caused death counts by sex, from 2019 to 2021.")
```

![](README_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### By age

``` r
a= as.tibble(group_by(ah, Year, AgeGroup) %>% 
                       summarize( AllCause= mean(AllCause), Covid_Multi= mean(Covid_Multi) , Covid_Under= mean(Covid_Under)))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups`
    ## argument.

``` r
knitr::kable(a, caption= "Table 2. Summary of Covid-caused Average Death Counts by Age")
```

| Year | AgeGroup |   AllCause | Covid_Multi | Covid_Under |
|:-----|:---------|-----------:|------------:|------------:|
| 2019 | 0-4      |  170.81250 |   0.0000000 |   0.0000000 |
| 2019 | 5-14     |   38.17361 |   0.0000000 |   0.0000000 |
| 2019 | 15-24    |  206.74306 |   0.0000000 |   0.0000000 |
| 2019 | 25-34    |  410.95833 |   0.0000000 |   0.0000000 |
| 2019 | 35-44    |  576.29167 |   0.0000000 |   0.0000000 |
| 2019 | 45-54    | 1113.84028 |   0.0000000 |   0.0000000 |
| 2019 | 55-64    | 2603.72917 |   0.0000000 |   0.0000000 |
| 2019 | 65-74    | 3858.04861 |   0.0000000 |   0.0000000 |
| 2019 | 75-84    | 4777.96528 |   0.0000000 |   0.0000000 |
| 2019 | \>=85    | 6067.68056 |   0.0000000 |   0.0000000 |
| 2020 | 0-4      |  161.00000 |   0.5347222 |   0.3750000 |
| 2020 | 5-14     |   39.19444 |   0.4722222 |   0.3402778 |
| 2020 | 15-24    |  250.10417 |   4.2638889 |   3.4791667 |
| 2020 | 25-34    |  513.07639 |  18.1944444 |  15.7083333 |
| 2020 | 35-44    |  729.06250 |  47.0694444 |  42.3402778 |
| 2020 | 45-54    | 1333.21528 | 127.1875000 | 118.2291667 |
| 2020 | 55-64    | 3069.50000 | 316.3263889 | 293.4305556 |
| 2020 | 65-74    | 4693.91667 | 571.1875000 | 530.9513889 |
| 2020 | 75-84    | 5715.49306 | 737.5277778 | 675.0347222 |
| 2020 | \>=85    | 7037.00000 | 852.6319444 | 761.0763889 |
| 2021 | 0-4      |  149.30556 |   1.0092593 |   0.6666667 |
| 2021 | 5-14     |   38.60185 |   0.8981481 |   0.6203704 |
| 2021 | 15-24    |  252.41667 |   9.0648148 |   7.7129630 |
| 2021 | 25-34    |  538.61111 |  39.8148148 |  35.1203704 |
| 2021 | 35-44    |  805.20370 |  98.8425926 |  91.1944444 |
| 2021 | 45-54    | 1402.75000 | 235.6111111 | 220.8796296 |
| 2021 | 55-64    | 3122.27778 | 489.8055556 | 454.9722222 |
| 2021 | 65-74    | 4747.54630 | 717.7129630 | 660.5833333 |
| 2021 | 75-84    | 5464.61111 | 746.8611111 | 667.2222222 |
| 2021 | \>=85    | 6252.26852 | 663.2037037 | 564.0740741 |

Table 2. Summary of Covid-caused Average Death Counts by Age

``` r
c_a= a %>% pivot_longer(cols = c(AllCause, Covid_Multi, Covid_Under),
                           names_to = "Cause")
```

``` r
f5_1= subset(c_a, Cause %in% "Covid_Multi") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = AgeGroup, 
                     fill = AgeGroup)) +
  geom_bar(stat='identity', position = "dodge", width= 0.7) +
  ylab("Average Death Counts") +
  scale_color_brewer(palette="PuOr") +
  scale_fill_brewer(palette="PuOr") +
  theme_linedraw()

f5_2= subset(c_a, Cause %in% "Covid_Under") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = AgeGroup, 
                     fill = AgeGroup)) +
  geom_bar(stat='identity', position = "dodge", width= 0.7) +
  ylab("Average Death Counts") +
  scale_color_brewer(palette="PuOr") +
  scale_fill_brewer(palette="PuOr") +
  theme_linedraw()

f5= ggarrange(f5_1, f5_2, nrow=2, common.legend = TRUE, legend= "right")
grid.arrange(f5, bottom="Figure 5. Covid-caused death counts by age, from 2019 to 2021.")
```

![](README_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

### By race

``` r
r= as.tibble(group_by(ah, Year, Race) %>% 
                       summarize( AllCause= mean(AllCause), Covid_Multi= mean(Covid_Multi) , Covid_Under= mean(Covid_Under)))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups`
    ## argument.

``` r
knitr::kable(r, caption= "Table 4. Summary of Covid-caused Average Death Counts by Race")
```

| Year | Race          |    AllCause | Covid_Multi | Covid_Under |
|:-----|:--------------|------------:|------------:|------------:|
| 2019 | Asian         |   293.87500 |     0.00000 |     0.00000 |
| 2019 | Black         |  1444.40833 |     0.00000 |     0.00000 |
| 2019 | Hispanic      |   884.94583 |     0.00000 |     0.00000 |
| 2019 | Indian/Alaska |    75.23750 |     0.00000 |     0.00000 |
| 2019 | Other         |    96.94583 |     0.00000 |     0.00000 |
| 2019 | White         |  9099.13333 |     0.00000 |     0.00000 |
| 2020 | Asian         |   381.62917 |    56.54583 |    53.07500 |
| 2020 | Black         |  1875.12083 |   256.13750 |   235.21250 |
| 2020 | Hispanic      |  1281.43750 |   289.31250 |   273.25000 |
| 2020 | Indian/Alaska |   103.10833 |    19.23333 |    17.77917 |
| 2020 | Other         |   122.26250 |    14.26250 |    13.17500 |
| 2020 | White         | 10361.37917 |   969.74583 |   872.08750 |
| 2021 | Asian         |   378.57778 |    62.75000 |    58.54444 |
| 2021 | Black         |  1761.60000 |   253.45000 |   229.17222 |
| 2021 | Hispanic      |  1290.42222 |   314.92778 |   296.87778 |
| 2021 | Indian/Alaska |   100.41667 |    17.62222 |    16.15000 |
| 2021 | Other         |   120.27222 |    15.22778 |    13.80556 |
| 2021 | White         | 10012.86667 |  1137.71667 |  1007.27778 |

Table 4. Summary of Covid-caused Average Death Counts by Race

``` r
c_r= r %>% pivot_longer(cols = c(AllCause, Covid_Multi, Covid_Under),
                           names_to = "Cause")
```

``` r
f6_1= subset(c_r, Cause %in% "Covid_Multi") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = Race, 
                     fill = Race)) +
  geom_bar(stat='identity', position = "dodge", width= 0.5) +
  ylab("Average Death Counts") +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme_linedraw()

f6_2= subset(c_r, Cause %in% "Covid_Under") %>%
  ggplot(mapping= aes(x = Year, 
                     y = value, 
                     col = Race, 
                     fill = Race)) +
  geom_bar(stat='identity', position = "dodge", width= 0.5) +
  ylab("Average Death Counts") +
  scale_color_brewer(palette="Set2") +
  scale_fill_brewer(palette="Set2") +
  theme_linedraw()

f6= ggarrange(f6_1, f6_2, nrow=2, common.legend = TRUE, legend= "right")
grid.arrange(f6, bottom="Figure 6. Covid-caused death counts by race, from 2019 to 2021.")
```

![](README_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->
