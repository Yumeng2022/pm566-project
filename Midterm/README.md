PM566 Midterm Project
================
Yumeng Gao
2022-10-18

### Prepare the library–\> DELETE?

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
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
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

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

# Data Wrangling

### Download dataset from CDC’s website and read it in.

DASH YRBSS - Alcohol and Other Drug Use (MS):
<https://chronicdata.cdc.gov/Youth-Risk-Behaviors/DASH-YRBSS-Alcohol-and-Other-Drug-Use-MS-/bdg5-rnzz>

-   This dataset was subtracted from DASH - Youth Risk Behavior
    Surveillance System (YRBSS): Middle School.

``` r
if (!file.exists("YRBSS_MS.csv")) {
  download.file(
    url = "https://chronicdata.cdc.gov/api/views/bdg5-rnzz/rows.csv?accessType=DOWNLOAD", "YRBSS_MS.csv", method = "libcurl", timeout  = 60)
}
ms= data.table::fread("YRBSS_MS.csv")
```

# Exploratory Data Analysis

1.  Formulate a question
2.  Read in the data
3.  Check the dimensions and headers and footers of the data

``` r
dim(ms)
```

    ## [1] 33408    35

``` r
head(ms)
```

    ##    YEAR LocationAbbr                     LocationDesc DataSource
    ## 1: 2017           GU                             Guam      YRBSS
    ## 2: 2011           NM                       New Mexico      YRBSS
    ## 3: 2013           ND                     North Dakota      YRBSS
    ## 4: 2017           PW                            Palau      YRBSS
    ## 5: 2011           CM Charlotte-Mecklenburg County, NC      YRBSS
    ## 6: 2009           NV                           Nevada      YRBSS
    ##                         Topic    Subtopic ShortQuestionText
    ## 1: Alcohol and Other Drug Use Alcohol Use  Ever alcohol use
    ## 2: Alcohol and Other Drug Use Alcohol Use  Ever alcohol use
    ## 3: Alcohol and Other Drug Use Alcohol Use  Ever alcohol use
    ## 4: Alcohol and Other Drug Use Alcohol Use  Ever alcohol use
    ## 5: Alcohol and Other Drug Use Alcohol Use  Ever alcohol use
    ## 6: Alcohol and Other Drug Use Alcohol Use  Ever alcohol use
    ##    Greater_Risk_Question           Description Data_Value_Symbol
    ## 1:    Ever drank alcohol other than a few sips                 %
    ## 2:    Ever drank alcohol other than a few sips                 %
    ## 3:    Ever drank alcohol other than a few sips                 %
    ## 4:    Ever drank alcohol other than a few sips                 %
    ## 5:    Ever drank alcohol other than a few sips                 %
    ## 6:    Ever drank alcohol other than a few sips                 %
    ##    Data_Value_Type Greater_Risk_Data_Value
    ## 1:      Percentage                      NA
    ## 2:      Percentage                      NA
    ## 3:      Percentage                      NA
    ## 4:      Percentage                      NA
    ## 5:      Percentage                      NA
    ## 6:      Percentage                      NA
    ##    Greater_Risk_Data_Value_Footnote_Symbol Greater_Risk_Data_Value_Footnote
    ## 1:                                      NA                               NA
    ## 2:                                      NA                               NA
    ## 3:                                      NA                               NA
    ## 4:                                      NA                               NA
    ## 5:                                      NA                               NA
    ## 6:                                      NA                               NA
    ##    Greater_Risk_Low_Confidence_Limit Greater_Risk_High_Confidence_Limit
    ## 1:                                NA                                 NA
    ## 2:                                NA                                 NA
    ## 3:                                NA                                 NA
    ## 4:                                NA                                 NA
    ## 5:                                NA                                 NA
    ## 6:                                NA                                 NA
    ##    Lesser_Risk_Question Lesser_Risk_Data_Value
    ## 1:  Never drank alcohol                     NA
    ## 2:  Never drank alcohol                     NA
    ## 3:  Never drank alcohol                     NA
    ## 4:  Never drank alcohol                     NA
    ## 5:  Never drank alcohol                     NA
    ## 6:  Never drank alcohol                     NA
    ##    Lesser_Risk_Data_Value_Footnote_Symbol Lesser_Risk_Data_Value_Footnote
    ## 1:                                     NA                              NA
    ## 2:                                     NA                              NA
    ## 3:                                     NA                              NA
    ## 4:                                     NA                              NA
    ## 5:                                     NA                              NA
    ## 6:                                     NA                              NA
    ##    Lesser_Risk_Low_Confidence_Limit Lesser_Risk_High_Confidence_Limit
    ## 1:                               NA                                NA
    ## 2:                               NA                                NA
    ## 3:                               NA                                NA
    ## 4:                               NA                                NA
    ## 5:                               NA                                NA
    ## 6:                               NA                                NA
    ##    Sample_Size    Sex                             Race Grade
    ## 1:          30   Male                            Asian   8th
    ## 2:          14  Total                            Asian   8th
    ## 3:           0   Male                            White   6th
    ## 4:           1 Female                            Asian   8th
    ## 5:          16   Male                            Asian   7th
    ## 6:           8  Total American Indian or Alaska Native   8th
    ##                                  GeoLocation TopicId SubTopicID QuestionCode
    ## 1:                   (13.444304, 144.793731)     C03        C14          M26
    ## 2:  (34.52088095200048, -106.24058098499967)     C03        C14          M26
    ## 3:  (47.47531977900047, -100.11842104899966)     C03        C14          M26
    ## 4:                      (7.51498, 134.58252)     C03        C14          M26
    ## 5:                   (35.227087, -80.843127)     C03        C14          M26
    ## 6: (39.493240390000494, -117.07184056399967)     C03        C14          M26
    ##    LocationId StratID1 StratID2 StratID3 StratificationType
    ## 1:         66       S8      R11      G19          Territory
    ## 2:         35       S1      R11      G19              State
    ## 3:         38       S8      R15      G17              State
    ## 4:        204       S7      R11      G19          Territory
    ## 5:        122       S8      R11      G18              Local
    ## 6:         32       S1      R10      G19              State

``` r
tail(ms)
```

    ##    YEAR LocationAbbr      LocationDesc DataSource                      Topic
    ## 1: 2017           ME             Maine      YRBSS Alcohol and Other Drug Use
    ## 2: 2009           ML     Milwaukee, WI      YRBSS Alcohol and Other Drug Use
    ## 3: 2007           HI            Hawaii      YRBSS Alcohol and Other Drug Use
    ## 4: 2009           ML     Milwaukee, WI      YRBSS Alcohol and Other Drug Use
    ## 5: 2017           OL Orange County, FL      YRBSS Alcohol and Other Drug Use
    ## 6: 1999           DA        Dallas, TX      YRBSS Alcohol and Other Drug Use
    ##       Subtopic         ShortQuestionText
    ## 1: Alcohol Use Initiation of alcohol use
    ## 2: Alcohol Use Initiation of alcohol use
    ## 3: Alcohol Use Initiation of alcohol use
    ## 4: Alcohol Use Initiation of alcohol use
    ## 5: Alcohol Use Initiation of alcohol use
    ## 6: Alcohol Use          Ever alcohol use
    ##                                   Greater_Risk_Question           Description
    ## 1: Drank alcohol for the first time before age 11 years other than a few sips
    ## 2: Drank alcohol for the first time before age 11 years other than a few sips
    ## 3: Drank alcohol for the first time before age 11 years other than a few sips
    ## 4: Drank alcohol for the first time before age 11 years other than a few sips
    ## 5: Drank alcohol for the first time before age 11 years other than a few sips
    ## 6:                                   Ever drank alcohol other than a few sips
    ##    Data_Value_Symbol Data_Value_Type Greater_Risk_Data_Value
    ## 1:                 %      Percentage                      NA
    ## 2:                 %      Percentage                      NA
    ## 3:                 %      Percentage                      NA
    ## 4:                 %      Percentage                    16.2
    ## 5:                 %      Percentage                      NA
    ## 6:                 %      Percentage                    68.5
    ##    Greater_Risk_Data_Value_Footnote_Symbol Greater_Risk_Data_Value_Footnote
    ## 1:                                      NA                               NA
    ## 2:                                      NA                               NA
    ## 3:                                      NA                               NA
    ## 4:                                      NA                               NA
    ## 5:                                      NA                               NA
    ## 6:                                      NA                               NA
    ##    Greater_Risk_Low_Confidence_Limit Greater_Risk_High_Confidence_Limit
    ## 1:                                NA                                 NA
    ## 2:                                NA                                 NA
    ## 3:                                NA                                 NA
    ## 4:                             13.78                              19.05
    ## 5:                                NA                                 NA
    ## 6:                             64.44                              72.32
    ##                                            Lesser_Risk_Question
    ## 1: Did not drink alcohol for the first time before age 11 years
    ## 2: Did not drink alcohol for the first time before age 11 years
    ## 3: Did not drink alcohol for the first time before age 11 years
    ## 4: Did not drink alcohol for the first time before age 11 years
    ## 5: Did not drink alcohol for the first time before age 11 years
    ## 6:                                          Never drank alcohol
    ##    Lesser_Risk_Data_Value Lesser_Risk_Data_Value_Footnote_Symbol
    ## 1:                     NA                                     NA
    ## 2:                     NA                                     NA
    ## 3:                     NA                                     NA
    ## 4:                   83.8                                     NA
    ## 5:                     NA                                     NA
    ## 6:                   31.5                                     NA
    ##    Lesser_Risk_Data_Value_Footnote Lesser_Risk_Low_Confidence_Limit
    ## 1:                              NA                               NA
    ## 2:                              NA                               NA
    ## 3:                              NA                               NA
    ## 4:                              NA                            80.95
    ## 5:                              NA                               NA
    ## 6:                              NA                            27.68
    ##    Lesser_Risk_High_Confidence_Limit Sample_Size    Sex
    ## 1:                                NA           0 Female
    ## 2:                                NA          35   Male
    ## 3:                                NA          68  Total
    ## 4:                             86.22         689  Total
    ## 5:                                NA          29  Total
    ## 6:                             35.56         536   Male
    ##                         Race Grade                               GeoLocation
    ## 1: Black or African American   6th  (45.254228894000505, -68.98503133599962)
    ## 2:                     White   8th                   (43.038903, -87.906474)
    ## 3:                     White Total (21.304850435000446, -157.85774940299973)
    ## 4:                     Total   6th                   (43.038903, -87.906474)
    ## 5:                     Asian   7th                     (28.4845, -81.251883)
    ## 6:                     Total Total                   (32.776664, -96.796988)
    ##    TopicId SubTopicID QuestionCode LocationId StratID1 StratID2 StratID3
    ## 1:     C03        C14          M27         23       S7      R12      G17
    ## 2:     C03        C14          M27        130       S8      R15      G19
    ## 3:     C03        C14          M27         15       S1      R15       G1
    ## 4:     C03        C14          M27        130       S1       R1      G17
    ## 5:     C03        C14          M27        109       S1      R11      G18
    ## 6:     C03        C14          M26        126       S8       R1       G1
    ##    StratificationType
    ## 1:              State
    ## 2:              Local
    ## 3:              State
    ## 4:              Local
    ## 5:              Local
    ## 6:              Local

``` r
str(ms)
```

    ## Classes 'data.table' and 'data.frame':   33408 obs. of  35 variables:
    ##  $ YEAR                                   : int  2017 2011 2013 2017 2011 2009 2013 2011 2015 2015 ...
    ##  $ LocationAbbr                           : chr  "GU" "NM" "ND" "PW" ...
    ##  $ LocationDesc                           : chr  "Guam" "New Mexico" "North Dakota" "Palau" ...
    ##  $ DataSource                             : chr  "YRBSS" "YRBSS" "YRBSS" "YRBSS" ...
    ##  $ Topic                                  : chr  "Alcohol and Other Drug Use" "Alcohol and Other Drug Use" "Alcohol and Other Drug Use" "Alcohol and Other Drug Use" ...
    ##  $ Subtopic                               : chr  "Alcohol Use" "Alcohol Use" "Alcohol Use" "Alcohol Use" ...
    ##  $ ShortQuestionText                      : chr  "Ever alcohol use" "Ever alcohol use" "Ever alcohol use" "Ever alcohol use" ...
    ##  $ Greater_Risk_Question                  : chr  "Ever drank alcohol" "Ever drank alcohol" "Ever drank alcohol" "Ever drank alcohol" ...
    ##  $ Description                            : chr  "other than a few sips" "other than a few sips" "other than a few sips" "other than a few sips" ...
    ##  $ Data_Value_Symbol                      : chr  "%" "%" "%" "%" ...
    ##  $ Data_Value_Type                        : chr  "Percentage" "Percentage" "Percentage" "Percentage" ...
    ##  $ Greater_Risk_Data_Value                : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Greater_Risk_Data_Value_Footnote_Symbol: logi  NA NA NA NA NA NA ...
    ##  $ Greater_Risk_Data_Value_Footnote       : logi  NA NA NA NA NA NA ...
    ##  $ Greater_Risk_Low_Confidence_Limit      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Greater_Risk_High_Confidence_Limit     : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Lesser_Risk_Question                   : chr  "Never drank alcohol" "Never drank alcohol" "Never drank alcohol" "Never drank alcohol" ...
    ##  $ Lesser_Risk_Data_Value                 : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Lesser_Risk_Data_Value_Footnote_Symbol : logi  NA NA NA NA NA NA ...
    ##  $ Lesser_Risk_Data_Value_Footnote        : logi  NA NA NA NA NA NA ...
    ##  $ Lesser_Risk_Low_Confidence_Limit       : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Lesser_Risk_High_Confidence_Limit      : num  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Sample_Size                            : int  30 14 0 1 16 8 7 99 0 36 ...
    ##  $ Sex                                    : chr  "Male" "Total" "Male" "Female" ...
    ##  $ Race                                   : chr  "Asian" "Asian" "White" "Asian" ...
    ##  $ Grade                                  : chr  "8th" "8th" "6th" "8th" ...
    ##  $ GeoLocation                            : chr  "(13.444304, 144.793731)" "(34.52088095200048, -106.24058098499967)" "(47.47531977900047, -100.11842104899966)" "(7.51498, 134.58252)" ...
    ##  $ TopicId                                : chr  "C03" "C03" "C03" "C03" ...
    ##  $ SubTopicID                             : chr  "C14" "C14" "C14" "C14" ...
    ##  $ QuestionCode                           : chr  "M26" "M26" "M26" "M26" ...
    ##  $ LocationId                             : chr  "66" "35" "38" "204" ...
    ##  $ StratID1                               : chr  "S8" "S1" "S8" "S7" ...
    ##  $ StratID2                               : chr  "R11" "R11" "R15" "R11" ...
    ##  $ StratID3                               : chr  "G19" "G19" "G17" "G19" ...
    ##  $ StratificationType                     : chr  "Territory" "State" "State" "Territory" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

4.  Check the variable types in the data
5.  Take a closer look at some/all of the variables
6.  Validate with an external source
7.  Conduct some summary statistics to answer the initial question -
    Make exploratory graphs
