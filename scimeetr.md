---
title: "Introduction to scimeetr"
author: "Maxime Rivest"
date: "2017-11-13"
output: md_document
vignette: >
  %\VignetteIndexEntry{Introduction to scimeetr}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---
#Introduction

Scimeetr helps explore the scholarly literature. It contains a suit of function that let someone:

- load bibliometric data into R
- make a map of peer reviewed papers by creating various networks
- find research community
- characterise the research communities
- generate reading list

This tutorial is composed of two self-contained section. The first section show case the whole process with all the default parameters. The second section describes each function in more detail by presenting the rational for the function, the algorithms used and the options.

<a href="#top">Back to top</a>

#From data to reading list

You can automatically generate a reading list of seminal papers in a research litterature by using only those three functions: `ìmport_wos_files`, `scimap`, and `scilist`. This first section describes this process in more details.

<a href="#top">Back to top</a>

##loading and exploring bibliometric data

The first step in exploring the literature is to retrieve bibliometric data from the *Web of Science* or *Scopus*. In this first tutorial I use a dataset from the *Web of Science* about ecological networks.



```r
library(scimeetr)
scimeetr_list <- import_wos_files("path/to/folder/")
```

Then,`summary` can be used to get a quick characterisation of the data.


```r
summary(scimeetr_list)
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##        comID                      tag
## 1 com1 (742)                         
## 2                        BIODIVERSITY
## 3                        CONSERVATION
## 4                          MANAGEMENT
## 5                         AGRICULTURE
## 6            AGRI-ENVIRONMENT SCHEMES
## 7                  ECOSYSTEM SERVICES
```

From this summary, we see that there is 396 papers in my data set which overal cites 16567 different elements. On average, each paper cites 53 elements. 

Than we learn that, in this research community, 25% of the papers are cited less than 2 times, 50% are cited less than 9 times and 75% are cited less than ~23 times. There are papers that are cited up to 1333 times. The average citation per paper is ~25. This is much higher than the median (9), thus most paper are cited only a few times and a few papers are profusely cited. When correcting for the age of the paper, we learn that papers are cited 2 times per year on average.

By looking at the most frequent keyword and journals, we learn that this community of research is about biodiversity, agriculture, ecosystem services and policy. Keyword and journal frequency tables efficiently reveal the theme of a scientific community.

<a href="#top">Back to top</a>

##Mapping scientific community

The previous characterisation is great, but it is limited if your dataset contains many different scientific communities. By detecting the scientific communities present within a dataset a map of science can be drawn and each cluster can be characterised on its own. The function `scimap` can be used for this task.


```r
scimap_result <- scimap(scimeetr_list)
```

The function returns all the data that scimeetr_list contained and more. For example communities have been identified and now if the function `summary` is used on scim_result. In addition of the previous information. The descriminant keywords of each communities constituating the main community are listed.


```r
summary(scimap_result)
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##           comID                        tag
## 1    com1 (742)                           
## 2                             BIODIVERSITY
## 3                             CONSERVATION
## 4                               MANAGEMENT
## 5                              AGRICULTURE
## 6                 AGRI-ENVIRONMENT SCHEMES
## 7                       ECOSYSTEM SERVICES
## 8  com1_1 (202)                           
## 9                            PARTICIPATION
## 10                             AGRICULTURE
## 11                                 FARMERS
## 12                               ATTITUDES
## 13              AGRI-ENVIRONMENTAL SCHEMES
## 14                                  POLICY
## 15 com1_8 (132)                           
## 16                                ADOPTION
## 17                             AGRICULTURE
## 18                                  POLICY
## 19                           WATER QUALITY
## 20                               AUSTRALIA
## 21                                 FARMERS
## 22  com1_4 (67)                           
## 23                                LAND-USE
## 24                             AGRICULTURE
## 25                                 SCHEMES
## 26                               LANDSCAPE
## 27                                  POLICY
## 28              COMMON AGRICULTURAL POLICY
## 29 com1_3 (292)                           
## 30                            BIODIVERSITY
## 31                AGRI-ENVIRONMENT SCHEMES
## 32                            CONSERVATION
## 33                              MANAGEMENT
## 34                               DIVERSITY
## 35                 AGRICULTURAL LANDSCAPES
```

Except for the last tables, all of the output is identical to the `summary` output above. Those last tables now reveals that the papers in our database can be clustered in two communities. One that is about x and the other that is about y.

The function `plot` can be used on the output of the function summary for a graphical representation of the sub-communities. 


```r
plot(summary(scimap_result, com_size = 30))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

<a href="#top">Back to top</a>

##Automatically generating a reading list of seminal papers

Now that we have characterise the main community and seen of which community it is constituted, we can decide if it is the community that we wish to join / review. If it is, we use the function `scilist` to get reading lists. The defaul readin list will find the seminal papers of each communitiy.


```r
reading_list <- scilist(scimap_result)
reading_list$com1
```

|ID                                                                              | Frequency.x| Pourcentage|
|:-------------------------------------------------------------------------------|-----------:|-----------:|
|KLEIJN D, 2003, J APPL ECOL, V40, P947, DOI 10.1111/J.1365-2664.2003.00868.X    |         113|   0.0030005|
|KLEIJN D, 2006, ECOL LETT, V9, P243, DOI 10.1111/J.1461-0248.2005.00869.X       |          73|   0.0019383|
|KLEIJN D, 2001, NATURE, V413, P723, DOI 10.1038/35099540                        |          57|   0.0015135|
|BENTON TG, 2003, TRENDS ECOL EVOL, V18, P182, DOI 10.1016/S0169-5347(03)00011-9 |          54|   0.0014338|
|PANNELL DJ, 2006, AUST J EXP AGR, V46, P1407, DOI 10.1071/EA5037                |          50|   0.0013276|
|MORRIS C, 1995, J RURAL STUD, V11, P51, DOI 10.1016/0743-0167(94)00037-A        |          47|   0.0012480|
|TSCHARNTKE T, 2005, ECOL LETT, V8, P857, DOI 10.1111/J.1461-0248.2005.00782.X   |          45|   0.0011949|
|FALCONER K, 2000, J RURAL STUD, V16, P379, DOI 10.1016/S0743-0167(99)00066-2    |          43|   0.0011418|

<a href="#top">Back to top</a>

# In depth description of each steps
##How to get bibliometric data?

Biliometric data can be obtained from either *Scopus* or the *Web of Science*. Most university library have access to either one and some have access to both.

<a href="#top">Back to top</a>

### Retrieving data from Scopus

![](./vignettes/scopus.png)

*Scopus home page.*

![](./vignettes/scopus1.png)

*Select all and export*

![](./vignettes/scopus2.png)

*Export as CSV file and select all fields for exportation*


Following the previous steps will get you one or several .csv files. Then, to import this/these file(s) in `R`, you need to put it/them in a **new folder which contains only the files to import into `R`**

<a href="#top">Back to top</a>

### Retrieving data from Web of Science

![Web of Science home page. Make sure that Select a database corresponds to Web of Science Core Collection](./vignettes/wos.png)

*Web of Science home page. Make sure that Select a database corresponds to Web of Science Core Collection*

![Save to Other Files Formats](./vignettes/wos1.png)

*Save to Other Files Formats*

![You can download only 500 items at a time. You should select Full Record and Cited References. And select the Tab-delimeted (UTF-8) as file format.](./vignettes/wos2.png)

*You can download only 500 items at a time. You should select Full Record and Cited References. And select the Tab-delimeted (UTF-8) as file format.*


Following the previous steps will get you one or several .txt files. Then, to import this/these file(s) in `R`, you need to put it/them in a **new folder which contains only the files to import into `R`**

<a href="#top">Back to top</a>

## How to upload bibliometric data into R

The bibliometric data obtained from Scopus or Web of science are either in .csv or .txt format. These are standard file formats and you most likely know them. There are built in function in `R` that let you import .csv and .txt files. So why does scimeetr provide you with `import_scopus_files` and `import_wos_files`? There are four reasons. The main one is that bibliometric data contains author names from around the world, which means that all alphabets are used and this leads to problems with file encoding. Scimeetr's import functions solves that problem. Second, Scopus do not provide standard, uniform and consisten cited reference list. Thus, `import_scopus_files` has to standardize it at import. This explains the additional time required to load scopus files versus wos files. Third, Scopus and Web of Science do not use the same column names so they have to be homogenized at import. Finally, the data can be transformed into a scimeetr object so that `summary`, `plot` and `print` will know what to do with it.


```r
scimeetr_list <- import_wos_files(files_directory = "/path/to/folder/")
scimeetr_list <- import_scopus_files(files_directory = "/path/to/folder/")
```
Do not forget that this function take in a path to a folder not a file. Thus, it need a slash at the end of the folder path.

<a href="#top">Back to top</a>

## Exploring scimeetr data

Printing `scimeetr_list` that we just created will provide some informations about it, but `summary` will provide more.


```r
scimeetr_list
```

```
## 
## # A scimeetr object #
## ---------------------
## Number of papers:  742
## Number of communities:  1
## Names of communities:  com1
## 
## Table of the 5 most mentionned words 
## 
##                  key_words  title_words abstract_words
## 1             BIODIVERSITY CONSERVATION        FARMERS
## 2             CONSERVATION AGRICULTURAL   CONSERVATION
## 3               MANAGEMENT   MANAGEMENT   AGRICULTURAL
## 4              AGRICULTURE       POLICY     MANAGEMENT
## 5 AGRI-ENVIRONMENT SCHEMES      FARMERS         POLICY
```

```r
summary(scimeetr_list)
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##        comID                      tag
## 1 com1 (742)                         
## 2                        BIODIVERSITY
## 3                        CONSERVATION
## 4                          MANAGEMENT
## 5                         AGRICULTURE
## 6            AGRI-ENVIRONMENT SCHEMES
## 7                  ECOSYSTEM SERVICES
```

A scimeetr object such as `scimeetr_list` contains more data than what can be seen with `print` and `summary`. A scimeetr object is in fact a list of communities list which are themselves list of up to 9 elements. Each communities contain a data.frame called `dfsci`. This dataframe contains all the bibliometric data that was importedinto `R`.


```r
scimeetr_list$com1$dfsci
```

|PT |AU                                                                |BA |BE |GP |AF                                                                         |BF |CA |TI                                                                                  |SO                               |SE |BS |LA      |DT      |CT |CY |CL |SP |HO |DE                                                                                                             |ID                                                                                      |C1                                                                                                                                                                                                                                                  |RP                                                                                                         |EM                           |RI |OI |FU                                                                                                                                                                | NR| TC| Z9| U1| U2|PU    |PI      |PA                                       |SN        |EI |BN |J9                 |JI                   |PD  |   PY|VL |IS |PN |SU |SI |MA |BP  |EP  |AR |DI                 |D2 | PG|WC                                      |SC                                                |GA    |UT                  | PM|RECID                                          |
|:--|:-----------------------------------------------------------------|:--|:--|:--|:--------------------------------------------------------------------------|:--|:--|:-----------------------------------------------------------------------------------|:--------------------------------|:--|:--|:-------|:-------|:--|:--|:--|:--|:--|:--------------------------------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------|:----------------------------|:--|:--|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------|--:|--:|--:|--:|--:|:-----|:-------|:----------------------------------------|:---------|:--|:--|:------------------|:--------------------|:---|----:|:--|:--|:--|:--|:--|:--|:---|:---|:--|:------------------|:--|--:|:---------------------------------------|:-------------------------------------------------|:-----|:-------------------|--:|:----------------------------------------------|
|J  |Holstead, KL; Kenyon, W; Rouillard, JJ; Hopkins, J; Galan-Diaz, C |NA |NA |NA |Holstead, K. L.; Kenyon, W.; Rouillard, J. J.; Hopkins, J.; Galan-Diaz, C. |NA |   |Natural flood management from the farmer's perspective: criteria that affect uptake |JOURNAL OF FLOOD RISK MANAGEMENT |NA |NA |English |Article |   |   |   |   |   |Natural flood management; catchment management; flood risk management; farmer decision making; land use change |DECISION-MAKING; BEHAVIOR; CONSERVATIONISTS; PARTICIPATION; ATTITUDES; SCHEMES; ENGLAND |[Holstead, K. L.; Kenyon, W.; Hopkins, J.] James Hutton Inst, Social Econ & Geog Sci Grp, Aberdeen AB15 8QH, Scotland; [Rouillard, J. J.] Univ Dundee, Sch Environm, Dundee, Scotland; [Galan-Diaz, C.] Dot Rural Univ Aberdeen, Aberdeen, Scotland |Holstead, KL (reprint author), James Hutton Inst, Social Econ & Geog Sci Grp, Aberdeen AB15 8QH, Scotland. |kirsty.holstead@hutton.ac.uk |   |   |Scottish Government's Rural and Environment Science and Analytical Services (RESAS) Division, Work Package 2.4: Methods for mitigating and adapting to flood risk | 59|  2|  2|  0|  0|WILEY |HOBOKEN |111 RIVER ST, HOBOKEN 07030-5774, NJ USA |1753-318X |   |NA |J FLOOD RISK MANAG |J. Flood Risk Manag. |JUN | 2017|10 |2  |   |NA |SI |NA |205 |218 |   |10.1111/jfr3.12129 |NA | 14|Environmental Sciences; Water Resources |Environmental Sciences & Ecology; Water Resources |EU4HB |WOS:000400989300008 | NA|HOLSTEAD KL, 2017, J FLOOD RISK MANAG, 10, 205 |

<a href="#top">Back to top</a>

## Making reading lists

If we are confident that the papers contained in `scimeetr_list` are those for which we want a reading list we can used the function `scilist` to find various lists of papers. The default list given by `scilist` contains the seminal papers for the community analysed. That is, it rank the paper by the number of times they were cited by all the papers and list them by citation frequency.


```r
scilist(scimeetr_list)
```

|ID                                                                              | Frequency.x| Pourcentage|
|:-------------------------------------------------------------------------------|-----------:|-----------:|
|KLEIJN D, 2003, J APPL ECOL, V40, P947, DOI 10.1111/J.1365-2664.2003.00868.X    |         113|   0.0030005|
|KLEIJN D, 2006, ECOL LETT, V9, P243, DOI 10.1111/J.1461-0248.2005.00869.X       |          73|   0.0019383|
|KLEIJN D, 2001, NATURE, V413, P723, DOI 10.1038/35099540                        |          57|   0.0015135|
|BENTON TG, 2003, TRENDS ECOL EVOL, V18, P182, DOI 10.1016/S0169-5347(03)00011-9 |          54|   0.0014338|
|PANNELL DJ, 2006, AUST J EXP AGR, V46, P1407, DOI 10.1071/EA5037                |          50|   0.0013276|
|MORRIS C, 1995, J RURAL STUD, V11, P51, DOI 10.1016/0743-0167(94)00037-A        |          47|   0.0012480|
|TSCHARNTKE T, 2005, ECOL LETT, V8, P857, DOI 10.1111/J.1461-0248.2005.00782.X   |          45|   0.0011949|
|FALCONER K, 2000, J RURAL STUD, V16, P379, DOI 10.1016/S0743-0167(99)00066-2    |          43|   0.0011418|

With the parameter `k`, we can control the length of the reading list.


```r
scilist(scimeetr_list, k = 3)
```

|ID                                                                           | Frequency.x| Pourcentage|
|:----------------------------------------------------------------------------|-----------:|-----------:|
|KLEIJN D, 2003, J APPL ECOL, V40, P947, DOI 10.1111/J.1365-2664.2003.00868.X |         113|   0.0030005|
|KLEIJN D, 2006, ECOL LETT, V9, P243, DOI 10.1111/J.1461-0248.2005.00869.X    |          73|   0.0019383|
|KLEIJN D, 2001, NATURE, V413, P723, DOI 10.1038/35099540                     |          57|   0.0015135|

With the parameter `reading_list`, we can get any of the following 12 reading lists that fits into three categories:

* Core
    - core_papers
    - core_yr
    - core_residual
* Experts
    - by_expert_LC
    - by_expert_TC
    - group_of_experts_TC
    - group_of_experts_LC
* Centrality
    - cite_most_others
    - betweeness
    - closeness
    - connectness
    - page_rank
    
The default reading list is `core_papers`.

<a href="#top">Back to top</a>

### Core

I categorise the reading lists as **core** because they are reading lists of core papers as they are all a variation of the number of times papers within our community of interest refers to the paper listed. Although the number of citation is not a perfect measure of a papers importance for a community it should be a good proxy. A weekness of the number of citation as a measure of papers importance is that not all citations are equal. For example, sometimes a paper is cited because it is criticized or because it contrasts with other findings. This as been realised by others before and some have attempted to fix it by creating the concept of influential citation. Influential citation is a great concept by to be calculated it requires advance text processing and access to the full text of each papers. As it is notoriosly time consuming to get full text and even harder to get it in the right format, we are left with citation count.

#### Most cited per year

Using `scilist` with `reading_list = "core_yr"` will list the most cited paper for each year from three years before present to ten years before present. The parameter `k` controls the number of paper per year to list.

```r
scilist(scimeetr_list, reading_list = "core_yr", k = 2)
```

|record                                                                                   | Frequency.x| age|
|:----------------------------------------------------------------------------------------|-----------:|---:|
|PE'ER G, 2014, SCIENCE, V344, P1090, DOI 10.1126/SCIENCE.1253425                         |           9|   3|
|MEICHTRY-STIER KS, 2014, AGR ECOSYST ENVIRON, V189, P101, DOI 10.1016/J.AGEE.2014.02.038 |           5|   3|
|RIBEIRO PF, 2014, AGR ECOSYST ENVIRON, V183, P138, DOI 10.1016/J.AGEE.2013.11.002        |           5|   3|
|BURTON RJF, 2013, LAND USE POLICY, V30, P628, DOI 10.1016/J.LANDUSEPOL.2012.05.002       |          25|   4|
|UTHES S, 2013, ENVIRON MANAGE, V51, P251, DOI 10.1007/S00267-012-9959-6                  |          16|   4|
|BAUMGART-GETZ A, 2012, J ENVIRON MANAGE, V96, P17, DOI 10.1016/J.JENVMAN.2011.10.006     |          17|   5|
|EMERY SB, 2012, J RURAL STUD, V28, P218, DOI 10.1016/J.JRURSTUD.2012.02.004              |          12|   5|
|KLEIJN D, 2011, TRENDS ECOL EVOL, V26, P474, DOI 10.1016/J.TREE.2011.05.009              |          23|   6|
|BATARY P, 2011, P ROY SOC B-BIOL SCI, V278, P1894, DOI 10.1098/RSPB.2010.1923            |          15|   6|
|SATTLER C, 2010, LAND USE POLICY, V27, P70, DOI 10.1016/J.LANDUSEPOL.2008.02.002         |          20|   7|
|MATZDORF B, 2010, LAND USE POLICY, V27, P535, DOI 10.1016/J.LANDUSEPOL.2009.07.011       |          18|   7|
|STOATE C, 2009, J ENVIRON MANAGE, V91, P22, DOI 10.1016/J.JENVMAN.2009.07.005            |          22|   8|
|RUTO E, 2009, J ENVIRON PLANN MAN, V52, P631, DOI 10.1080/09640560902958172              |          19|   8|
|BURTON RJF, 2008, SOCIOL RURALIS, V48, P16, DOI 10.1111/J.1467-9523.2008.00452.X         |          41|   9|
|DEFRANCESCO E, 2008, J AGR ECON, V59, P114, DOI 10.1111/J.1477-9552.2007.00134.X         |          38|   9|
|KNOWLER D, 2007, FOOD POLICY, V32, P25, DOI 10.1016/J.FOODPOL.2006.01.003                |          38|  10|
|WHITTINGHAM MJ, 2007, J APPL ECOL, V44, P1, DOI 10.1111/J.1365-2664.2006.01263.X         |          29|  10|

<a href="#top">Back to top</a>

#### More cited than expected

Using `scilist` with `reading_list = "core_residual"` will list the papers that diverge most from the expected number of citation for this particular paper. This can be visualised in the figure below. The point that have the biggest difference between their frequency value and the fitted blue lines are listed in the `core_residual` reading list.
![plot of chunk unnamed-chunk-19](figure/unnamed-chunk-19-1.png)

Here is an example of the code and its result.

```r
scilist(scimeetr_list, reading_list = "core_residual", k = 3)
```

|record                                                                       | Frequency.x| age|
|:----------------------------------------------------------------------------|-----------:|---:|
|KLEIJN D, 2003, J APPL ECOL, V40, P947, DOI 10.1111/J.1365-2664.2003.00868.X |         113|  14|
|MORRIS C, 1995, J RURAL STUD, V11, P51, DOI 10.1016/0743-0167(94)00037-A     |          47|  22|
|ERVIN CA, 1982, LAND ECON, V58, P277, DOI 10.2307/3145937                    |          15|  35|

<a href="#top">Back to top</a>

### Experts
The reading lists that I categorise as **expert** are built from authors information. Experts within a community are identified based on the number of papers they published and the number of times each of their papers are cited. 

#### Recent paper of a few experts

Using `scilist` with `reading_list = "by_expert_LC"` we will get a list of recent papers by one or a few experts in the community. For the option `by_expert_LC`, authors are ranked based on their *harmonic local H-index*. The H-index is a measure of an other productivity and impact. An author with an H-index of 10 means that he has published at least 10 papers with 10 or more citation each. A local H-index means that only citations from other papers in the community are counted. A harmonic local H-index means that authors do not get the full credit for each citation their paper received. It is corrected depending on the authos position in the authors list. First authors gets most of the credit, then the last author gets the second most, and the authors gets credit as a proportion of their position. Once the authors harmonic-local-H-index is found they are ranked and the `m` most recent publication of the `k` most 'expert' authors are listed as a reading list.

```r
scilist(scimeetr_list, reading_list = "by_expert_LC", k = 2, m = 2)
```

|AU           | HL|PAPER                                            |
|:------------|--:|:------------------------------------------------|
|Herzog, F    |  6|SEREKE F, 2015, AGRON SUSTAIN DEV, 35, 759       |
|Herzog, F    |  6|KELEMEN E, 2013, LAND USE POLICY, 35, 318        |
|Matzdorf, B  |  5|MEYER C, 2016, LAND USE POLICY, 55, 352          |
|Matzdorf, B  |  5|SCHOMERS S, 2015, SUSTAINABILITY-BASEL, 7, 13856 |
|Matzdorf, B  |  5|SCHOMERS S, 2015, LAND USE POLICY, 42, 58        |
|Schupbach, B |  5|SCHUPBACH B, 2016, LAND USE POLICY, 53, 27       |
|Schupbach, B |  5|AVIRON S, 2011, RESTOR ECOL, 19, 500             |
|Schupbach, B |  5|JUNGE X, 2011, BIOL CONSERV, 144, 1430           |

Using `scilist` with `reading_list = "by_expert_TC"` instead of `reading_list = "by_expert_LC"`, notice the `_TC` instead of the `_LC` will based the ranking calculation on **t**otal citation of it's publications instead of only the **l**ocal citations.

<a href="#top">Back to top</a>

#### Paper of experts group

Using `scilist` with `reading_list = "group_of_experts_LC"` we will get a list of papers for which many authors are experts in the community. For this option, authors are assigned a *harmonic local H-index* like described in the previous section. But this time, a weighted sum of the harmonic-local-H-index of each authors of a paper is calculated. 


```r
scilist(scimeetr_list, reading_list = "group_of_experts_LC", k = 5)
```

|RECID                                         |      AuS|
|:---------------------------------------------|--------:|
|HERZOG F, 2005, AGR ECOSYST ENVIRON, 108, 189 | 8.678571|
|AVIRON S, 2011, RESTOR ECOL, 19, 500          | 8.383333|
|AVIRON S, 2007, AGR ECOSYST ENVIRON, 122, 295 | 8.166667|
|AVIRON S, 2005, GRASSLAND SCI EUR, 10, 340    | 7.955952|
|KAMPMANN D, 2008, J NAT CONSERV, 16, 12       | 7.926190|

Using `scilist` with `reading_list = "group_of_experts_TC"` instead of `reading_list = "group_of_experts_LC"`, notice the `_TC` instead of the `_LC` will based the ranking calculation on **t**otal citation of it's publications instead of only the **l**ocal citations.

<a href="#top">Back to top</a>

### Centrality

Their are several measures of [nodes centrality in graph theory](https://en.wikipedia.org/wiki/Centrality). The most central papers of a community of papers can be found with `scilist`.

#### Betweeness

Betweeness measures the importance of a paper in connecting two clusters of papers. Papers with a high betweeness would therefore be a paper that tend to be more interdisciplinary.


```r
scilist(scimeetr_list, reading_list = "betweeness", k = 5)
```

|.x[[i]]                                      |
|:--------------------------------------------|
|HEJNOWICZ AP, 2016, LAND USE POLICY, 55, 240 |
|UTHES S, 2013, ENVIRON MANAGE, 51, 251       |
|BURTON RJF, 2013, LAND USE POLICY, 30, 628   |
|JARVIS DI, 2011, CRIT REV PLANT SCI, 30, 125 |
|STOATE C, 2001, J ENVIRON MANAGE, 63, 337    |

<a href="#top">Back to top</a>

#### Closeness

Closeness measures the average number of link between a paper and all other papers. Papers with a high closeness would therefore be a paper that tend to have a large and wide list of citations.


```r
scilist(scimeetr_list, reading_list = "closeness", k = 5)
```

|.x[[i]]                                      |
|:--------------------------------------------|
|HEJNOWICZ AP, 2016, LAND USE POLICY, 55, 240 |
|UTHES S, 2013, ENVIRON MANAGE, 51, 251       |
|HOLLAND JM, 2016, PEST MANAG SCI, 72, 1638   |
|JARVIS DI, 2011, CRIT REV PLANT SCI, 30, 125 |
|STOATE C, 2001, J ENVIRON MANAGE, 63, 337    |

<a href="#top">Back to top</a>

#### Connectness

Connectness measures the number of links a paper has. Papers with a high connectness would therefore be a paper that tend to have cited what most other studies cited.


```r
scilist(scimeetr_list, reading_list = "connectness", k = 5)
```

|.x[[i]]                                         |
|:-----------------------------------------------|
|METTEPENNINGEN E, 2013, LAND USE POLICY, 33, 20 |
|GUILLEM EE, 2013, LAND USE POLICY, 31, 565      |
|UTHES S, 2013, ENVIRON MANAGE, 51, 251          |
|BURTON RJF, 2013, LAND USE POLICY, 30, 628      |
|WADE MR, 2008, PHILOS T R SOC B, 363, 831       |

<a href="#top">Back to top</a>

#### Page rank

[Page rank](https://en.wikipedia.org/wiki/PageRank) was developped by Larry Page at google and it's a way to measure web page importance. The algorithm was applied to directed graph, so I am not sure of the consequence of applying it on the undirected graph that we have here.


```r
scilist(scimeetr_list, reading_list = "page_rank", k = 5)
```

|.x[[i]]                                         |
|:-----------------------------------------------|
|METTEPENNINGEN E, 2013, LAND USE POLICY, 33, 20 |
|GUILLEM EE, 2013, LAND USE POLICY, 31, 565      |
|UTHES S, 2013, ENVIRON MANAGE, 51, 251          |
|GABRIEL D, 2009, J APPL ECOL, 46, 323           |
|VAN DER WAL R, 2008, BIOL LETTERS, 4, 256       |

<a href="#top">Back to top</a>

#### Cite most others

With the option `cite_most_others`, the papers that cite most other papers of the community can be found. This is not a centrality measure but it is also based on papers connection to each other. It should tend to find litterature review and recent papers that have an especially good grasp on the community.


```r
scilist(scimeetr_list, reading_list = "cite_most_others", k = 5)
```

|RECID.x                                          |DOI                              | Nb_of_ref_within_com|
|:------------------------------------------------|:--------------------------------|--------------------:|
|UTHES S, 2013, ENVIRON MANAGE, 51, 251           |10.1007/s00267-012-9959-6        |                   24|
|HEJNOWICZ AP, 2016, LAND USE POLICY, 55, 240     |10.1016/j.landusepol.2016.04.005 |                   16|
|SCHOMERS S, 2015, SUSTAINABILITY-BASEL, 7, 13856 |10.3390/su71013856               |                   13|
|SCHOMERS S, 2015, LAND USE POLICY, 42, 58        |10.1016/j.landusepol.2014.06.025 |                   12|
|DEDEURWAERDERE T, 2015, ECOL ECON, 119, 24       |10.1016/j.ecolecon.2015.07.025   |                   11|

<a href="#top">Back to top</a>

## Finding the main communities of research

In the previous sections we have looked at only the main research community. But, splitting the main community in sub-communities can provide a more detail picture of the litterature. It can also help identify and then remove irrelevant sub-communities. To achieve any of this, the sub-communities have to be identified and characterized. The function `scimap`, as in science map, was developped for this task. By default, the graph use bibliographic coupling to calculate connections between papers, but coupling can also be done based on abstract words (abc), title words (tic) or keywords (kec).


```r
summary(scimap(scimeetr_list, coupling_by = 'bic', community_algorithm = 'louvain', min_com_size = 100))
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##           comID                        tag
## 1    com1 (742)                           
## 2                             BIODIVERSITY
## 3                             CONSERVATION
## 4                               MANAGEMENT
## 5                              AGRICULTURE
## 6                 AGRI-ENVIRONMENT SCHEMES
## 7                       ECOSYSTEM SERVICES
## 8  com1_1 (202)                           
## 9                            PARTICIPATION
## 10                             AGRICULTURE
## 11                                 FARMERS
## 12                               ATTITUDES
## 13              AGRI-ENVIRONMENTAL SCHEMES
## 14                                  POLICY
## 15 com1_8 (132)                           
## 16                                ADOPTION
## 17                             AGRICULTURE
## 18                                  POLICY
## 19                           WATER QUALITY
## 20                               AUSTRALIA
## 21                                 FARMERS
## 22 com1_3 (292)                           
## 23                            BIODIVERSITY
## 24                AGRI-ENVIRONMENT SCHEMES
## 25                            CONSERVATION
## 26                              MANAGEMENT
## 27                               DIVERSITY
## 28                 AGRICULTURAL LANDSCAPES
```

```r
summary(scimap(scimeetr_list, coupling_by = 'abc', community_algorithm = 'louvain', min_com_size = 100))
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##           comID                        tag
## 1    com1 (742)                           
## 2                             BIODIVERSITY
## 3                             CONSERVATION
## 4                               MANAGEMENT
## 5                              AGRICULTURE
## 6                 AGRI-ENVIRONMENT SCHEMES
## 7                       ECOSYSTEM SERVICES
## 8  com1_3 (255)                           
## 9                                 ADOPTION
## 10                                  POLICY
## 11                                 FARMERS
## 12                      ECOSYSTEM SERVICES
## 13                               AUSTRALIA
## 14                              INCENTIVES
## 15 com1_2 (237)                           
## 16                             AGRICULTURE
## 17                           PARTICIPATION
## 18                                  POLICY
## 19                                 SCHEMES
## 20              COMMON AGRICULTURAL POLICY
## 21              AGRI-ENVIRONMENTAL SCHEMES
## 22 com1_1 (249)                           
## 23                            BIODIVERSITY
## 24                AGRI-ENVIRONMENT SCHEMES
## 25                            CONSERVATION
## 26                              MANAGEMENT
## 27                               DIVERSITY
## 28                 AGRICULTURAL LANDSCAPES
```

```r
summary(scimap(scimeetr_list, coupling_by = 'tic', community_algorithm = 'louvain', min_com_size = 100))
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##           comID                          tag
## 1    com1 (742)                             
## 2                               BIODIVERSITY
## 3                               CONSERVATION
## 4                                 MANAGEMENT
## 5                                AGRICULTURE
## 6                   AGRI-ENVIRONMENT SCHEMES
## 7                         ECOSYSTEM SERVICES
## 8  com1_1 (176)                             
## 9                               CONSERVATION
## 10                                    POLICY
## 11                                   FARMERS
## 12                                  ADOPTION
## 13                                 AUSTRALIA
## 14                                LANDSCAPES
## 15 com1_3 (127)                             
## 16                              BIODIVERSITY
## 17                                   SCHEMES
## 18                                    POLICY
## 19                   AGRICULTURAL LANDSCAPES
## 20                        ECOSYSTEM SERVICES
## 21                             PARTICIPATION
## 22 com1_5 (128)                             
## 23                                MANAGEMENT
## 24                        ECOSYSTEM SERVICES
## 25                               AGRICULTURE
## 26                                  ADOPTION
## 27                 BIODIVERSITY CONSERVATION
## 28                                    POLICY
## 29 com1_4 (160)                             
## 30                              BIODIVERSITY
## 31                              CONSERVATION
## 32                  AGRI-ENVIRONMENT SCHEMES
## 33                                MANAGEMENT
## 34                   AGRICULTURAL LANDSCAPES
## 35              AGRICULTURAL INTENSIFICATION
## 36 com1_2 (134)                             
## 37                              BIODIVERSITY
## 38                                  LAND-USE
## 39                       AGRICULTURAL POLICY
## 40                             PARTICIPATION
## 41                                   SCHEMES
## 42                                   FARMERS
```

```r
summary(scimap(scimeetr_list, coupling_by = 'kec', community_algorithm = 'louvain', min_com_size = 100))
```

```
## 
##  # Summary of Scimeetr #
## -----------------------
##     Number of papers:  742
##     Number of different reference:  28526
## 
##     Average number of reference per paper:  51
## 
##     Quantiles of total citation per paper: 
## 
##      0%     25%     50%     75%    100% 
##    0.00    2.00    7.00   19.75 1333.00 
## 
##     Mean number of citation per paper:  19.81536
## 
##     Average number of citation per paper per year:  1.666667
## 
## 
##   Table of the 10 most mentionned keywords 
## 
##                       Keyword    Frequency
## 1                BIODIVERSITY           57
## 2                 AGRICULTURE           46
## 3  COMMON AGRICULTURAL POLICY           32
## 4          ECOSYSTEM SERVICES           31
## 5                CONSERVATION           28
## 6    AGRI-ENVIRONMENT SCHEMES           27
## 7     AGRI-ENVIRONMENT SCHEME           20
## 8  AGRI-ENVIRONMENTAL SCHEMES           19
## 9         AGRICULTURAL POLICY           18
## 10              WATER QUALITY           18
## 
## 
## 
##   Table of the 10 most productive journal 
## 
##                                             Journal    Frequency
## 1                                   LAND USE POLICY           84
## 2              AGRICULTURE ECOSYSTEMS & ENVIRONMENT           37
## 3               JOURNAL OF ENVIRONMENTAL MANAGEMENT           33
## 4                           BIOLOGICAL CONSERVATION           24
## 5                        JOURNAL OF APPLIED ECOLOGY           21
## 6                              ECOLOGICAL ECONOMICS           17
## 7                          JOURNAL OF RURAL STUDIES           17
## 8                              AGRICULTURAL SYSTEMS           14
## 9  JOURNAL OF ENVIRONMENTAL PLANNING AND MANAGEMENT           14
## 10                     LANDSCAPE AND URBAN PLANNING           14
## 
## 
## 
##   Table of the most descriminant keywords 
## 
##           comID                          tag
## 1    com1 (742)                             
## 2                               BIODIVERSITY
## 3                               CONSERVATION
## 4                                 MANAGEMENT
## 5                                AGRICULTURE
## 6                   AGRI-ENVIRONMENT SCHEMES
## 7                         ECOSYSTEM SERVICES
## 8  com1_3 (297)                             
## 9                                AGRICULTURE
## 10                                  ADOPTION
## 11                                    POLICY
## 12                             PARTICIPATION
## 13                                   FARMERS
## 14                AGRI-ENVIRONMENTAL SCHEMES
## 15 com1_2 (151)                             
## 16                              BIODIVERSITY
## 17                              CONSERVATION
## 18                  AGRI-ENVIRONMENT SCHEMES
## 19                                MANAGEMENT
## 20              AGRICULTURAL INTENSIFICATION
## 21                                 DIVERSITY
## 22 com1_5 (112)                             
## 23                                MANAGEMENT
## 24                        ECOSYSTEM SERVICES
## 25                 BIODIVERSITY CONSERVATION
## 26                                 LANDSCAPE
## 27                                    POLICY
## 28                                   SYSTEMS
## 29 com1_1 (127)                             
## 30                              CONSERVATION
## 31                  AGRI-ENVIRONMENT SCHEMES
## 32                   AGRICULTURAL LANDSCAPES
## 33                                 DIVERSITY
## 34                                     BIRDS
## 35                                  LAND-USE
```

<a href="#top">Back to top</a>

## Focusing on a sub-community

With the function `focus_on`, it is possible to change focus on a sub-community.


```r
scil <- scimap(scimeetr_list)
scil
```

```
## 
## # A scimeetr object #
## ---------------------
## Number of papers:  742
## Number of communities:  5
## Names of communities:  com1 com1_1 com1_8 com1_4 com1_3
## 
## Table of the 5 most mentionned words 
## 
##                  key_words  title_words abstract_words
## 1             BIODIVERSITY CONSERVATION        FARMERS
## 2             CONSERVATION AGRICULTURAL   CONSERVATION
## 3               MANAGEMENT   MANAGEMENT   AGRICULTURAL
## 4              AGRICULTURE       POLICY     MANAGEMENT
## 5 AGRI-ENVIRONMENT SCHEMES      FARMERS         POLICY
```

```r
subscil <- focus_on(scil, grab = 'com1_1')
subscil
```

```
## 
## # A scimeetr object #
## ---------------------
## Number of papers:  202
## Number of communities:  1
## Names of communities:  com1_1
## 
## Table of the 5 most mentionned words 
## 
##       key_words       title_words abstract_words
## 1 PARTICIPATION           FARMERS        FARMERS
## 2   AGRICULTURE            POLICY         POLICY
## 3  CONSERVATION AGRIENVIRONMENTAL  ENVIRONMENTAL
## 4       FARMERS      AGRICULTURAL   AGRICULTURAL
## 5    MANAGEMENT           SCHEMES           FARM
```

<a href="#top">Back to top</a>

## Dive to a sub-community

With the function `dive_to`, it is possible to move down to a sub-community and keep it's sub-communities.


```r
scil <- scimap(scimap(scimeetr_list))
scil
```

```
## 
## # A scimeetr object #
## ---------------------
## Number of papers:  742
## Number of communities:  13
## Names of communities:  com1 com1_1 com1_1_2 com1_1_1 com1_1_3 com1_8 com1_8_3 com1_8_4 com1_4 com1_3 com1_3_4 com1_3_1 com1_3_5
## 
## Table of the 5 most mentionned words 
## 
##                  key_words  title_words abstract_words
## 1             BIODIVERSITY CONSERVATION        FARMERS
## 2             CONSERVATION AGRICULTURAL   CONSERVATION
## 3               MANAGEMENT   MANAGEMENT   AGRICULTURAL
## 4              AGRICULTURE       POLICY     MANAGEMENT
## 5 AGRI-ENVIRONMENT SCHEMES      FARMERS         POLICY
```

```r
subscil <- dive_to(scil, aim_at = 'com1_1')
subscil
```

```
## 
## # A scimeetr object #
## ---------------------
## Number of papers:  202
## Number of communities:  4
## Names of communities:  com1_1 com1_1_2 com1_1_1 com1_1_3
## 
## Table of the 5 most mentionned words 
## 
##       key_words       title_words abstract_words
## 1 PARTICIPATION           FARMERS        FARMERS
## 2   AGRICULTURE            POLICY         POLICY
## 3  CONSERVATION AGRIENVIRONMENTAL  ENVIRONMENTAL
## 4       FARMERS      AGRICULTURAL   AGRICULTURAL
## 5    MANAGEMENT           SCHEMES           FARM
```

<a href="#top">Back to top</a>
