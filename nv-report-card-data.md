


NV Report Card Data
========================================================

Last week, Shawn Looker sent around some data pulled from 
[Nevada Report Card](http://www.nevadareportcard.com). Because we'll get the bulk
of our data from there, I took a look at what was available.

Obviously, not everyone will want to wade through an R script, so you can skip to the end for
for a toy example.

## Preliminaries

We'll make use of the following libraries in this script:


```r
library(RCurl) 
library(RJSONIO)
library(plyr)          
library(data.table)
library(stringr)
library(knitr)
library(ggmap)
```


## Getting cohort exam data

Standardized test results are our primary goal. In order to access the data, we'll need
to examine the DI API to determine the most efficient way to proceed.

### Getting our bearings

Begin by getting the Organization Hierarchy in order to get a list of all schools
in CCSD:


```r
org_chart <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationHierarchyTree?organization=64825", 
    encoding = "gzip")
org <- fromJSON(org_chart, asText = TRUE)
```


### Extracting Clark

We expect that the counties will be at the second node of our tree. Verify:


```r
# Check for counties at the second level
unlist(lapply(org$children, function(x) x$organization$name))
```

```
##  [1] "Churchill"      "Clark"          "Douglas"        "Elko"          
##  [5] "Esmeralda"      "Eureka"         "Humboldt"       "Lander"        
##  [9] "Lincoln"        "Lyon"           "Mineral"        "Nye"           
## [13] "Carson City"    "Pershing"       "Storey"         "Washoe"        
## [17] "White Pine"     "State Charters"
```

```r

# double checking
org$children[[2]]$organization$name
```

```
## [1] "Clark"
```


Clark County is our subject. From visual inspection We can infer the
meaning of each `type`:


```r
head(unlist(org), n = 10)
```

```
##                 organization.id          organization.parent_id 
##                         "64825"                            "-1" 
##               organization.code               organization.name 
##                            "00"                         "State" 
##               organization.type               organization.demo 
##                             "S"                         "FALSE" 
##               organization.home        children.organization.id 
##                          "TRUE"                         "64826" 
## children.organization.parent_id      children.organization.code 
##                         "64825"                            "01"
```

```r
tail(unlist(org), n = 10)
```

```
##      children.children.organization.type 
##                                      "B" 
##      children.children.organization.demo 
##                                  "FALSE" 
##      children.children.organization.home 
##                                  "FALSE" 
##        children.children.organization.id 
##                                  "65527" 
## children.children.organization.parent_id 
##                                  "64843" 
##      children.children.organization.code 
##                                  "18903" 
##      children.children.organization.name 
##                        "Independence HS" 
##      children.children.organization.type 
##                                      "B" 
##      children.children.organization.demo 
##                                  "FALSE" 
##      children.children.organization.home 
##                                  "FALSE"
```


Note the first `organization.id`, `64825`, which is the same as our `GET`
request. From the rest we can infer that `S` = State, `D` = District, and `B` =
School. We assume that `A` and `C` are not used. Checking:


```r
table(unlist(org)[grep("organization.type", names(unlist(org)))])
```

```
## 
##   B   D   S 
## 684  18   1
```


Checks out. Now we subset CCSD:


```r
clark_org <- org$children[[2]]
unlist(clark_org$organization)
```

```
##        id parent_id      code      name      type      demo      home 
##   "64827"   "64825"      "02"   "Clark"       "D"   "FALSE"   "FALSE"
```


Finally, we flatten the JSON into a `data.table`

```r
# ldply: split list, apply as.data.table, combine into
# data.frame
ccsd.DT <- ldply(clark_org$children, function(x) as.data.table(x[1]$organization))
ccsd.DT <- data.table(ccsd.DT, key = c("name", "id"))
```


### Extracting subjects

We now have CCSD isolated, we can pull the info for our Downtown Achieves cohort.
This script can be modified for an arbitrary subset of schools, here we are collecting
data for the following schools:


```r
da_list <- list( 
    # concatenate with school level
    da_es_1 = paste(c("Hollingsworth", "Crestwood", "Lake", "Park"), "ES", sep = " "),
    da_es_2 = paste(c("Fyfe" , "McWilliams", "Twin Lakes"),  "ES", sep = " "),
    da_ms   = paste(c("Fremont", "Gibson (Robert)"), "MS", sep = " "),
    da_hs   = paste(c("Valley", "Western"), "HS", sep = " ")
    )
```


Because we may want to consider each pipeline separately, we also create two character 
vectors to hold their names:


```r
da_pipeline_1 <- c(da_list$da_es_1, da_list$da_ms[1], da_list$da_hs[1])
da_pipeline_2 <- c(da_list$da_es_2, da_list$da_ms[2], da_list$da_hs[2])

da_pipeline_1
```

```
## [1] "Hollingsworth ES" "Crestwood ES"     "Lake ES"         
## [4] "Park ES"          "Fremont MS"       "Valley HS"
```

```r
da_pipeline_2
```

```
## [1] "Fyfe ES"            "McWilliams ES"      "Twin Lakes ES"     
## [4] "Gibson (Robert) MS" "Western HS"
```


We'll select rows from our `ccsd.DT` data table by passing it our list of names
as keys:


```r
da.DT <- ccsd.DT[unlist(da_list)]
setkeyv(da.DT, c("name", "id"))
```


<table>
 <thead>
  <tr>
   <th> name </th>
   <th> id </th>
   <th> parent_id </th>
   <th> code </th>
   <th> type </th>
   <th> demo </th>
   <th> home </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> Crestwood ES </td>
   <td> 64985 </td>
   <td> 64827 </td>
   <td> 02205 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Fremont MS </td>
   <td> 65088 </td>
   <td> 64827 </td>
   <td> 02308 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Fyfe ES </td>
   <td> 65020 </td>
   <td> 64827 </td>
   <td> 02240 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Gibson (Robert) MS </td>
   <td> 65090 </td>
   <td> 64827 </td>
   <td> 02310 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Hollingsworth ES </td>
   <td> 65053 </td>
   <td> 64827 </td>
   <td> 02273 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Lake ES </td>
   <td> 65017 </td>
   <td> 64827 </td>
   <td> 02237 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> McWilliams ES </td>
   <td> 64998 </td>
   <td> 64827 </td>
   <td> 02218 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Park ES </td>
   <td> 64996 </td>
   <td> 64827 </td>
   <td> 02216 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Twin Lakes ES </td>
   <td> 65023 </td>
   <td> 64827 </td>
   <td> 02243 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Valley HS </td>
   <td> 65147 </td>
   <td> 64827 </td>
   <td> 02404 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
  <tr>
   <td> Western HS </td>
   <td> 65148 </td>
   <td> 64827 </td>
   <td> 02405 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
</tbody>
</table>


In order to query arbitrary sets of schools we must generate a collection hash


```r
getOrgCollnHash <- function(ids, hdr, vb = FALSE) {
    r = dynCurlReader()
    ids <- paste0("=", paste(ids, collapse = ","))
    curlPerform(postfields = ids, url = "http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationCollectionHash", 
        verbose = vb, .opts = (httpheader = hdr), post = 1L, 
        writefunction = r$update)
    paste0("c", r$value())
}
```


We run this function against our targeted schools and collect the hash for future use:


```r
csv_header <- c("Cookie: sid=BCEA0A1BA4249DF33EC03E7A0269DE101FC829462BD8EE8CEBDDF0C46FC9AFFDBD6C1F56D896E3296DEDD9ECE4CFB1E633EBD9D850D7A68126D9B7264653E8CC282DD2CAB5D364513DCF3E71D4481FF3E5D937BFE72E31A1EBECC6733E798322E34B4838A4265E34D3449D6EEFB52C7C24EED7E6484621603BC2F488B1E8C7AC", 
    "Accept-Language: en-US,en;q=0.8", "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.77 Safari/537.36")
rg <- getOrgCollnHash(da.DT$id, csv_header)
rg
```

```
## [1] "c4939"
```


### Accessing the CSV API

Inspection of the URL used to download the CRT scores for another group shows:


```r
getFormParams("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization=c2272&scope=e5.g3.y10&scores=N_MA,MA_pass,MA_AMO,MA_level,N_RD,RD_pass,RD_AMO,RD_level")
```

```
##                                                      report 
##                                                 "summary_1" 
##                                                organization 
##                                                     "c2272" 
##                                                       scope 
##                                                 "e5.g3.y10" 
##                                                      scores 
## "N_MA,MA_pass,MA_AMO,MA_level,N_RD,RD_pass,RD_AMO,RD_level"
```


The `organization` parameter is the hash generated by `getOrgCollnHash()`. The 
`scope` is constructed from:

  1. `en`, where *n* is 1, 2, 5, or 6,
  2. `gn`, where *n* is grade number,
  3. `yn`, where *n* is year post-NCLB, ranging from 1 (2003) to 10 (2012)

(These variables, and others, can also be described programmatically via the API)

The set of scores available for each test vary, and will need to be specified for each
exam. We hard code them in our retrieval function, but these can easily be edited to narrow
the query to specific variables. 

 1. `e1`, years 3:10; grades 3:8; CRT
 3. `e2`, year 3:10; grade 11; High School Proficiency Exam
 4. `e5`, years 3:10, grades 3:8, 11;NAA
 2. `e6`, years 3:7; grades 3,5; Writing

We'll retrieve each table with the following function:


```r
getDiCSV <- function(org, scope, exam, header, ethnicity = FALSE, 
    gender = FALSE, iep = FALSE, frl = FALSE, filterdata = NULL, 
    rel = "and") {
    # report parameter
    org <- getOrgCollnHash(org, header)
    # scores parameter
    score <- list(e1 = "N_MA,MA_SS,MA_pass,MA_AMO,MA_level,N_RD,RD_SS,RD_pass,RD_AMO,RD_level", 
        e2 = "N_MA,MA_SS,MA_pass,MA_AMO,MA_level,N_RD,RD_SS,RD_pass,RD_AMO,RD_level,N_SC,SC_SS,SC_pass,SC_AMO,SC_level,N_WR,WR_pass,WR_AMO,WR_level", 
        e5 = "N_MA,MA_pass,MA_AMO,MA_level,N_RD,RD_pass,RD_AMO,RD_level", 
        e6 = "N_WR,WR_pass,WR_AMO,WR_level")
    target <- paste0("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization=", 
        org, "&scope=", scope, "&scores=", score[[exam]])
    
    # inelegant disaggregation selection
    subgroup <- ""
    if (ethnicity == TRUE) 
        subgroup <- paste0("ethnicity,", subgroup)
    if (gender == TRUE) 
        subgroup <- paste0("gender,", subgroup)
    if (iep == TRUE) 
        subgroup <- paste0("iep,", subgroup)
    if (frl == TRUE) 
        subgroup <- paste0("frl,", subgroup)
    
    # if any of the above, concatenate with target
    if (subgroup != "") {
        target <- paste0(target, "&subgroups=", sub(",$", 
            "", subgroup), "&filterrelation=and")
    }
    
    # filtering on subpopulation, but by passing a string
    # argument instead if present, subgroups above are
    # ignored
    if (is.character(filterdata)) {
        
        # convert .  getFilterKeys <- function(filterdatum) { .
        # # split and combine .  filterkey <-
        # strsplit(filterdatum, split = '.') .  paste(filterkey,
        # collapse = '_') .  }
        filterkeys <- gsub(".", "_", filterdata, exact = TRUE)
        
        target <- paste0(target, "&filterkey=", filterkeys, 
            "&filterdata=", filterdata, "&filterrelation=", 
            rel)
    }
    # target make it so
    resultsText <- getURL(target)
    
    # create temporary connection from results and return
    # data.table
    data.table(read.csv(text = resultsText))
}
```


Now we'll download the results of the Criterion Referenced Test (exam `e1`) for Grade 3 (`g3`), and years
2007 (`y3`) through 2012 (`y10`):


```r
scopestr <- paste(c("e1", "g3", paste0("y", 3:10)), collapse = ".")
da.crt.g3 <- getDiCSV(da.DT$id, scopestr, "e1", csv_header)
```


Printing the first six rows:

<table id="da_table">
 <thead>
  <tr>
   <th> Group </th>
   <th> Year </th>
   <th> Grade </th>
   <th> Number.Enrolled </th>
   <th> Mathematics...Number.Tested </th>
   <th> Mathematics...Mean.Scale.Score </th>
   <th> Mathematics.....Proficient </th>
   <th> Mathematics.....Above.AMO </th>
   <th> Mathematics...Emergent.Developing </th>
   <th> Mathematics...Approaches.Standard </th>
   <th> Mathematics...Meets.Standard </th>
   <th> Mathematics...Exceeds.Standard </th>
   <th> Reading...Number.Tested </th>
   <th> Reading...Mean.Scale.Score </th>
   <th> Reading.....Proficient </th>
   <th> Reading.....Above.AMO </th>
   <th> Reading...Emergent.Developing </th>
   <th> Reading...Approaches.Standard </th>
   <th> Reading...Meets.Standard </th>
   <th> Reading...Exceeds.Standard </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> Crestwood ES </td>
   <td> 2007-2008 </td>
   <td> 3 </td>
   <td> 126 </td>
   <td> 126 </td>
   <td> 316.8 </td>
   <td> 64.3 </td>
   <td>   8.0 </td>
   <td>  7.1 </td>
   <td> 28.6 </td>
   <td> 35.7 </td>
   <td> 28.6 </td>
   <td> 126 </td>
   <td> 298.0 </td>
   <td> 54.0 </td>
   <td>   2.3 </td>
   <td>  8.7 </td>
   <td> 37.3 </td>
   <td> 29.4 </td>
   <td> 24.6 </td>
  </tr>
  <tr>
   <td> Crestwood ES </td>
   <td> 2008-2009 </td>
   <td> 3 </td>
   <td> 114 </td>
   <td> 114 </td>
   <td> 312.0 </td>
   <td> 52.6 </td>
   <td>  -3.7 </td>
   <td>  6.1 </td>
   <td> 41.2 </td>
   <td> 23.7 </td>
   <td> 28.9 </td>
   <td> 114 </td>
   <td> 302.4 </td>
   <td> 57.0 </td>
   <td>   5.3 </td>
   <td>  4.4 </td>
   <td> 38.6 </td>
   <td> 40.4 </td>
   <td> 16.7 </td>
  </tr>
  <tr>
   <td> Crestwood ES </td>
   <td> 2009-2010 </td>
   <td> 3 </td>
   <td> 108 </td>
   <td> 108 </td>
   <td> 305.0 </td>
   <td> 53.7 </td>
   <td> -12.2 </td>
   <td> 16.7 </td>
   <td> 29.6 </td>
   <td> 32.4 </td>
   <td> 21.3 </td>
   <td> 108 </td>
   <td> 296.1 </td>
   <td> 50.0 </td>
   <td> -13.8 </td>
   <td>  4.6 </td>
   <td> 45.4 </td>
   <td> 34.3 </td>
   <td> 15.7 </td>
  </tr>
  <tr>
   <td> Crestwood ES </td>
   <td> 2010-2011 </td>
   <td> 3 </td>
   <td> 112 </td>
   <td> 112 </td>
   <td> 345.1 </td>
   <td> 80.4 </td>
   <td>  14.5 </td>
   <td>  9.8 </td>
   <td>  9.8 </td>
   <td> 37.5 </td>
   <td> 42.9 </td>
   <td> 112 </td>
   <td> 301.0 </td>
   <td> 57.1 </td>
   <td>  -6.7 </td>
   <td> 25.0 </td>
   <td> 17.9 </td>
   <td> 36.6 </td>
   <td> 20.5 </td>
  </tr>
  <tr>
   <td> Crestwood ES </td>
   <td> 2011-2012 </td>
   <td> 3 </td>
   <td> 120 </td>
   <td> 120 </td>
   <td> 322.4 </td>
   <td> 70.8 </td>
   <td>  -2.7 </td>
   <td>  8.3 </td>
   <td> 20.8 </td>
   <td> 42.5 </td>
   <td> 28.3 </td>
   <td> 120 </td>
   <td> 294.1 </td>
   <td> 48.3 </td>
   <td> -17.5 </td>
   <td> 20.8 </td>
   <td> 30.8 </td>
   <td> 34.2 </td>
   <td> 14.2 </td>
  </tr>
  <tr>
   <td> Crestwood ES </td>
   <td> 2012-2013 </td>
   <td> 3 </td>
   <td> 104 </td>
   <td> 104 </td>
   <td> 314.3 </td>
   <td> 57.7 </td>
   <td> -13.9 </td>
   <td> 14.4 </td>
   <td> 27.9 </td>
   <td> 29.8 </td>
   <td> 27.9 </td>
   <td> 104 </td>
   <td> 289.0 </td>
   <td> 41.3 </td>
   <td> -26.1 </td>
   <td> 30.8 </td>
   <td> 27.9 </td>
   <td> 24.0 </td>
   <td> 17.3 </td>
  </tr>
</tbody>
</table>


Try again, but this time dis-aggregate based upon ethnicity:


```r
da.crt.g3.e <- getDiCSV(da.DT$id, scopestr, "e1", csv_header, 
    ethnicity = TRUE)
```


The first six rows:

<table>
 <thead>
  <tr>
   <th> Group </th>
   <th> Year </th>
   <th> Grade </th>
   <th> Number.Enrolled </th>
   <th> Mathematics...Number.Tested </th>
   <th> Mathematics...Mean.Scale.Score </th>
   <th> Mathematics.....Proficient </th>
   <th> Mathematics.....Above.AMO </th>
   <th> Mathematics...Emergent.Developing </th>
   <th> Mathematics...Approaches.Standard </th>
   <th> Mathematics...Meets.Standard </th>
   <th> Mathematics...Exceeds.Standard </th>
   <th> Reading...Number.Tested </th>
   <th> Reading...Mean.Scale.Score </th>
   <th> Reading.....Proficient </th>
   <th> Reading.....Above.AMO </th>
   <th> Reading...Emergent.Developing </th>
   <th> Reading...Approaches.Standard </th>
   <th> Reading...Meets.Standard </th>
   <th> Reading...Exceeds.Standard </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td> Crestwood ES </td>
   <td> 2007-2008 </td>
   <td> 3 </td>
   <td> 126 </td>
   <td> 126 </td>
   <td> 316.8 </td>
   <td> 64.3 </td>
   <td> 8.0 </td>
   <td> 7.1 </td>
   <td> 28.6 </td>
   <td> 35.7 </td>
   <td> 28.6 </td>
   <td> 126 </td>
   <td> 298.0 </td>
   <td> 54.0 </td>
   <td> 2.3 </td>
   <td> 8.7 </td>
   <td> 37.3 </td>
   <td> 29.4 </td>
   <td> 24.6 </td>
  </tr>
  <tr>
   <td> Black </td>
   <td> 2007-2008 </td>
   <td> 3 </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
  </tr>
  <tr>
   <td> Hispanic </td>
   <td> 2007-2008 </td>
   <td> 3 </td>
   <td> 105 </td>
   <td> 105 </td>
   <td> 312.8 </td>
   <td> 61.9 </td>
   <td> 5.6 </td>
   <td> 7.6 </td>
   <td> 30.5 </td>
   <td> 38.1 </td>
   <td> 23.8 </td>
   <td> 105 </td>
   <td> 292.4 </td>
   <td> 50.5 </td>
   <td> -1.2 </td>
   <td> 9.5 </td>
   <td> 40.0 </td>
   <td> 29.5 </td>
   <td> 21.0 </td>
  </tr>
  <tr>
   <td> White </td>
   <td> 2007-2008 </td>
   <td> 3 </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
  </tr>
  <tr>
   <td> Asian </td>
   <td> 2007-2008 </td>
   <td> 3 </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
   <td> - </td>
  </tr>
  <tr>
   <td> Crestwood ES </td>
   <td> 2008-2009 </td>
   <td> 3 </td>
   <td> 114 </td>
   <td> 114 </td>
   <td> 312.0 </td>
   <td> 52.6 </td>
   <td> -3.7 </td>
   <td> 6.1 </td>
   <td> 41.2 </td>
   <td> 23.7 </td>
   <td> 28.9 </td>
   <td> 114 </td>
   <td> 302.4 </td>
   <td> 57.0 </td>
   <td> 5.3 </td>
   <td> 4.4 </td>
   <td> 38.6 </td>
   <td> 40.4 </td>
   <td> 16.7 </td>
  </tr>
</tbody>
</table>


As we can see, dis-aggregation has its pitfalls: test results are suppressed for
any group where $n\le20$ 

From here, one can filter for complete cases, reshape data, tabulate, etc.
Next, an overview of the non-assessment data on NRC.

## Getting NRC data

### Accessing demographic and profile data

Test scores alone are of limited value. Additional context is provided by
several addditional datasets on NRC. 

#### Adequate Yearly Progress


```r
all.ccsd.ayp.csv.txt <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5376&scope=e13.y1.y2.y3.y4.y5.y6.y7.y8.y9&scores=852,853,854")
ccsd.ayp.DT <- data.table(read.csv(text = all.ccsd.ayp.csv.txt))
```


#### Demographics
Download all count and percentage demographic data:


```r
all.ccsd.demo.csv.txt <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e7.y1.y10.y2.y4.y5.y6.y7.y8.y9&scores=1026,566,567,568,569,570,571,572,573,574,575,805,576,577,806,586,587,588,589,578,579,580,581,582,583,584,585")
ccsd.demo.DT <- data.table(read.csv(text = all.ccsd.demo.csv.txt))
```


#### Personnel

Download all personnel data (e.g., % of highly qualified teachers):


```r
all.ccsd.pers.csv.txt <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e12.y1.y10.y2.y4.y5.y6.y7.y8.y9&scores=779,780,781,782,851,783,784,785,786,787,788,789,790,791,792,793,795,796,1029,797,798,799,800,801,802,803,760,761,762,856,763,765,767,769,771,764,766,768,770,772,775,773,777,776,774,778")
ccsd.pers.DT <- data.table(read.csv(text = all.ccsd.pers.csv.txt))
```


#### Technology

Download all Technology data:

```r
ccsd.tech2011.csv <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e17.y1.y2.y3.y4.y5.y6&scores=590,591,592,593,594,595")
ccsd.tech2012.csv <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e8.y10.y7&scores=809,810,811,812,813,814,815")
ccsd.tech.DT <- data.table(rbind.fill(read.csv(text = ccsd.tech2011.csv), 
    read.csv(text = ccsd.tech2012.csv)))
```


### Historical profiles

Historical profiles are analogous to metadata for each school year. These include the name
of each school's principal, addresses, phone numbers,  and names of school board
members and more. Also included are goals, such as the "percent[age] of
non-proficient math students in the Class of 2006 will be reduced by at least
50%." ([Advanced Technologies Academy](http://www.atech.org/), 2005) 


```r

getProfiles <- function(org) {
    years <- paste0("y", 1:10)
    
    # Create a list of lists containing searches for each
    # year
    searches <- lapply(years, function(x) structure(c("profile_1", 
        "profile", x, org), .Names = c("report", "reportID", 
        "scope", "organization")))
    prof.json.lst <- lapply(searches, function(search) {
        getForm("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/Profile", 
            .params = search, .opts = (httpheader = "Accept: application/json, text/javascript, */*; q=0.01"))
    })
    prof.list <- lapply(prof.json.lst, fromJSON)
    names(prof.list) <- years
    prof.list
}
```



## Example

This is already far too long, but I want to shoehorn in a few applications. 
We'll download the profiles of Valley High School, one of our DA schools:


```r
valley.prof.list <- getProfiles(da.DT["Valley HS"]$id)
```


### Principal turnover

How many principals has Valley had in the last decade?


```r
# Finding principals
table(sapply(valley.prof.list, function(x) x$superintendent))
```

```
## 
## Deann R. Burnett, Principal      Ron Montoya, Principal 
##                           2                           8
```


### Geocoding

We can geocode Valley's address:


```r
# Geocoding
valley.address <- paste(valley.prof.list$y1$address, valley.prof.list$y1$zip)
valley.address
```

```
## [1] "2839 S Burnham Ave 89109"
```

```r
valley.latlong <- geocode(valley.address)
```


then map the surrounding area:


```r
sta <- get_map(location = c(lon = valley.latlong$lon, lat = valley.latlong$lat), 
    zoom = 15, crop = TRUE, source = "stamen")
ggmap(sta)
```

![Area near Valley HS](figure/toyExample-os.png) 



### Other data sources

What crimes have been reported within a one mile radius since 28 August 2013?


```r

crimes <- getURL("http://www.crimemapping.com/GetIncidents.aspx?db=8/28/2013+00:00:00&de=2/03/2014+23:59:00&ccs=AR,AS,BU,DP,DR,DU,FR,HO,VT,RO,SX,TH,VA,VB,WE&add=2839%20Burnham%20Ave%2C%20Las%20Vegas%2C%20Nevada%2C%2089169&bcy=4319584.042143912&bcx=-12815448.676366305&br=1.0&xmin=-12818038.894702934&ymin=4318442.044425546&xmax=-12813524.33271972&ymax=4321389.647328871")

crimes <- fromJSON(crimes)

table(sapply(crimes$incidents, function(x) x$CrimeCode))
```

```
## 
##              Assault             Burglary Disturbing the Peace 
##                   41                  152                  153 
##  Motor Vehicle Theft              Robbery           Sex Crimes 
##                   79                   21                    1 
##        Theft/Larceny            Vandalism              Weapons 
##                    1                   50                    7
```


We can also get additional data about the people who live there:


```r
dsk <- getURL("http://www.datasciencetoolkit.org/coordinates2statistics/36.13727%2c-115.1217")
dsk <- fromJSON(dsk)
```



```r
dsk[[1]]$statistics$us_population_poverty
```

```
## $description
## [1] "The proportion of residents whose income is below the poverty level"
## 
## $value
## [1] 0.1067
## 
## $proportion_of
## [1] 1818
## 
## $source_name
## [1] "US Census and the CGIAR Consortium for Spatial Information"
```

```r
dsk[[1]]$statistics$us_population_low_income
```

```
## $description
## [1] "The proportion of residents who earn less than twice the poverty level"
## 
## $value
## [1] 0.2536
## 
## $proportion_of
## [1] 1818
## 
## $source_name
## NULL
```



```r
dsk[[1]]$statistics$us_households_single_mothers
```

```
## $description
## [1] "The proportion of households with a female householder, no husband, and one or more children under eighteen."
## 
## $value
## [1] 0.07375
## 
## $proportion_of
## [1] 678
## 
## $source_name
## [1] "US Census and the CGIAR Consortium for Spatial Information"
```

```r
dsk[[1]]$statistics$us_sample_area
```

```
## $description
## [1] "The total area of the grid cell US Census samples were calculated on."
## 
## $value
## [1] 693096
## 
## $units
## [1] "square meters"
## 
## $source_name
## [1] "US Census and the CGIAR Consortium for Spatial Information"
```



```r
save.image(file = "nv-report-card-data.RData", ascii = TRUE)
sessionInfo()
```

```
## R version 3.0.2 (2013-09-25)
## Platform: i686-pc-linux-gnu (32-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggmap_2.3         ggplot2_0.9.3.1   stringr_0.6.2     data.table_1.8.10
## [5] plyr_1.8          RJSONIO_1.0-3     RCurl_1.95-4.1    bitops_1.0-6     
## [9] knitr_1.5        
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-4    dichromat_2.0-0     digest_0.6.4       
##  [4] evaluate_0.5.1      formatR_0.10        grid_3.0.2         
##  [7] gtable_0.1.2        labeling_0.2        mapproj_1.2-2      
## [10] maps_2.3-6          MASS_7.3-29         munsell_0.4.2      
## [13] png_0.1-7           proto_0.3-10        RColorBrewer_1.0-5 
## [16] reshape2_1.2.2      RgoogleMaps_1.2.0.5 rjson_0.2.13       
## [19] scales_0.2.3        tools_3.0.2
```














