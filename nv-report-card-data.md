


NV Report Card Data
========================================================

Shawn sent around some data pulled from [Nevada Report Card](http://www.nevadareportcard.com). 
He mentioned that the test results weren't available on the NRC site. We will find another way.

## Introduction

```r
library(RCurl) # Parsing Excel files
library(RJSONIO)
library(plyr)         # libxml for parsing HTML 
library(data.table)
library(stringr)
library(knitr)
```


## Scraping cohort exam data
### Getting our bearings

Making use of [`RCurl`]()

```bash
curl 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationHierarchyTree?organization=64825' -H 'Cookie: sid=05116CCF8FFBB0648F2C7273580309964292E36AE33840C4FA0594FE6462CA195159FCF65C5A58FFB3DA8E23B57356DF02F16C41CA8040FDF361CAB2A13FDA3D1D821ACDAFEAF60561EB7E717D719D520AE7175ACE93E96595D8846FF2E5DF13AFE51350B46F17D186F9EAC2EA4A368508B36C84191D432D0646081FBC23EDE2' -H 'Accept-Encoding: gzip,deflate,sdch' -H 'Accept-Language: en-US,en;q=0.8' -H 'User-Agent: Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.102 Safari/537.36' -H 'Accept: application/json, text/javascript, */*; q=0.01' -H 'Referer: http://www.nevadareportcard.com/di/main/assessment' -H 'X-Requested-With: XMLHttpRequest' -H 'Connection: keep-alive' --compressed
```

Using `%s/ -H/, /g`, we get the following


```r
hierarchy_header <- c("Cookie: sid=05116CCF8FFBB0648F2C7273580309964292E36AE33840C4FA0594FE6462CA195159FCF65C5A58FFB3DA8E23B57356DF02F16C41CA8040FDF361CAB2A13FDA3D1D821ACDAFEAF60561EB7E717D719D520AE7175ACE93E96595D8846FF2E5DF13AFE51350B46F17D186F9EAC2EA4A368508B36C84191D432D0646081FBC23EDE2", 
    "Accept-Encoding: gzip,deflate,sdch", "Accept-Language: en-US,en;q=0.8", 
    "User-Agent: Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.102 Safari/537.36", 
    "Accept: application/json, text/javascript, */*; q=0.01", 
    "Referer: http://www.nevadareportcard.com/di/main/assessment", 
    "X-Requested-With: XMLHttpRequest", "Connection: keep-alive")
```



```r
org_chart <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationHierarchyTree?organization=64825", 
    httpheader = hierarchy_header, encoding = "gzip")
org <- fromJSON(org_chart, asText = TRUE)
```





### Extracting Clark
We expect that counties will be at the second node of our tree. Verify by visual inspection:


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


So the second node is our target. We can infer the meaning of each type:


```r
head(unlist(org), n = 15)
```

```
##                   organization.id            organization.parent_id 
##                           "64825"                              "-1" 
##                 organization.code                 organization.name 
##                              "00"                           "State" 
##                 organization.type                 organization.demo 
##                               "S"                           "FALSE" 
##                 organization.home          children.organization.id 
##                            "TRUE"                           "64826" 
##   children.organization.parent_id        children.organization.code 
##                           "64825"                              "01" 
##        children.organization.name        children.organization.type 
##                       "Churchill"                               "D" 
##        children.organization.demo        children.organization.home 
##                           "FALSE"                           "FALSE" 
## children.children.organization.id 
##                           "64844"
```

```r
tail(unlist(org), n = 15)
```

```
##      children.children.organization.home 
##                                  "FALSE" 
##        children.children.organization.id 
##                                  "65526" 
## children.children.organization.parent_id 
##                                  "64843" 
##      children.children.organization.code 
##                                  "18424" 
##      children.children.organization.name 
##           "Honors Academy of Literature" 
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


Note the first `organization.id`, `64825`, which is the same as our `GET` request.
From the rest we can infer that `S` = State, `D` = District, and `B` = School. We assume that `A` and `C` are not used. Checking:


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
While this script can be modified to download and parse AYP reports for an arbitrary subset of
Nevada schools, we are limiting ourselves to collecting data for the following 
schools:


```r
da_list <- list( 
    # concatenate with school level
    da_es_1 = paste(c("Hollingsworth", "Crestwood", "Lake", "Park"), "ES", sep = " "),
    da_es_2 = paste(c("Fyfe" , "McWilliams", "Twin Lakes"),  "ES", sep = " "),
    da_ms   = paste(c("Fremont", "Gibson (Robert)"), "MS", sep = " "),
    da_hs   = paste(c("Valley", "Western"), "HS", sep = " ")
    )
```


Because we may want to consider each pipeline seperately, we also create two character 
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


We'll select rows frow our `ccsd.DT` data table by passing it our list of names:


```r
da.DT <- ccsd.DT[unlist(da_list)]
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
   <td> Hollingsworth ES </td>
   <td> 65053 </td>
   <td> 64827 </td>
   <td> 02273 </td>
   <td> B </td>
   <td> FALSE </td>
   <td> FALSE </td>
  </tr>
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
   <td> Lake ES </td>
   <td> 65017 </td>
   <td> 64827 </td>
   <td> 02237 </td>
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
   <td> Fyfe ES </td>
   <td> 65020 </td>
   <td> 64827 </td>
   <td> 02240 </td>
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
   <td> Twin Lakes ES </td>
   <td> 65023 </td>
   <td> 64827 </td>
   <td> 02243 </td>
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
   <td> Gibson (Robert) MS </td>
   <td> 65090 </td>
   <td> 64827 </td>
   <td> 02310 </td>
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
  3. `yn`, where *n* is year post NCLB, ranging from 1 (2003) to 10 (2012)

(These variables, and others, can also be described programmatically via the API)

The set of scores available for each test vary, and will need to be specified for each
one. We include them in our retreival function, but these can also be edited to narrow
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


Now we'll download the results of the Criteron Referenced Test (exam `e1`) for Grade 3 (`g3`), and years
2007 (`y3`) through 2012 (`y10`):


```r
scopestr <- paste(c("e1", "g3", paste0("y", 3:10)), collapse = ".")
da.crt.g3 <- getDiCSV(da.DT$id, scopestr, "e1", csv_header)
```


Printing the first ten rows:
<script type="text/javascript" charset="utf-8">
  $(document).ready(function() {
  	$('#da_table').dataTable();
	} );
</script>
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


Try again, but this time disaggregate based upon ethnicity:


```r
da.crt.g3.e <- getDiCSV(da.DT$id, scopestr, "e1", csv_header, 
    ethnicity = TRUE)
```


The first 10 rows:

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


However, disaggregation has its pitfalls, as test results are supressed where $n\le20$

### Accessing demographic and profile data

Test scores, in isolation,  are removed from their context and are of limited
utility. 





















