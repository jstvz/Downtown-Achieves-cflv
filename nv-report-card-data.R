## ----knitr-opts, echo=FALSE, message=FALSE-------------------------------
library(knitr)
opts_chunk$set(tidy.opts=list(width.cutoff=55))
#opts_knit$set(self.contained=FALSE)

options(rstudio.markdownToHTML =
  function(inputFile, outputFile) {
    require(knitrBootstrap)
    knit_bootstrap_md(input=inputFile, output=outputFile,
                      code_style = "Zenburn", boot_style = "Cerulean",
                      show_code = TRUE, show_output = TRUE,
                      chooser = NULL, markdown_options("highlight_code")
                      
                      )
  }
)
# options(rstudio.markdownToHTML = 
#   function(inputFile, outputFile) {      
#     system(paste("pandoc", shQuote(inputFile), "-o", shQuote(outputFile)))
#   }
# )  
    # options(rstudio.markdownToHTML = 
    #   function(inputFile, outputFile) {      
    #     require(markdown)
    #     markdownToHTML(inputFile, outputFile)   
    #   }
    # )



## ----libraries, message=FALSE, tidy=FALSE--------------------------------
library(RCurl) 
library(RJSONIO)
library(plyr)          
library(data.table)
library(stringr)
library(knitr)
library(ggmap)


## ----org-header----------------------------------------------------------
hierarchy_header <- c('Cookie: sid=05116CCF8FFBB0648F2C7273580309964292E36AE33840C4FA0594FE6462CA195159FCF65C5A58FFB3DA8E23B57356DF02F16C41CA8040FDF361CAB2A13FDA3D1D821ACDAFEAF60561EB7E717D719D520AE7175ACE93E96595D8846FF2E5DF13AFE51350B46F17D186F9EAC2EA4A368508B36C84191D432D0646081FBC23EDE2',  'Accept-Encoding: gzip,deflate,sdch',  'Accept-Language: en-US,en;q=0.8',  'User-Agent: Mozilla/5.0 (X11; Linux i686) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.102 Safari/537.36',  'Accept: application/json, text/javascript, */*; q=0.01',  'Referer: http://www.nevadareportcard.com/di/main/assessment',  'X-Requested-With: XMLHttpRequest',  'Connection: keep-alive')


## ----get_orgs, eval=FALSE------------------------------------------------
## org_chart <- getURL('http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationHierarchyTree?organization=64825', httpheader = hierarchy_header, encoding='gzip')
## org <- fromJSON(org_chart, asText=TRUE)


## ----check-orgs----------------------------------------------------------
 # Check for counties at the second level
unlist(lapply(org$children, function (x) x$organization$name))

  # double checking
org$children[[2]]$organization$name


## ----org-list------------------------------------------------------------
head(unlist(org), n=10)
tail(unlist(org), n=10)


## ----check-type-var------------------------------------------------------
table(unlist(org)[grep("organization.type", names(unlist(org)))])


## ----subset-clark-co-----------------------------------------------------
clark_org <- org$children[[2]]
unlist(clark_org$organization)


## ----jsonToDT------------------------------------------------------------
 # ldply: split list, apply as.data.table, combine into data.frame
ccsd.DT <- ldply(clark_org$children, function (x) as.data.table(x[1]$organization))
ccsd.DT <- data.table(ccsd.DT, key = c("name", "id"))


## ----generate-da-names---------------------------------------------------
da_list <- list( 
    # concatenate with school level
    da_es_1 = paste(c("Hollingsworth", "Crestwood", "Lake", "Park"), "ES", sep = " "),
    da_es_2 = paste(c("Fyfe" , "McWilliams", "Twin Lakes"),  "ES", sep = " "),
    da_ms   = paste(c("Fremont", "Gibson (Robert)"), "MS", sep = " "),
    da_hs   = paste(c("Valley", "Western"), "HS", sep = " ")
    )


## ----generate-da-pipelines-----------------------------------------------
da_pipeline_1 <- c(da_list$da_es_1, da_list$da_ms[1], da_list$da_hs[1])
da_pipeline_2 <- c(da_list$da_es_2, da_list$da_ms[2], da_list$da_hs[2])

da_pipeline_1
da_pipeline_2


## ----getDA---------------------------------------------------------------
da.DT <- ccsd.DT[unlist(da_list)]
setkeyv(da.DT, c("name", "id"))


## ----print-da-table, results='asis', echo=FALSE--------------------------
kable(da.DT, format = 'html')


## ----def-getOrgCollnHash-------------------------------------------------
getOrgCollnHash <- function(ids, hdr, vb = FALSE) {
  r = dynCurlReader()
  ids <- paste0("=", paste(ids, collapse = ','))
  curlPerform(postfields = ids, url = 'http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/OrganizationCollectionHash', verbose = vb, .opts=(httpheader=hdr),
  post = 1L, writefunction = r$update)
  paste0("c", r$value())
}


## ----getDaHash-----------------------------------------------------------
csv_header <- c('Cookie: sid=BCEA0A1BA4249DF33EC03E7A0269DE101FC829462BD8EE8CEBDDF0C46FC9AFFDBD6C1F56D896E3296DEDD9ECE4CFB1E633EBD9D850D7A68126D9B7264653E8CC282DD2CAB5D364513DCF3E71D4481FF3E5D937BFE72E31A1EBECC6733E798322E34B4838A4265E34D3449D6EEFB52C7C24EED7E6484621603BC2F488B1E8C7AC', 'Accept-Language: en-US,en;q=0.8', 'User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/32.0.1700.77 Safari/537.36')
rg <- getOrgCollnHash(da.DT$id, csv_header) 
rg


## ----checkCSVParams------------------------------------------------------
getFormParams('http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization=c2272&scope=e5.g3.y10&scores=N_MA,MA_pass,MA_AMO,MA_level,N_RD,RD_pass,RD_AMO,RD_level')


## ----def-getDiCSV--------------------------------------------------------
getDiCSV <- function(org, scope, exam, header, 
                     ethnicity = FALSE, gender = FALSE,
                     iep = FALSE, frl = FALSE,
                     filterdata = NULL, rel = "and") {
  # report parameter
  org <- getOrgCollnHash(org, header)
  # scores parameter
  score <- list(e1 = 'N_MA,MA_SS,MA_pass,MA_AMO,MA_level,N_RD,RD_SS,RD_pass,RD_AMO,RD_level',
                e2 = 'N_MA,MA_SS,MA_pass,MA_AMO,MA_level,N_RD,RD_SS,RD_pass,RD_AMO,RD_level,N_SC,SC_SS,SC_pass,SC_AMO,SC_level,N_WR,WR_pass,WR_AMO,WR_level',
                e5 = "N_MA,MA_pass,MA_AMO,MA_level,N_RD,RD_pass,RD_AMO,RD_level",
                e6 = "N_WR,WR_pass,WR_AMO,WR_level")
  target <- paste0('http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/summaryCSV?report=summary_1&organization=', org, '&scope=', scope, '&scores=', score[[exam]])
  
  # inelegant disaggregation selection
  subgroup <- "" 
  if (ethnicity == TRUE) subgroup <- paste0("ethnicity,", subgroup)
  if (gender == TRUE) subgroup <- paste0("gender,", subgroup)
  if (iep == TRUE) subgroup <- paste0("iep,", subgroup)
  if (frl == TRUE) subgroup <- paste0("frl,", subgroup)
  
  # if any of the above, concatenate with target
  if (subgroup != "") {
      target <- paste0(target, "&subgroups=", sub(",$", "", subgroup), "&filterrelation=and")
  }

  # filtering on subpopulation, but by passing a string argument instead
  # if present, subgroups above are ignored
  if (is.character(filterdata)) {

      # convert
#.      getFilterKeys <- function(filterdatum) {
#.          # split and combine
#.          filterkey <- strsplit(filterdatum, split = '.')
#.          paste(filterkey, collapse = '_')
#.      }
      filterkeys <- gsub(".", "_", filterdata, exact = TRUE)

      target <- paste0(target, "&filterkey=", filterkeys, "&filterdata=",
                       filterdata, "&filterrelation=", rel)
  }
  # target 
  # make it so
  resultsText <- getURL(target)
    
  # create temporary connection from results and return data.table
  data.table(read.csv(text = resultsText))
  }


## ----getDiCSV------------------------------------------------------------
scopestr <- paste(c('e1', 'g3', paste0('y', 3:10)), collapse = '.')
da.crt.g3 <- getDiCSV(da.DT$id, scopestr, 'e1', csv_header)


## ----printDa, results='asis', echo=FALSE---------------------------------
kable(head(da.crt.g3), "html",table.attr = "id=\"da_table\"")


## ----getDiCSV-ethnicity--------------------------------------------------
da.crt.g3.e <- getDiCSV(da.DT$id, scopestr, 'e1', csv_header, ethnicity = TRUE)


## ----print-getDiCSV-ethnicity, echo=FALSE, results='asis'----------------
kable(head(da.crt.g3.e), "html")


## ----ayp-----------------------------------------------------------------
all.ccsd.ayp.csv.txt <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5376&scope=e13.y1.y2.y3.y4.y5.y6.y7.y8.y9&scores=852,853,854")
ccsd.ayp.DT <- data.table(read.csv(text = all.ccsd.ayp.csv.txt))


## ----demographic---------------------------------------------------------
all.ccsd.demo.csv.txt <- getURL('http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e7.y1.y10.y2.y4.y5.y6.y7.y8.y9&scores=1026,566,567,568,569,570,571,572,573,574,575,805,576,577,806,586,587,588,589,578,579,580,581,582,583,584,585')
ccsd.demo.DT <- data.table(read.csv(text = all.ccsd.demo.csv.txt))


## ----personnel-----------------------------------------------------------
all.ccsd.pers.csv.txt <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e12.y1.y10.y2.y4.y5.y6.y7.y8.y9&scores=779,780,781,782,851,783,784,785,786,787,788,789,790,791,792,793,795,796,1029,797,798,799,800,801,802,803,760,761,762,856,763,765,767,769,771,764,766,768,770,772,775,773,777,776,774,778")
ccsd.pers.DT <- data.table(read.csv(text = all.ccsd.pers.csv.txt))


## ----tech----------------------------------------------------------------
ccsd.tech2011.csv <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e17.y1.y2.y3.y4.y5.y6&scores=590,591,592,593,594,595")
ccsd.tech2012.csv <- getURL("http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/rosterCSV?report=reportcard_1&organization=c5195&scope=e8.y10.y7&scores=809,810,811,812,813,814,815")
ccsd.tech.DT <- data.table(rbind.fill(read.csv(text = ccsd.tech2011.csv), read.csv(text = ccsd.tech2012.csv)))


## ----def-getProfiles-----------------------------------------------------

getProfiles <- function(org) {
  years <- paste0('y', 1:10)
  
  # Create a list of lists containing searches for each year
  searches <- lapply(years, function(x) structure(c("profile_1", "profile", x, org), 
                                                  .Names = c("report",                                                                                                                                                     
                                                             "reportID", 
                                                             "scope",
                                                             "organization")
                                                  )
                     )
  prof.json.lst <- lapply(searches, function(search) {
    getForm('http://www.nevadareportcard.com/DIWAPI-NVReportCard/api/Profile', 
            .params = search, 
            .opts = (httpheader = 'Accept: application/json, text/javascript, */*; q=0.01')
            )
    }
    )
  prof.list <- lapply(prof.json.lst, fromJSON)
  names(prof.list) <- years
  prof.list
}


## ----getProfiles---------------------------------------------------------
valley.prof.list <- getProfiles(da.DT["Valley HS"]$id)


## ----pricipaltable-------------------------------------------------------
 # Finding principals
table(sapply(valley.prof.list, function(x) x$superintendent))


## ----toyExample-geocoding------------------------------------------------
 # Geocoding
valley.address <- paste(valley.prof.list$y1$address, valley.prof.list$y1$zip)
valley.address
valley.latlong <- geocode(valley.address)



## ----toyExample-osm------------------------------------------------------
sta <- get_map(location = c(lon = valley.latlong$lon, lat = valley.latlong$lat),
               zoom = 15, 
               crop = TRUE,
               source = 'stamen'
               )
ggmap(sta)


## ----toyExample-crime----------------------------------------------------

crimes <- getURL("http://www.crimemapping.com/GetIncidents.aspx?db=8/28/2013+00:00:00&de=2/03/2014+23:59:00&ccs=AR,AS,BU,DP,DR,DU,FR,HO,VT,RO,SX,TH,VA,VB,WE&add=2839%20Burnham%20Ave%2C%20Las%20Vegas%2C%20Nevada%2C%2089169&bcy=4319584.042143912&bcx=-12815448.676366305&br=1.0&xmin=-12818038.894702934&ymin=4318442.044425546&xmax=-12813524.33271972&ymax=4321389.647328871")

crimes <- fromJSON(crimes)

table(sapply(crimes$incidents, function (x) x$CrimeCode))


## ------------------------------------------------------------------------
dsk <- getURL('http://www.datasciencetoolkit.org/coordinates2statistics/36.13727%2c-115.1217')
dsk <- fromJSON(dsk)


## ------------------------------------------------------------------------
dsk[[1]]$statistics$us_population_poverty
dsk[[1]]$statistics$us_population_low_income


## ------------------------------------------------------------------------
dsk[[1]]$statistics$us_households_single_mothers
dsk[[1]]$statistics$us_sample_area


## ----housekeeping--------------------------------------------------------
save.image("nv-report-card-data.RData")

sessionInfo()


