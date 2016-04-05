##The 2016 Texas Primary

This repo is dedicated toward examining the data from the 2016 Texas Presidential Primary. Most of the code in `TexasPrimary2016.R` comes from [this post on R-Bloggers](http://www.r-bloggers.com/mapping-election-results-with-r-and-choroplethr/) (by way of [R. Duncan McIntosh](http://rduncanmcintosh.com/) and his [post on Ari Lamstein's blog](http://www.arilamstein.com/blog/2016/03/21/mapping-election-results-r-choroplethr/))  
You will need to run the `TexasPrimary2016.R` script to run the examples in `examples.R`.

###Getting the Election Data

#####Data scraping files
* `TexasPrimary2016.R` = will scrape data from the Texas secretary of state's election data website

#####Data exploration files
* `examples.R` = will create maps and knitr tables (depending on which examples you choose)
  * Dependencies
    * `TexasPrimary2016.R`

###Getting the Economic Data
I am in the early stages  of adding some county-level economic data from FRED (Federal Reserve's Economic Data).  
I have a few functions that will scrape data from FRED (YOU WILL NEED AN API KEY FROM FRED).  
I have another R script which will demonstrate how to scrape massive amounts of data from FRED

#####FRED functions
`series.scraper()` = this function will scrape metadata about a particular 'release', including the series within that 'release' as defined by the 'filters'
  * Dependencies
    * `county.scraper()` = used to create a searchable column of counties in the master/top-level table
    * `category.scraper()` = used to create a searchable column of categories within each release
`obs.scraper()` = this function will scrape the actual data from a given series

#####FRED examples
`FRED.example1.R` = this script will scrape quite a bit of data from several different releases using two common filters ('tx', 'county')  
`FRED.example2.R` = this script will tidy up the `fred.obs1` and `fred.obs2` data. Right now, there are thousands of data frames attached to these two lists - the goal of this example is to condense each `Category` into a table itself.

#####R Objects
I have saved much of the data as R objects (.RData) which can be loaded with `load("Data/robject_name.RData")` or whichever filepath you need to use.  
I am working on creating an object-relationalish database model to store all the data that will eventually be saved as R objects.  
I highly recommend SAVING YOUR DATA after you have scraped the data. Hint: I saved mine as R objects by using `save(some.object, file = "some.file.RData")`.  