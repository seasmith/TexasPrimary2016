##The 2016 Texas Primary

This repo is dedicated toward examining the data from the 2016 Texas Presidential Primary. Most of the code in `TexasPrimary2016.R` comes from [this post on R-Bloggers](http://www.r-bloggers.com/mapping-election-results-with-r-and-choroplethr/) (by way of [R. Duncan McIntosh](http://rduncanmcintosh.com/) and his [post on Ari Lamstein's blog](http://www.arilamstein.com/blog/2016/03/21/mapping-election-results-r-choroplethr/))  
You will need to run the `TexasPrimary2016.R` script to run the examples in `examples.R`.

###Getting the Election Data
Data scraping files
* `TexasPrimary2016.R` = will scrape data from the Texas secretary of state's election data website

Data exploration files
* `examples.R` = will create maps and knitr tables (depending on which examples you choose)
  * Dependencies
    * `TexasPrimary2016.R`

###Getting the Economic Data
I am in the process of adding some economic data from FRED (Federal Reserve's Economic Data).  
I am in the early stages of adding the data. The data will be county level data.    
So far I have two functions that will scrape data from FRED (YOU WILL NEED AN API KEY FROM FRED).  
I have another R script which will demonstrate how to scrape massive amounts of data from FRED

`fred.series()` = this function will scrape metadata about a particular 'release', including the series within that 'release' as defined by the 'filters'  
`fred.data()` = this function will scrape the actual data from a given series
`FRED.data.example.R` = this script will scrape quite a bit of data from several different releases using two common filters ('tx', 'county')  

I recommend SAVING YOUR DATA after you have scraped the data. Hint: I saved mine to R objects using save("somefile.RData").