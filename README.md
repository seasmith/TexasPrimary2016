# The 2016 Texas Primary

> Examination of the 2016 Texas Presidential Primary.

## Presentations
Check out the posts below if you want to see what I have been doing with the primary data. I have a couple posts on my [GitHub page](https://seasmith.github.io) as well as my [other site](https://protocolvital.info).

#### [TexasPrimary2016: Z-Scores](https://seasmith.github.io/posts/texasprimary2016_update_with_zscores.html)
12-12-2016  
Adding a new stat: Z-Scores.

<img src="https://seasmith.github.io/posts/texasprimary2016_update_with_zscores_files/figure-html/marco-1.png" width="600">

#### [TexasPrimary2016: Update](https://seasmith.github.io/posts/texasprimary2016_update.html)
11-21-2016  
Refocusing on this once forgotten project on the 2016 Texas Presidential Primary.

<img src="https://seasmith.github.io/posts/reference/candidate_winner.png", width="600">

#### [R, FRED, and the 2016 Texas Primary: Part 3.1](https://protocolvital.info/2016/05/12/r-fred-and-the-2016-texas-primary-part-3-1/)
05-12-2016

<img src="https://protocolvitaldotinfo.files.wordpress.com/2016/05/unrate-pcpi-partywinner.png", width="600">

#### [R, FRED, and the 2016 Texas Primary: Part 2](https://protocolvital.info/2016/04/26/r-fred-and-the-2016-texas-primary-part-2/)
04-26-2016

#### [R, FRED, and the 2016 Texas Primary: Part 1](https://protocolvital.info/2016/04/05/r-fred-and-the-2016-texas-primary-part-1/)
04-05-2016

___

__Update in progress: not all of the information in the sections below may be accurate__

### Election Data

* **Where are the data sources?**  
    [Texas SOS Election Results](http://www.sos.state.tx.us/elections/historical/)  
    [Texas SOS ELECTION HISTORY](http://elections.sos.state.tx.us/index.htm)  
    [Open Data Stackexchange: Where to find Texas election data](http://opendata.stackexchange.com/questions/6583/where-can-i-find-data-on-the-winner-of-the-presidential-popular-vote-by-u-s-cou/6587)  

* **Data scraping files** | `"~/R/TexasPrimary2016/Functions/..."`  
    `scraper.functions.R` = functions to scrape raw data from the web
    `cat.functions.R` = functions to group similar data together
    `agg.functions.R` = functions to aggregate FRED data
    

* **Data exploration files** | `"~/R/TexasPrimary2016/Examples/..."`  
    `TexasPrimary2016.R` = scrapes county-level election results from the Texas secretary of state's website  
    `examples.R` = creates maps and knitr tables with data from `TexasPrimary2016.R`   
    `FRED.example1.R` = scrapes Texas county level data from FRED  
    `FRED.example2.R` = tidies up the data from `FRED.example1.R`
    `FRED.example3.R` = aggregates the FRED data for some exploratory analysis

* **Where are the data itself?** | `"~/R/TexasPrimary/Data/..."`  
    `"~/R/TexasPrimary/Data/Election"` = all election data I have scraped is stored in this folder  
    `"~/R/TexasPrimary/Data/FRED"` = all FRED (economic and demographic) data I have scraped is stored in this folder

___

### FRED functions
* `series.scraper()` = this function will scrape metadata about a particular 'release', including the series within that 'release' as defined by the 'filters'
  * Script dependencies
    * `cat.county.scraper()` = uses regular expressions to extract category and county information (replaces earlier functions)

* `obs.scraper()` = this function will scrape the actual data from a given series  
* `cat.functions.R` = functions in this script will be used in `FRED.example2.R` to tidy up those data frames into more manageable data frames. I will clarify their descriptions after my next post.  

___

### Other data yet to come
* `[FEC ftp server](ftp://ftp.fec.gov/FEC/)` = Federal Election Commission data (campaign finance)

___
### Misc
This repo is dedicated toward examining the data from the 2016 Texas Presidential Primary. The inspiration for this project comes from [this post on R-Bloggers](http://www.r-bloggers.com/mapping-election-results-with-r-and-choroplethr/) (by way of [R. Duncan McIntosh](http://rduncanmcintosh.com/) and his [post on Ari Lamstein's blog](http://www.arilamstein.com/blog/2016/03/21/mapping-election-results-r-choroplethr/))  

