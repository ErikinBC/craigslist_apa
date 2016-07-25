###### THIS CODE SCRAPES THE CRAIGSLIST APARTMENT DATA ########

# Define the directory
dir <- "/Users/..."
setwd(dir)

# Libraries
ll <- c('magrittr','rvest','Hmisc','dplyr','tidyr','stringr','ggmap','ggplot2','gridExtra','gtable')
sapply(ll,function(qq) library(qq,character.only = T))

# Get the current date: year and month day (to match craigslist)
Y <- Sys.time() %>% format('%Y')
bd <- Sys.time() %>% format('%b %d')

# Set the city
city <- 'vancouver'

# Define the urls
url.seq <- paste('https://',city,'.craigslist.ca/search/apa?s=',seq(0,2400,100),sep='')

# This function reads in the Craigslist data for a given page
cl.fun <- function(url,Y,city) {
  # url=url.seq[1]; Y=Y
  # Call in the html page0
  cl.url <- read_html(url)
  # Get the numeric price data
  cl.price <- 
    paste('.row:nth-child(',1:100,') .price',sep='') %>%
    sapply(.,function(qq) html_nodes(x=cl.url,css=qq) %>% html_text(trim=T),USE.NAMES=F) %>%
    lapply(.,function(qq) ifelse(length(qq)==0,NA,qq)) %>% unlist %>% 
    gsub(pattern='\\$',replacement='') %>% as.numeric
  # Get the number of bedrooms and square feet
  cl.br.seq <- 
    paste('.row:nth-child(',1:100,') .housing',sep='') %>% 
    sapply(.,function(qq) html_nodes(x=cl.url,css=qq) %>% html_text(trim=T),USE.NAMES=F) %>%
    lapply(.,function(qq) ifelse(length(qq)==0,NA,qq)) %>% unlist
  # Now pull out the number of bedrooms
  cl.br <- cl.br.seq %>% substring(3,3) %>% as.numeric
  # Find which data have the square feet
  cl.ft2 <- 
    cl.br.seq %>% strsplit(' - ') %>% sapply(.,'[',2) %>% gsub(pattern='ft2 -',replacement='') %>% as.numeric
  # Get the date (no need for row-elements because it's pasted by craigslist)
  cl.date <- cl.url %>% html_nodes('time') %>% html_text(trim = T) %>% paste(Y,.,sep=' ') %>% as.Date(x=.,format='%Y %b %d')
  # Get the city data
  cl.city.str <- 
    paste('.row:nth-child(',1:100,') .pnr',sep='') %>%
    sapply(.,function(qq) html_nodes(x=cl.url,css=qq) %>% html_text(trim=T),USE.NAMES=F) %>%
    lapply(.,function(qq) ifelse(length(qq)==0,NA,qq)) %>% unlist
  # Get the data between the paranthesis
  cl.city <- cl.city.str %>% strsplit('\\(|\\)') %>% sapply(.,'[',2)
  # Get the character name of the city
  cl.name <- 
    sapply(paste(cl.city,'BC, Canada',sep=', '),function(qq) geocode(location=qq,output=c('more')),USE.NAMES=F)
  # Extract the city locality
  cl.local <- rep(NA,100)
  # Get the index that has the locality in it
  idx.local <- lapply(cl.name,function(qq) 'locality' %in% colnames(qq)) %>% unlist %>% which
  # And fill
  cl.local[idx.local] <- lapply(cl.name[idx.local],function(qq) qq$locality) %>% unlist %>% as.character
  # Make the data.frame 
  cl.df <- data.frame(Price=cl.price,Bedroom=cl.br,Feet=cl.ft2,City=cl.local,
                      Date=cl.date,Craigslist=rep(city,length(cl.date)))
  # Return
  return(cl.df)
}

# Run function for list
cl.list <- lapply(url.seq,cl.fun,Y=Y,city=city)

# Define the ctiy/date search
city.date <- paste(city,gsub(' ','_',paste(bd,Y,sep=' ')),sep='_')

# See if the search already exists
if (length(grep(city.date,list.files(paste(dir,'rt',sep='/'))))==0) {
  save(cl.list,file=paste(dir,'rt',paste(city.date,'RData',sep='.'),sep='/'))
} else {
  print("We've already created this file for this city!")
}


##### ---- RUN ACHIVED FROM HERE ---- #####

# Next, we need to find files that have our city in it
city.files <- grep(city,list.files(paste(dir,'rt',sep='/')),value = T)

rm(cl.list)
master.list <- list()
for (cc in city.files) {
  # Load in the city list files
  load(paste(dir,'rt',cc,sep='/'))
  # Store in master list
  master.list <- c(master.list,cl.list)
}


# Put into a data.frame
cl.df <- do.call('rbind',master.list)
