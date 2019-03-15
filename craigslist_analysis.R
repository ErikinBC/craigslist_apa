# Load in the libraries we'll need
rm(list=ls())

# Define the directory
dir <- ""
setwd(dir)

# Libraries
ll <- c('magrittr','dplyr','tidyr','ggplot2','gridExtra','GGally','stringr','gtools','corrplot','geosphere',
        'ggmap','maptools','gpclib','rgeos','sp','rgdal','broom','reshape2','lmtest','car','stargazer')
sapply(ll,function(qq) library(qq,character.only = T))

gpclibPermit()

###################################################
### ---------- DATA ANALYSIS SECTION ---------- ###
###################################################

load('rt_data.RData')
source('craigslist_analysis_functions.R')

# Recode the lat/lon if it is equal to one
rt.tbl <- rt.tbl %>% mutate(lat=replace(lat,lat==1,NA),lon=replace(lon,lon==1,NA))
# rt.tbl %>% group_by(city) %>% summarise(act.lat=sum(!is.na(lat)),na.lat=sum(is.na(lat)))


#########################################
### ----- STEP 1: CLEAN UP DATA ----- ###
#########################################

tv <- c('Toronto','Vancouver')

# Subset Vancouver and Toronto
vt.dat <- rt.tbl %>% filter(city %in% tolower(tv)) %>%
              mutate(city=factor(city,labels=tv))
# Scrub upper and lower price/ft2 data
vt.dat <- vt.dat %>% 
            filter(price<quantile(price,0.975,na.rm=T) &
                   price>quantile(price,0.025,na.rm=T) &
                   ft2<quantile(ft2,0.975,na.rm=T) &
                   ft2>quantile(ft2,0.025,na.rm=T) &
                   !is.na(price) & 
                   !is.na(ft2))

# Select the columns we can about
cc.select <- c('date','locality','pc','city','price','br','ba','ft2','lat','lon',
               'no.features','no_smoking','house','apartment','townhouse','condo','duplex','loft',
              'w.d_in_unit','w.d_hookups','off.street_parking','street_parking','cats_are_OK_._purrr','dogs_are_OK_._wooof',
              'wheelchair_accessible','laundry_on_site','no_laundry_on_site','laundry_in_bldg',
              'attached_garage','carport','furnished')
cc.rename <- c('date','locality','postalcode','city','price','br','ba','ft2','lat','lon',
               'no.features','no.smoking','is.house','is.apartment','is.townhouse','is.condo','is.duplex','is.loft',
               'has.washer.dryer1','has.washer.dryer2','has.parking.inside','has.parking.outside','allows.cats','allows.dogs',
               'has.wheelchair','has.laundry1','has.no.laundry','has.laundry2',
               'has.garage','has.carport','is.furnished')

vt.dat <- vt.dat[,cc.select]
names(vt.dat) <- cc.rename

# Aggregate the columns where necessary
vt.dat <- vt.dat %>% mutate(has.washer.dryer=or(has.washer.dryer1,has.washer.dryer2),
                  has.laundry=or(or(has.laundry1,has.laundry2),!has.no.laundry)) %>%
            select(-c(has.washer.dryer1,has.washer.dryer2,has.laundry1,has.laundry2,has.no.laundry))
# Drop any duplicated obsercations
vt.dat <- vt.dat[-which(duplicated(select(vt.dat,-date))),]

# Add any other variables
vt.dat <- vt.dat %>% mutate(p2ft=price/ft2)


#########################################
### ----- STEP 2A: PRICE to FT2 ----- ###
#########################################

# Use binhexing!
gg.vt.binhex <- 
ggplot(data=vt.dat,aes(x=ft2,y=price)) +
  stat_binhex(color='white') + 
  scale_fill_gradientn(colors=c('white','blue'),name='Frequency') + 
  facet_wrap(~city) + 
  xlab('Square feet') + 
  ylab('Monthly rental price')
# gg.vt.binhex


#####################################################
### ----- STEP 2B: MEAN/MEDAIN OF PRICE/FT2 ----- ###
#####################################################

# Get mean and SE
vt.mean.se <- 
vt.dat %>% select(city,price,ft2,p2ft) %>% gather(var,val,-city) %>% 
  group_by(city,var) %>% 
  summarise(mu=mean(val),l=mean(val)-sd(val),h=mean(val)+sd(val),count=length(val)) %>%
  mutate(stat='Mean and 1-SD')

# Get median and IQR
vt.med.iqr <- 
  vt.dat %>% select(city,price,ft2,p2ft) %>% gather(var,val,-city) %>% 
  group_by(city,var) %>% 
  summarise(mu=median(val),l=quantile(val,0.25),h=quantile(val,0.75),count=length(val)) %>%
  mutate(stat='Median and IQR')

# Combine
vt.mean.med <- rbind(vt.mean.se,vt.med.iqr) %>% mutate(var=factor(var,labels=c('Square Feet','Price to Ft2','Price')))

# Make the graph showing the range of prices/square feet
gg.vt.price.ft <- 
ggplot(vt.mean.med,aes(x=city,y=mu,color=city)) +
  geom_point(size=5,position=position_dodge(0.9)) + 
  geom_errorbar(size=2,aes(ymin=l,ymax=h),position=position_dodge(0.9)) +
  facet_grid(var~stat,scales='free') + 
  xlab('') + ylab('') + 
  theme(legend.position='bottom',text=element_text(size=15)) + 
  scale_color_discrete(name='City: ')
# ggsave('price_feet.png',gg.vt.price.ft)

#####################################################
### ----- STEP 2C: CORRELATIONS OF FEATURES ----- ###
#####################################################

# Get Correlation coefficient for each city and melt the data for ggplotting in geom_tile() format
tv.cor <- 
  lapply(tv,function(cc)
  cor(vt.dat %>% select(city,price,br,ba,ft2,no.features:has.laundry) %>% filter(city==cc) %>% select(-c(city,no.features)),
      use='pairwise.complete.obs') %>%
    data.frame %>% mutate(var2=rownames(.)) %>% gather(var1,val,-var2))
# Tidy up the text formatting
tv.cor <- lapply(tv.cor,function(df) df %>% transmute(var1=gsub('is\\.|has\\.','',var1),
                                            var2=gsub('is\\.|has\\.','',var2),
                                            val=val) %>%
                                  mutate(var1=gsub('\\.',' ',var1) %>% sapply(simpleCap),
                                         var2=gsub('\\.',' ',var2) %>% sapply(simpleCap) ))
names(tv.cor) <- tv
# Merge
tv.cor <- do.call('rbind',lapply(tv,function(cc) tv.cor[[cc]] %>% mutate(city=cc) %>% tbl_df))
# Get the factor version
tv.cor.factor <- tv.cor %>% mutate(valf=cut(val,breaks=c(-1,-0.2,0.2,1)) %>% 
                                     factor(labels=c('[-1,-0.2]','[-0.2,0.2]','[0.5,1]')))

gg.cor.list <- 
  lapply(tv,function(cc) 
  ggplot(tv.cor.factor %>% filter(city==cc),aes(x=var1,y=var2)) + 
  geom_tile(aes(fill=valf),color='black',show.legend = T) + 
  scale_fill_manual(name='Corr: ',values=c('skyblue','gray95','coral1')) + 
  theme(legend.position='bottom',axis.text.x=element_text(angle=90)) + 
  xlab('') + ylab('') + facet_wrap(~city,ncol=1)  )
  
gg.vt.cor.lgl <- grid.arrange(gg.cor.list[[1]],gg.cor.list[[2]],nrow=1,ncol=2)
# plot(gg.vt.cor.lgl)

# ggcorr(data.frame(t.cor) %>% select(-c(No.Features,Duplex,Loft,Townhouse,Parking.Inside)),
#        nbreaks=5,label=F,vjust=0.5,hjust=0.5,angle=0,color='black',legend.position='right',size=4)

##############################################
### ----- STEP 2D: SHARE OF FEATURES ----- ###
##############################################

# --- Calculate the conditional means based on the different factors --- # 

vt.gather <- 
vt.dat %>% 
  select(city,price,no.smoking:has.laundry) %>% 
  set_colnames(gsub('is\\.|has\\.','',names(.)) %>% gsub('\\.',' ',.) %>% sapply(simpleCap)) %>% 
  gather(var,val,-Price,-City) 

vt.share <-
vt.gather %>% group_by(City,var) %>%
  summarise(yes=sum(val,na.rm=T),no=length(val)-sum(val,na.rm=T),total=length(val)) %>% 
  mutate(share=yes/total)

gg.vt.share <- 
ggplot(vt.share,aes(x=var,y=share*100,fill=City)) + 
  geom_bar(stat='identity',position=position_dodge(),color='black') + 
  xlab('') + ylab('Share of listings with features (%)') + 
  theme(axis.text.x=element_text(angle=90),text=element_text(size=15),legend.position='bottom') + 
  scale_color_discrete(name='City: ')
# plot(gg.vt.share)

###################################################
### ----- STEP 2E: MEAN PRICE BY FEATURES ----- ###
###################################################

vt.features.mean <- 
vt.gather %>% group_by(City,var,val) %>%
  summarise(mu=mean(Price),se=sd(Price)) %>% 
  mutate(feature=ifelse(val,'Yes','No')) %>% mutate(feature=factor(feature,levels=c('Yes','No')))

gg.vt.features.mean <- 
ggplot(vt.features.mean,aes(x=var,y=mu,fill=feature)) + 
  geom_bar(stat='identity',color='black',position=position_dodge()) + 
  facet_wrap(~City,nrow=2) + 
  ylab('Average monthly rental price') + xlab('') + 
  theme(axis.text.x=element_text(angle=270),legend.position='bottom',text=element_text(size=15)) + 
  scale_fill_discrete(name='Has Feature: ')
# gg.vt.features.mean

#####################################################
### ----- STEP 2F: MEAN PRICE BY NO. BR/BAs ----- ###
#####################################################

# Create a data set for the different bedrooms and bathrooms
vt.ba.br <- 
vt.dat %>% select(city,price,br,ba) %>% 
  gather(var,val,-price,-city) %>% filter(!is.na(val)) %>% 
  mutate(count=factor(ifelse(val>3,'4+',val)))  %>% filter(count!=0) %>%
  group_by(city,var,count) %>% 
  summarise(mu=mean(price),n=length(price)) %>% data.frame %>% 
  mutate(var=factor(var,labels=c('Bathrooms','Bedrooms'))) %>% tbl_df

gg.vt.ba.br <-
ggplot(vt.ba.br,aes(x=count,y=mu,fill=city)) + 
  geom_bar(stat='identity',color='black',position=position_dodge()) + 
  xlab('Number of Bathrooms/Bedrooms') + ylab('Average monthly rental price') + 
  facet_wrap(~var,nrow=1) + scale_fill_discrete(name='City: ') + 
  theme(legend.position='bottom',text=element_text(size=15))
# gg.ba.br

#########################################################
### ----- STEP 2G: AVERAGE DISTANCE TO DOWNTOWN ----- ###
#########################################################

# Define downtown for each city #

t.dt <- c(-79.38899,43.64273) # CN Tower GPS to 5 deciments in lon/lat format
v.dt <- c(-123.12577,49.28574) # Shangri-la GPS to 5 decimals in lon/lat format

# Get the specific data
vt.lat.lon <- vt.dat %>% select(city,price,ft2,lat,lon) %>% 
                  filter(!is.na(lat) & !is.na(lon))
# # Check robustness over plus-minus three degrees by city
# ggplot(vt.lat.lon %>% group_by(city) %>% mutate(lat=lat-median(lat,na.rm=T),lon=lon-median(lon,na.rm=T)) %>% select(city,lat,lon) %>% gather(var,val,-city),
#        aes(x=val)) + geom_histogram() + facet_grid(city~var) + xlim(c(-1,1))
vt.lat.lon <- vt.lat.lon %>% group_by(city) %>% filter(abs(lat-median(lat,na.rm=T))<=1,abs(lon-median(lon,na.rm=T))<=1) 

# Calculate the t.dist/v.dist
vt.dist <-  vt.lat.lon[,c('lon','lat')] %>% apply(1,function(rr) 
                c(gcd.hf(rr[1],rr[2],t.dt[1],t.dt[2]),
                    gcd.hf(rr[1],rr[2],v.dt[1],v.dt[2])))

vt.lat.lon <- 
vt.lat.lon %>% rowwise() %>% mutate(tdist=gcd.hf(lon,lat,t.dt[1],t.dt[2]),
                                            vdist=gcd.hf(lon,lat,v.dt[1],v.dt[2])) %>%
                            mutate(dist=min(tdist,vdist)/1000) %>% select(-c(tdist,vdist)) %>%
                            data.frame %>% tbl_df
# Make ggplot of distance
gg.vt.dist <- 
ggplot(vt.lat.lon %>% select(city,dist),aes(x=dist,fill=city)) + 
  geom_histogram(binwidth=1,color='black') + xlab('Distance to downtown (km)') + ylab('Number of listings') + 
  facet_wrap(~city,scales='free_y',nrow=1) + theme(legend.position='bottom') + scale_fill_discrete(name='City: ') + 
  theme(text=element_text(size=15))
# plot(gg.dist)

############################################################
### ----- STEP 2H: MEAN PRICE BY DOWNTOWN DISTANCE ----- ###
############################################################

vt.dist.price <- 
vt.lat.lon %>% data.frame %>% 
  mutate(range=cut(dist,breaks=c(0,1,5,10,25,100),right=F,labels=c('0<km<1','1<km<5','5<km<10','10<km<25','km>25'))) %>%
  group_by(city,range) %>% summarise(mu.price=mean(price),se.price=sd(price),mu.p2f=mean(price/ft2),se.p2f=sd(price/ft2))

# Put into tabular
vt.dist.price.tab <- 
vt.dist.price %>% gather(type,val,-city,-range,-se.price,-se.p2f) %>%
  mutate(type=gsub('mu.','',type)) %>% mutate(se=ifelse(type=='price',se.price,se.p2f)) %>% 
  select(-contains('se.')) %>% mutate(type=factor(type,labels=c('Price to Ft2','Price'))) %>% data.frame

gg.vt.mean.price.dist <-
ggplot(vt.dist.price.tab,aes(x=city,y=val,color=city)) + 
  geom_point(size=5,position=position_dodge(0.9)) + 
  geom_errorbar(size=2,aes(ymin=val-se,ymax=val+se),position=position_dodge(0.9)) +
  xlab('Distance to dowtown core (km)') + ylab('Mean and standard deviation') + 
  theme(legend.position='bottom',text=element_text(size=15),axis.text.x=element_text(color='white'),
        axis.ticks.x=element_blank()) + 
  scale_color_discrete(name='City: ') + 
  facet_grid(type~range,scales='free')
# plot(gg.vt.mean.price.dist)

####################################################
### ----- STEP 3: CHOROPLETS VISUALIZATION ----- ###
####################################################

source('geo_script.R')

# Show the relationship between average neighbourhood price and price to square feet
gg.vt.neighbourhood.scatter <- 
ggplot(rbind(tor.id.mean %>% mutate(city='Toronto'),van.id.mean %>% mutate(city='Vancouver')),
       aes(x=p2f,y=mu,color=city)) + 
  geom_point(size=3) + 
  xlab('Neighbourhood price to square feet average') + 
  ylab('Neighbourhood price average') + 
  theme(legend.position='bottom',text=element_text(size=15)) + 
  scale_color_discrete(name='City: ') + 
  geom_smooth(method = 'lm',se=F)

# Show the super interesting ecological fallacy!
vt.eco.fallacy <- rbind(select(combine.van,city,price,ft2,id),select(combine.tor,city,price,ft2,id)) %>% 
  mutate(p2ft=price/ft2) %>% group_by(city,id) %>% summarise(cc=cor(price,p2ft)) %>% filter(!is.na(cc))

gg.vt.eco <- 
ggplot(vt.eco.fallacy,aes(x=city,y=cc,color=city)) + 
  geom_point() +  xlab('') + ylab('Correlation with price and price to Ft2 within each neighbourhood') + 
  coord_flip() + geom_jitter(width=1,height=0) + 
  scale_color_discrete(name='City: ') + 
  theme(legend.position='none',text=element_text(size=15))

####################################################
### ----- STEP 4.A: OLS MODELLING - SET UP ----- ###
####################################################

# Get the distance variable from the other dataframe
distvar <- vt.lat.lon$dist

# Modify the vaiables as neccesary
vt.ols.data <-
  vt.dat %>% 
  filter(!is.na(lat) & !is.na(lon)) %>% # Drop any missing lat/lon
  group_by(city) %>% filter(abs(lat-median(lat,na.rm=T))<=1,abs(lon-median(lon,na.rm=T))<=1) %>% # Drop anything with wrong lat-lon
  as_data_frame %>% mutate(distance=distvar) %>% # This adds the term we have already calculated
  filter(!is.na(br)) %>% # Drop any observations that don't have bedroom count
  filter(is.house+is.apartment+is.townhouse+is.condo+is.duplex+is.loft==1) %>% # Drop any observations that are not a specific class of hum
  mutate(trend=factor(date,labels=seq(1,length(unique(date)))) %>% as.numeric, # Create a time series trend
         postalcode=substr(postalcode,1,3), # Take first three letters of postal code
         intercept=1) %>% # Make an intercept term
  mutate(postalcode=ifelse(grepl('^V|^M|^L',postalcode),postalcode,NA) %>% as.factor, # Keep only BC/Toronto Postal codes
         trend=(trend-min(trend)+1)/(365.25/12), # Re-center to 1, and then make it per month
         ft2=ft2/100, # Modify the square feet measure so that coefficient is "larger"
         distance=distance/10, # Modify the distance so that coefficient is "larger"
         is.apartment.condo=ifelse(or(is.apartment,is.condo),T,F), # Aggregate to match ft2 average
         is.townhouse.duplex=ifelse(or(is.townhouse,is.duplex),T,F)) %>% 
  select(date,city,postalcode,price,intercept,trend,ft2,br,distance,no.smoking,
         is.townhouse.duplex,is.house,
         is.furnished,allows.cats,allows.dogs,has.wheelchair,
         has.parking.inside,has.parking.outside,has.garage,has.carport,
         has.washer.dryer,has.laundry) %>% 
  as_data_frame()

##############################################
### ----- STEP 4.B: NAIVE REGRESSION ----- ###
##############################################

# vt.ols.data %>% glimpse()

# Run OLS the naive OLS regression (which we know has heteroskedastic errors)
vt.ols.naive  <- lm(price~.-1,data=select(vt.ols.data,price:has.laundry))
# View regression
vt.ols.naive %>% tidy

# Run the Breusch pagan test
erik.bp <- function(lmo) { return(as.numeric(bptest(lmo)$p.value)) }
erik.bp(vt.ols.naive) # Definitely reject the null hypothesis!

# Extract the fitted and residuals for the different cities:
vt.fitted <- vt.ols.naive %>% augment %>% as_data_frame %>% select(price,.fitted,.resid) %>% 
  rename(fitted=.fitted,resid=.resid) %>% filter(fitted>quantile(fitted,0.01))

# (i.a) Test for heteroskedasticity
vt.hetero.dat <- vt.fitted %>% mutate(bin_fitted=factor(ntile(fitted,5))) %>%
  mutate(val=factor(bin_fitted,labels=paste('$',quantile(fitted,seq(0,0.8,0.2))%>%round(0),'-',
                                            quantile(fitted,seq(0.2,1.0,0.2))%>%round(0),sep='')))

# Create the gpplot showing heteroskedasticity
gg.vt.hetero <-
  ggplot(vt.hetero.dat %>% mutate(test=factor('')),aes(x=test,y=resid,color=bin_fitted)) +  
  geom_point() + geom_jitter(width = 1) + 
  xlab('Fitted value ranges (quantiles)') + ylab('Distribution of residuals') +
  facet_wrap(~val,nrow=1) + scale_colour_discrete(name='City: ') +
  theme(legend.position='none',text=element_text(size=15),axis.ticks.x=element_blank())

###########################################################
### ----- STEP 4.C: REGRESSION WITH ROBUST ERRORS ----- ###
###########################################################

# Get the "meat" for the sandwich estimator
vt.meat <- hccm(vt.ols.naive,type='hc0')

# Get the White-adjusted standard errors!
vt.ols.white <- coeftest(vt.ols.naive,vcov=vt.meat)
# Compare the standard errors
vt.sd.compare <- 
rbind(tidy(vt.ols.naive) %>% select(term,std.error,p.value) %>% mutate(type='Naive'),
      tidy(vt.ols.white) %>% select(term,std.error,p.value) %>% mutate(type='Robust')) %>% tbl_df %>%
      mutate(term=term %>% gsub(x=.,'TRUE|FALSE|\\(|\\)','') %>%   
           gsub(x=.,'\\.',' ') %>% sapply(simpleCap,USE.NAMES=F)) %>%
  mutate(significant=ifelse(p.value<=0.05,'alpha','not.alpha'))

# Make a ggplot showing the difference
gg.naive.compare <-
ggplot(vt.sd.compare,
       aes(x=factor(term,levels=vt.sd.compare %>% arrange(std.error) %>% use_series(term) %>% unique),
           y=std.error,color=factor(type))) + 
  geom_point(aes(shape=factor(significant)),size=3) + coord_flip() + ylab('Standard Errors') + xlab('Regressors') +
  scale_color_discrete(name='Model type') + 
  scale_shape_discrete(name='',labels=c('P-value<0.05','P.value>0.05')) 

###########################################################
### ----- STEP 4.D: AUTOCORRELATION IN THE ERRORS ----- ###
###########################################################

# Combine data with date
vt.date.resid <- augment(vt.ols.naive,vt.ols.data) %>% tbl_df %>% rename(resid=.resid) %>% select(date,resid,city)
# Get the mean and variance for each day...
vt.date.moments <- vt.date.resid %>% group_by(date) %>% summarise(mu=mean(resid),se=sd(resid))

# Make the ggplot to view visually
gg.vt.resid <-
ggplot(vt.date.resid,aes(x=as.Date(date),y=resid)) + 
  geom_point(color='blue') + xlab('') + ylab('Residuals') + 
  geom_point(data=vt.date.moments,aes(x=as.Date(date),y=mu),size=3,color='black',shape=21,fill='white') + 
  theme(text=element_text(size=15)) + 
  annotate("text",size=5,color='black',label='White dots are means',x=as.Date('2016-08-15'),y=-2500)

vt.ar1 <- vt.date.moments %>% use_series(mu) %>% ts %>% arima(.,order=c(1,0,0))
vt.ar1

#############################################
### ----- STEP 4.E: CHOW BREAK TEST ----- ###
#############################################

# Run the regression on the two different cities
vt.ols.chow <- vt.ols.data %>% group_by(city) %>% do(ols=lm(price~.-1,data=select(.,price:has.laundry)))

# Do a CHOW TEST for the restricted/versus partitioned model
chow <- chow.test(r1=vt.ols.chow$ols[[1]],r2=vt.ols.chow$ols[[2]],u=vt.ols.naive)

# Get the White-adjusted standard errors for the city-level regessions
vt.ols.chow %>% do(tidy(coeftest(.$ols,vcov=hccm(.$ols,type='hc0'))))
# Combine the data together
vt.ols.chow.coef <- 
  data.frame(city=vt.ols.chow %>% tidy(ols) %>% select(city) %>% use_series(city),
             vt.ols.chow %>% do(tidy(coeftest(.$ols,vcov=hccm(.$ols,type='hc0'))))) %>% 
  mutate(term = term %>% gsub(x=.,'TRUE|FALSE|\\(|\\)','') %>% gsub(x=.,'\\.',' ') %>% 
           sapply(simpleCap,USE.NAMES=F) %>% gsub(x=.,'Intercept','Fixed Cost')) %>%
  tbl_df %>% group_by(city)

# Get the factor order which is the 'highest first', roughly
sig.order <- vt.ols.chow.coef %>% mutate(significant=ifelse(p.value<=0.05,T,F)) %>%
                mutate(sig.order=as.numeric(significant)) %>%
                arrange(desc(sig.order),desc(estimate)) %>%
                use_series(term) %>% unique

vt.ols.chow.coef <- 
vt.ols.chow.coef %>% mutate(term=factor(term,levels=rev(sig.order))) %>%
    mutate(is.sig=ifelse(p.value<=0.05,'alpha','not.alpha'))

# Create the coefficient plot
gg.vt.chow.coef <-
ggplot(vt.ols.chow.coef,aes(color=city)) + 
  geom_hline(yintercept = 0,color=gray(0.5),lty=2) +
  geom_linerange(aes(x=term,ymin=estimate-2*std.error,ymax=estimate+2*std.error),
                 lwd=1,position=position_dodge(0.5)) + 
  geom_point(size=2.5,aes(x=term,y=estimate,shape=is.sig),position = position_dodge(0.5),fill='white') + 
  coord_flip() + theme_bw() + xlab('OLS Coefficients') + ylab('Regressors') + 
  scale_color_discrete(name='City: ') + 
  scale_shape_discrete(name='5% Signficiance',labels=c('P-value<0.05','P.value>0.05')) 

##############################################
### ----- STEP 4.E: INTERACTION TERM ----- ###
##############################################

# Make the interaction data set
vt.inter.dat <- vt.ols.data %>% mutate(is.van=ifelse(city=='Vancouver',1,0)) %>% 
                    select(is.van,price,intercept:has.laundry)
# Get the OLS tern
vt.ols.inter <- lm(price~is.van*(intercept+.)-1,data=vt.inter.dat)

# Clean up into a data.frame
vt.df.interact <- 
tidy(vt.ols.inter) %>% mutate(term=ifelse(term=='is.van','is.van:intercept',term)) %>% 
  mutate(is.van=ifelse(grepl('is\\.van',term),'Vancouver premium/discount','General features')) %>%
  mutate(term = term %>% gsub(x=.,'TRUE|FALSE|\\(|\\)|\\:|is\\.van','') %>% 
                                   gsub(x=.,'\\.',' ') %>% sapply(simpleCap,USE.NAMES=F)) %>% as_data_frame()

interact.order <- vt.df.interact %>% filter(is.van=='General features') %>% arrange(estimate) %>% use_series(term) %>% unique

gg.vt.interact <- 
ggplot(vt.df.interact %>% mutate(term=factor(term,levels=interact.order),
                                 sig=ifelse(p.value<=0.05,'alpha','non.alpha')),aes(x=term,y=estimate)) + 
         geom_point(size=3,aes(color=sig)) + 
         facet_wrap(~is.van) + coord_flip() + 
  ylab('OLS Coefficients') + xlab('Regressors') + geom_hline(yintercept = 0) + 
  scale_color_discrete(name='5% Signficiance',labels=c('P-value<0.05','P.value>0.05')) + 
  geom_linerange(aes(ymin=estimate-2*std.error,ymax=estimate+2*std.error,color=sig)) + theme_bw()

# Perform a log likelihood test comparing vt.ols.inter and vt.ols.naive
lrtest(vt.ols.naive,vt.ols.inter)

# Is the partioned model better though?
ssr.inter <- vt.ols.inter %>% deviance
ssr.chow <- vt.ols.chow %>% summarise(ssr=deviance(ols)) %>% use_series(ssr) %>% sum

###########################################
### ----- STEP 4.F: RESIDUAL TEST ----- ###
###########################################
x=vt.ols.inter %>% augment %>% use_series(.resid)
hist(x,breaks=40)
qqline(x)

##############################################
### ----- STEP 5: TABLES AND VISUALS ----- ###
##############################################

# Create the table to put in HTML format via stargazer
vt.inter.table <- 
vt.df.interact %>% select(term,is.van,estimate,p.value) %>% mutate(p.value=ifelse(p.value<0.05,'*','')) %>%
  transmute(term,is.van,r=paste(sprintf('%.1f',round(estimate,1)),p.value,sep='')) %>% 
  dcast(term~is.van,value.var = 'r') %>% mutate(test=factor(term,levels=interact.order,labels=1:length(term))) %>%
  arrange(desc(test)) %>% select(-test) %>% rename(Features=term)

# Print stargazer output
vt.inter.table %>% as.matrix %>% stargazer(type='text')

