####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Aldo rescues me once more...
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

## load packages
library(ggplot2)
library(dplyr)

##+++++++++++++++++++++++++++++++++
##          functions
##+++++++++++++++++++++++++++++++++
## Aldos function for binning the survival
plot_binned_prop <- function(df, n_bins, siz_var, rsp_var){
  
  size_var <- deparse( substitute(siz_var) )
  resp_var <- deparse( substitute(rsp_var) )
  
  # remove all NAs
  # remove all NAs
  na_ids   <- c( which( is.na(df[,size_var]) ),
                 which( is.na(df[,resp_var]) )
  ) %>% unique
  
  if( length(na_ids) > 0 ){
    df       <- df[-na_ids,]  
  }
  
  # binned survival probabilities
  h    <- (max(df[,size_var],na.rm=T) - min(df[,size_var],na.rm=T)) / n_bins
  lwr  <- min(df[,size_var],na.rm=T) + (h*c(0:(n_bins-1)))
  upr  <- lwr + h
  mid  <- lwr + (1/2*h)
  
  binned_prop <- function(lwr_x, upr_x, response){
    
    id  <- which(df[,size_var] > lwr_x & df[,size_var] < upr_x) 
    tmp <- df[id,]
    
    if( response == 'prob' ){   return( sum(tmp[,resp_var],na.rm=T) / nrow(tmp) ) }
    if( response == 'n_size' ){ return( nrow(tmp) ) }
    
  }
  
  y_binned <- Map(binned_prop, lwr, upr, 'prob') %>% unlist
  x_binned <- mid
  y_n_size <- Map(binned_prop, lwr, upr, 'n_size') %>% unlist
  
  data.frame(x_binned, y_binned,
             n_s  = y_n_size,
             xlab = size_var,
             ylab = resp_var ) %>% 
    mutate( n_prob = y_n_size/sum(y_n_size) )
  
}

## logistic function
inv_logit <- function(int, slope, sv) {
  1/(1 + exp(-(int + slope * sv)))
}

## read data
dat <- read.csv("https://raw.githubusercontent.com/Martin19910130/Aldo_meeting/main/bro_data.csv")

##survival 
glm(survival ~ size_t0 * climate, data = dat)

