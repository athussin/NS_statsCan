# set_api_key("CensusMapper_f48566f8233bebc79bece0d5fe8a6fa8", install = TRUE, overwrite=TRUE)

rm(list = ls())

library(cancensus) # access, retrieve, and work with Census data
library(cancensusHelpers) # personal use helper functions - development
library(dplyr) # data manipulation
library(purrr) # recursive programming
library(tidyr) # reshaping data
library(sf) # spatial data and visualization
library(ggplot2) # visualization 
library(ggrepel) # for smarter labelling
library(hrbrthemes) # typographic theme elements
library(rmapzen) # vector tiles for maps

datasets <- data.frame(list_census_datasets(use_cache = TRUE, quiet = FALSE))
dataset <- "CA16" # Select StatsCanada 2016 database
regions_df <- data.frame(list_census_regions("CA16"))
vectors_df <- list_census_vectors("CA16")
vectors_df<-vectors_df[!(vectors_df$type=="Female" | vectors_df$type=="Male"),]


# explore_census_vectors(dataset = "CA16") # opens censusmapper.ca with map and vars to view in roseplot

# 100% data = income, language (most spoken and mother tongue), counts by age and type of dwelling (house/apt/detached etc)

#####################
#######################
########################
#######################
#####################
#######################
########################
######################

# retrieve sf dataframe
lunenburg <- get_census(dataset='CA16', regions=list(PR="1206"),
                      vectors=c("var"="v_CA16_3822"), level='CSD', quiet = TRUE, 
                      geo_format = 'sf', labels = 'short')

#####################
#######################
########################
#######################
#####################
#######################
########################
######################

library(cancensus) # access, retrieve, and work with Census data
library(cancensusHelpers) # personal use helper functions - development
library(dplyr) # data manipulation
library(purrr) # recursive programming
library(tidyr) # reshaping data
library(sf) # spatial data and visualization
library(ggplot2) # visualization 
library(ggrepel) # for smarter labelling
library(hrbrthemes) # typographic theme elements
library(rmapzen) # vector tiles for maps

dataset='CA16'
level="CSD"
median_income_vectors <- list_census_vectors(dataset, quiet=TRUE) %>% 
  filter(type=="Total",grepl("Median",label),grepl("income",label)) %>% pull("vector") 
regions <- list_census_regions(dataset) %>% filter(level==!!level) %>% top_n(10,pop) %>% as_census_region_list


library(ggplot2)
library(dplyr)
labels=list("v_CA16_2207"="Individual Total",
            "v_CA16_2213"="Individual After Tax",
            "v_CA16_2219"="Individual Market",
            "v_CA16_2231"="Individual Employment",
            "v_CA16_2397"="Household Total",
            "v_CA16_2398"="Household After Tax",
            "v_CA16_2400"="One person Household Total",
            "v_CA16_2401"="One person Household After Tax",
            "v_CA16_2403"="Two+ person Household Total",
            "v_CA16_2404"="Two+ person Household After Tax",
            "v_CA16_2447"="Family Total",
            "v_CA16_2448"="Family After Tax",
            "v_CA16_2451"="Couples w/o Children Total",
            "v_CA16_2452"="Couples w/o Children After Tax",
            "v_CA16_2455"="Couples with Children Total",
            "v_CA16_2456"="Couples with Children After Tax",
            "v_CA16_2459"="Lone Parent Total",
            "v_CA16_2460"="Lone Parent After Tax",
            "v_CA16_2465"="Unattached Total",
            "v_CA16_2468"="Unattached After Tax"
)
currency_format <- function(x){return(paste0("$",format(x,big.mark = ",")))}
currency_format_short <- function(d){return(paste0("$",d/1000,"k"))}
data_for <- function(region,vector){
  return(filter(data,grepl(region,`Region Name`))[[vector]] %>% currency_format)
}

data <- get_census(dataset = 'CA16',
                   level="Regions",
                   vectors=median_income_vectors , 
                   regions=regions, 
                   geo_format = NA,
                   labels='short')

plot_data <- data %>% select(c("Region Name",median_income_vectors)) %>% 
  reshape2::melt(id="Region Name") %>%
  mutate(`Region Name` = factor(`Region Name`, 
                                levels = data %>% arrange(desc(Population)) %>% pull("Region Name"),
                                ordered=TRUE)) 
plot_data$var <- factor(as.character(labels[plot_data$variable]),levels=as.character(labels),ordered=TRUE)

ggplot(plot_data , aes(x = `Region Name`, y = value, fill=`Region Name`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~var) +
  scale_y_continuous(labels=currency_format_short) +
  labs(fill = paste0("Top 10 ",level,"s by Population"), 
       y = "Median Income",
       x="",
       title="Median Income 2015 Various Statistics",
       caption="Canada Census 2016 via cancensus & CensusMapper.ca") +
  theme_bw() + 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank()) +
  coord_flip()
