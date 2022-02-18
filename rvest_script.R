#install.packages("rvest")
#install.packages("tidyverse")
#install.packages("lubridate")
#install.packages("stringr")
#install.packages("rebus")
library(rvest)
library(tidyverse)
library(lubridate)
library(stringr)
library(rebus)
library(curl)

#initial URL--phenology tables from MN wildflowers

# start with each month page
# extract info from table
# repeat!

#plants by name

url<-"https://www.minnesotawildflowers.info/page/plants-by-name"
plants<-url %>% 
  read_html() %>% 
  html_nodes(xpath=path) %>% 
  html_table()
path<-'//*[@id="content"]/div/table'

plants <- plants [[1]]
plants 

#this gives a list of every plant and common name



### now we want to open each plant page (sub-url) from within this main table.
## we need to look at how to reference those urls and loop thorugh them
## and then what content we want



# we want a function that extracts trait table from page

#function
get_plant_info<- function(url) {
  
  path<-as.character('//*[@id="content"]/div/table')# the xpath to table
 download.file(url, destfile = "MNFLW.html", quiet=TRUE)#much slower :( 
  page <- read_html("MNFLW.html")
# page<- read_html(curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0")))
  nodes <- html_nodes(page,xpath= path) #for some reason xpath only works as object
  plant_info <- html_table(nodes)
  
  plant_info <- plant_info[[1]]
 

  plant_info$X1 <- gsub(':', '', plant_info$X1)# drop colons
  
  #get plant species using same code
 
  name_path<-as.character('//*[@id="content"]/div/h2/i')
  name_nodes<-html_nodes(page,xpath=name_path)
  plant_name <-html_text(name_nodes)
  
  #now recode
                      
   

    
    plant_info<-plant_info%>%
      add_row(X1="Scientific Name", X2=plant_name)
    # tack on scientific name data as a new using add_row and plant_name. 
    
  #drop distribution stuff
    plant_info<-plant_info[!(plant_info$X1=="MN county distribution (click map to enlarge)"),]
    plant_info<-plant_info[!(plant_info$X1=="National distribution (click map to enlarge)"),]
    
    #now spread so its in long form and so each iteration will tack on in a way that's conducive to storage
    plant_info<-pivot_wider(plant_info, names_from=X1, values_from= X2)
    # now move species name to front
    plant_info<-plant_info%>%
      select("Scientific Name", everything())
    
    # now you need to pull flower shape info
    
    shape_pull<- page %>%
      html_nodes("h4")%>%html_nodes("img")%>%html_attr('alt')
    
    #now make a tibble or df
    shape<-enframe(shape_pull)
    
    if(length(shape$name)==0){
      shape[nrow(shape)+1, ] <- c(1, 'Flower shape: ')
      }
      #if this doesn't pull any flower shape data, add this
      
    
    #now split into shape variable and type and spread
    flw_shape<-shape%>%select(-name)%>%separate(value,c("shape_variable","type"),sep=': ')%>%
      mutate(shape_variable=str_replace(shape_variable," ", "_"))%>%
      mutate(ID= ave(as.character(interaction(shape_variable)),
                     interaction(shape_variable), FUN = seq_along)) %>% #adds ID that identifies repeats of shape_variable 
      pivot_wider(names_from=c(shape_variable,ID),values_from=type)%>%mutate_all(na_if,"")
    
    
                                   
    plant_info<-bind_cols(mutate_all(plant_info,as.character),mutate_all(flw_shape,as.character))
    
 
  return(plant_info)
}

test<-'https://www.minnesotawildflowers.info/flower/intermediate-pond-lily'
get_plant_info(test)
# now get plant names and URLs to run through function


### check this code for error
# Open main_url and navigate to interesting part of the page:
main_url <-"https://www.minnesotawildflowers.info/page/plants-by-name"
main_page <- read_html(main_url)
plants_table<-'//*[@id="content"]/div/table'
#plants_table<-'//*[@id="content"]/div/table/tbody'
plants_html <- html_nodes(main_page, xpath=plants_table)# we want to go to each plant page

# plant names
m_plants <- html_table(plants_html)
m_plants<-m_plants[[1]]



## urls
urls <- main_page %>%
  html_nodes("table") %>% html_nodes("tr") %>% html_nodes("a") %>%
  html_attr("href") #this goes down each node of the table until we get to the urls under "href"

#lost two because of special character; add back in



# got it!

### alternatively you can do it tidyverse way:
#plants by name

# url<-"https://www.minnesotawildflowers.info/page/plants-by-name"
# plants<-url %>% 
#   read_html() %>% 
#   html_nodes(xpath='//*[@id="content"]/div/table') %>% 
#   html_table()
# 
# 
# plants <- plants [[1]]
# plants 
# 
# #this gives a list of every plant and common name

# back to it:

m_urls <- paste0('https://www.minnesotawildflowers.info', urls)

#test @increments of 100 to see where issues are
m_plant_info<- map_df(m_urls, get_plant_info)



### tidy up m_plant_info

#remove spaces from column names
names(m_plant_info) <- gsub(" ", "_", names(m_plant_info))

#reorder variables & split Family into Family & Common Family columns
colnames(m_plant_info)

m_plant_info_order<-m_plant_info%>%
  select(Scientific_Name,Also_known_as,Genus,Family,Life_cycle,Origin, Habitat,Bloom_season,Fruiting_season,
         Plant_height,Wetland_Indicator_Status,Status,Flower_shape_1,Flower_shape_2,Flower_shape_3,Flower_shape_4,
         Cluster_type_1,Cluster_type_2,Cluster_type_3,Leaf_type_1,Leaf_type_2,Leaf_type_3,Leaf_type_4,Leaf_attachment_1,
         Leaf_attachment_2,Leaf_attachment_3,Fruit_type_1,Fruit_type_2)%>%
  mutate(Family=gsub("[()]", "", Family))%>%mutate(Family = str_replace(Family, "\\s", "|"))%>%
  separate(Family,into=c("Family","Common_family"), sep = "\\|")

### Now add in flower colors
# some plants will have more than one

#start with blue

blue_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/blue?pID=0'


#tidy version
blue<-blue_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

blue_flw<-blue%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_blue="blue")%>%select(-name)

#yellow
yellow_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/yellow?pID=0'

yellow<-yellow_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

yellow_flw<-yellow%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_yellow="yellow")%>%select(-name)


#red
red_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/red?pID=0'

red<-red_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

red_flw<-red%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_red="red")%>%select(-name)

#orange

orange_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/orange?pID=0'

orange<-orange_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

orange_flw<-orange%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_orange="orange")%>%select(-name)

#white

white_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/white?pID=0'

white<-white_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

white_flw<-white%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_white="white")%>%select(-name)

#purple

purple_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/purple?pID=0'

purple<-purple_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

purple_flw<-purple%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_purple="purple")%>%select(-name)


#pnk

pink_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/pink?pID=0'

pink<-pink_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

pink_flw<-pink%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_pink="pink")%>%select(-name)


#green

green_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/green?pID=0'

green<-green_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

green_flw<-green%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_green="green")%>%select(-name)


#brown

brown_url<-'https://www.minnesotawildflowers.info/page/flowers-by-color/other'

brown<-brown_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

brown_flw<-brown%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_brown="brown")%>%select(-name)


#indeterminate (same as brown, but kept as separate column)
indeterminate<-brown_url %>% 
  read_html() %>% 
  html_nodes(xpath='//*[@id="content"]/div/div/ul/li/a')%>%html_text()

indeterminate_flw<-indeterminate%>%enframe()%>%rename(Scientific_Name=value)%>%add_column(flower_color_indeterminate="indeterminate")%>%select(-name)
library(rvest)
##### now join all
MN_wildflowers_plant_traits<-m_plant_info_order%>%full_join(red_flw,by='Scientific_Name')%>%
  full_join(orange_flw,by='Scientific_Name')%>%
  full_join(yellow_flw,by='Scientific_Name')%>%
  full_join(green_flw,by='Scientific_Name')%>%
  full_join(blue_flw,by='Scientific_Name')%>%
  full_join(purple_flw,by='Scientific_Name')%>%
  full_join(pink_flw,by='Scientific_Name')%>%
  full_join(white_flw,by='Scientific_Name')%>%
  full_join(brown_flw,by='Scientific_Name')%>%
  full_join(indeterminate_flw,by='Scientific_Name')%>%filter(!is.na(Genus))

MN_wildflowers_plant_traits<-MN_wildflowers_plant_traits%>%filter(!is.na(Genus))

### you might need to add in MN wildflowers to name to differentiate

###add in phenology data from jess & dustin
pheno_bell<-read.csv("C:/Users/alritchi/Documents/MN_plant_pheno_bell_JP_DG.csv")

#split Scienftific name into genus_species, so genus can be used to link info
# then you only need to worry about synonyms between this list and mn wildflowers
pheno_bell<-pheno_bell%>%mutate(Scientific_Name2=Scientific_Name)%>%
  separate(Scientific_Name2,into=c("genus","species"), extra="merge")%>%select(-species)
#first read in MN releve composition data; 
rel_cross<-read.csv("~/releve_vw_rel_data_crosswalk20200129.csv")
#now read in releve information
rel<-read.csv("~/releve_vw_releve_export20200129.csv")

#read in mn taxa list
plants<-read.csv("~/plants.csv")
plants<-plants%>%mutate(DNR_name=MN.DNR.Official.Name)%>%
  separate(MN.DNR.Official.Name,c("genus","species"),extra= "merge")#change to taxon_new; DROP EXTRA
#read in data cross walk from dustin & nathan
crosswalk<-read.csv("bell_mntaxa_crosswalk.csv")




##heck out taxon_new, releves, and plants
##

#read in phys codes
plant_phys<-read.csv("~/plant_phys_descriptions.csv")

plants_desc<-left_join(plants,plant_phys,by="phys")%>%rename(Scientific_Name=taxon_new)%>%
  group_by(genus,phys,phys_desc)%>%summarize()

#join pheno data-- you want a left join on the plants data from MNtaxa
pheno_phys<-pheno_bell%>%left_join(plants_desc, by="genus")


### run a join and see where synonyms exist
MN_wildflowers_pheno<-MN_wildflowers_plant_traits%>%left_join(pheno_bell,by="Scientific_Name")
### check anti
anti_check_MNWF<-MN_wildflowers_plant_traits%>%anti_join(pheno_bell,by="Scientific_Name")

#subsp issue--you can split off to give flower data for all
# and then apply to all rows, so repeats for each subsp; but then summarize out
# ex, carduus 

# trees- not sure of a fix here. they just don't seem to be represented




#plots@
