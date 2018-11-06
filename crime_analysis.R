#source cleaning functions

source("crim_library.R")

##### A 
    ## create crime.data tibble 
    crime.data <- get_site_content(neighborhoods_URL)
    crime.data <- content_to_parsed_html(crime.data)
    crime.data <- parse_crimes_dates(crime.data)
    crime.data <- cleaning_function(crime.data)
    
    crime.data <- as.tibble(crime.data)
    head(crime.data)
    str(crime.data)    

##### B 
    ## Five most common crime types aggregated across neighborhoods and hours, and how many of each such crime occurred? 
    
      crime.data %>%
      count(crime) %>%
      arrange(desc(n)) %>%
      slice(1:5)
    
##### C    
    ## Make a plot of the total number of crimes (aggregated across neighborhoods and crime types) by hour. 
    ## Write a few sentences about the pattern you observe.
    
# library(randomcoloR)   
#      colorss <- randomcoloR::distinctColorPalette(k = 36)
     
     crime.data %>%
      group_by(hour)%>%
      count(crime) %>%
      summarise(n=sum(n)) %>%
      arrange(desc(n)) %>%
      ggplot(aes(x=hour, y = n)) + geom_line() + ggtitle("Total Number of Crimes across Boston Neighborhoods by Hour")

##### D 
     ## Restrict to the five most common crime types, and plot the total number of crimes (aggregated across neighborhoods) 
     ## for each crime type by hour (i.e., your plot should have five lines). Write a few sentences about the pattern you observe.
     
      crime.data %>%
       filter(crime %in% c("shooting", "gunfire", "murder", "assault", "stabbing")) %>%
       group_by(hour) %>%
       count(crime) %>%
       arrange(desc(n)) %>%
       ggplot(aes(x=hour, y = n, color = crime)) + geom_line() + ggtitle("Five Most Common Crime Types by Hour")

##### E
    ## Restrict to just the neighborhoods of Dorchester and Downtown,and plot the total number of crimes (aggregated across
    ## crime types (include all crime types, not just the top five)) for each of the two neighborhoods by hour 
    ## (i.e., your plot should have two lines). Write a few sentences about the pattern you observe.
      
      crime.data %>%
        filter(neighborhood %in% c("dorchester", "downtown")) %>%
        group_by(hour, neighborhood)%>%
        count(crime) %>%
        summarise(n=sum(n)) %>%
        arrange(neighborhood) %>%
        ggplot(aes(x=hour, y = n, color = neighborhood)) + geom_line() + ggtitle("Dorchester and Downtown Total Number of Crimes by Hour")
        
        
        