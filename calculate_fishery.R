


calculate_fishery <- function( harvest, prices, plot = FALSE)
  
  
{ set.seed(69)
  library(tidyverse)
  loc_rev <-left_join(harvest, prices, by = 'fish' ) %>% 
    group_by(location) %>% 
    mutate(revenue = catch * price) %>% 
    summarise(total_revenue = sum(revenue))
  
  loc_rev$location <- as.factor(loc_rev$location)
  
  
  list_results <- 
  list((harvest %>% 
          group_by(location) %>% 
          summarize( most_common_species_by_volume = fish[which.max(catch)], catch_volume=max(catch),)), (
            left_join(harvest, prices, by = 'fish' ) %>% 
              group_by(location) %>% 
              mutate(revenue = catch * price) %>% 
              summarise(fishery_revenue = sum(revenue))
          ), (
            left_join(harvest, prices, by = 'fish' ) %>% 
              mutate(revenue = catch * price) %>% 
              summarise(total_fishery_revenue = sum(revenue))
          ), ggplot(data = loc_rev, aes(x= location, y= total_revenue))+
         geom_histogram(stat = 'identity')+
         xlab("Location")+
         ylab("Total Revenue")+
         ggtitle("Total Fisheries Revenue by Location"))

if (plot == TRUE) return (list_results)

else 
  return (
    list((harvest %>% 
            group_by(location) %>% 
            summarize( most_common_species_by_volume = fish[which.max(catch)], catch_volume=max(catch),)), (
              left_join(harvest, prices, by = 'fish' ) %>% 
                group_by(location) %>% 
                mutate(revenue = catch * price) %>% 
                summarise(fishery_revenue = sum(revenue))
            ), (
              left_join(harvest, prices, by = 'fish' ) %>% 
                mutate(revenue = catch * price) %>% 
                summarise(total_fishery_revenue = sum(revenue)))
    ))

} 
