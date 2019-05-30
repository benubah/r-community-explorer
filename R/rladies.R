#library(meetupr)
library(rcommunityexplorer)
get_rladies <- function() {
 
meetup_api_key <- Sys.getenv("MEETUP_KEY")
 
# retrieve both past and upcoming event counts while finding groups
  all_rladies_groups <- find_groups(text = "r-ladies", fields = "past_event_count, upcoming_event_count", api_key = meetup_api_key)

# Cleanup
rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies|r ladies",  x = all_rladies_groups$name, ignore.case = TRUE), ]  
 
 past_event_counts <- purrr::map_dbl(rladies_groups$resource, "past_event_count", .default = 0)
 upcoming_event_counts <- purrr::map_dbl(rladies_groups$resource, "upcoming_event_count", .default = 0)
 
  # add a full urlname, past_events and upcoming_events as another column
 rladies_groups$fullurl <- paste0("https://www.meetup.com/", rladies_groups$urlname, "/")
 rladies_groups$past_events <- past_event_counts
 rladies_groups$upcoming_events <- upcoming_event_counts
 
  col_to_keep <- c("name", "city", "country",  "timezone", "members", "created", "fullurl", "past_events", "upcoming_events")
  rladies_groups <- rladies_groups[col_to_keep]
  
write.csv(rladies_groups, "docs/data/rladies.csv")   
}

get_rladies()
