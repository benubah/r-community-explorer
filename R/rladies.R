library(meetupr)

get_rladies <- function() {
 
meetup_api_key <- Sys.getenv("MEETUP_KEY")
all_rladies_groups <- meetupr::find_groups(text = "r-ladies", api_key = meetup_api_key)

# Cleanup
rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies|r ladies",  x = all_rladies_groups$name, ignore.case = TRUE), ]
  
 
  col_to_keep <- c("name", "city", "country", "members", "created","timezone")
  rladies_groups <- rladies_groups[col_to_keep]
  
write.csv(rladies_groups, "docs/data/rladies.csv")
   
}

get_rladies()
