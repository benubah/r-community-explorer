library(meetupr)

find_groups <- function(text = NULL, topic_id = NULL, radius = "global", fields = NULL, api_key = NULL) {
  api_method <- "find/groups"
  res <- .fetch_results(api_method = api_method,
                        api_key = api_key,
                        text = text,
                        topic_id = topic_id,
                        fields = fields,
                        radius = radius)
  tibble::tibble(
    id = purrr::map_int(res, "id"),
    name = purrr::map_chr(res, "name"),
    urlname = purrr::map_chr(res, "urlname"),
    created = .date_helper(purrr::map_dbl(res, "created")),
    members = purrr::map_int(res, "members"),
    status = purrr::map_chr(res, "status"),
    organizer = purrr::map_chr(res, c("organizer", "name")),
    lat = purrr::map_dbl(res, "lat"),
    lon = purrr::map_dbl(res, "lon"),
    city = purrr::map_chr(res, "city"),
    state = purrr::map_chr(res, "state", .null = NA),
    country = purrr::map_chr(res, "localized_country_name"),
    timezone = purrr::map_chr(res, "timezone", .null = NA),
    join_mode = purrr::map_chr(res, "join_mode", .null = NA),
    visibility = purrr::map_chr(res, "visibility", .null = NA),
    who = purrr::map_chr(res, "who", .null = NA),
    organizer_id = purrr::map_int(res, c("organizer", "id")),
    organizer_name = purrr::map_chr(res, c("organizer", "name")),
    category_id = purrr::map_int(res, c("category", "id"), .null = NA),
    category_name = purrr::map_chr(res, c("category", "name"), .null = NA),
    resource = res
  )
}


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
