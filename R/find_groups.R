# This function was initially created by R-Ladies for meetupr package.
# I added `fields` to find_groups() arguments to retrieve optional fields
# from Meetup API that are not retrieved by default.

## Example usage for this `find_groups()`:
## all_rladies_groups <- find_groups(text = "r-ladies", fields = "past_event_count, upcoming_event_count", api_key = meetup_api_key)

# I also updated the tibble to return `link`, `past_event_counts` and `upcoming_event_counts` which are the optional
# fields I plan to retrieve for each group.

# One issue: `past_event_counts` and `upcoming_event_counts` return 0 in the tibble if they are not requested for as part of the query
# in `fields` argument since they are optional fields.

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
    link = purrr::map_chr(res, "link"),
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
    past_events = purrr::map_dbl(res, "past_event_count", .default = 0),
    upcoming_events = purrr::map_dbl(res, "upcoming_event_count", .default = 0),
    resource = res
  )
}
