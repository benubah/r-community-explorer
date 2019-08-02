library(meetupr)
library(jsonlite)

#internals.R from rladies/meetupr package
# This helper function makes a single call, given the full API endpoint URL
# Used as the workhorse function inside .fetch_results() below
.quick_fetch <- function(api_url,
                         api_key = NULL,
                         event_status = NULL,
                         offset = 0,
                         ...) {
  
  # list of parameters
  parameters <- list(key = api_key,         # your api_key
                     status = event_status, # you need to add the status
                     # otherwise it will get only the upcoming event
                     offset = offset,
                     ...                    # other parameters
  )
  
  req <- httr::GET(url = api_url,          # the endpoint
                   query = parameters)
  
  httr::stop_for_status(req)
  reslist <- httr::content(req, "parsed")
  
  if (length(reslist) == 0) {
    stop("Zero records match your filter. Nothing to return.\n",
         call. = FALSE)
  }
  
  return(list(result = reslist, headers = req$headers))
}


# Fetch all the results of a query given an API Method
# Will make multiple calls to the API if needed
# API Methods listed here: https://www.meetup.com/meetup_api/docs/
.fetch_results <- function(api_method, api_key = NULL, event_status = NULL, ...) {
  
  # Build the API endpoint URL
  meetup_api_prefix <- "https://api.meetup.com/"
  api_url <- paste0(meetup_api_prefix, api_method)
  
  # Get the API key from MEETUP_KEY environment variable if NULL
  if (is.null(api_key)) api_key <- .get_api_key()
  if (!is.character(api_key)) stop("api_key must be a character string")
  
  # Fetch first set of results (limited to 200 records each call)
  res <- .quick_fetch(api_url = api_url,
                      api_key = api_key,
                      event_status = event_status,
                      offset = 0,
                      ...)
  
  # Total number of records matching the query
  total_records <- as.integer(res$headers$`x-total-count`)
  if (length(total_records) == 0) total_records <- 1L
  records <- res$result
  cat(paste("Downloading", total_records, "record(s)..."))
  
    if((length(records) < total_records) & !is.null(res$headers$link)){
      
      # calculate number of offsets for records above 200
      offsetn <- ceiling(total_records/length(records))
      all_records <- list(records)
      
      for(i in 1:(offsetn - 1)) {
        res <- .quick_fetch(api_url = api_url,
                            api_key = api_key,
                            event_status = event_status,
                            offset = i,
                            ...)
        all_records[[i + 1]] <- res$result
      }
      records <- unlist(all_records, recursive = FALSE)
      
    }
  
  return(records)
}


# helper function to convert a vector of milliseconds since epoch into POSIXct
.date_helper <- function(time) {
  if (is.character(time)) {
    # if date is character string, try to convert to numeric
    time <- tryCatch(expr = as.numeric(time),
                     error = warning("One or more dates could not be converted properly"))
  }
  if (is.numeric(time)) {
    # divide milliseconds by 1000 to get seconds; convert to POSIXct
    seconds <- time / 1000
    out <- as.POSIXct(seconds, origin = "1970-01-01")
  } else {
    # if no conversion can be done, then return NA
    warning("One or more dates could not be converted properly")
    out <- rep(NA, length(time))
  }
  return(out)
}

# function to return meetup.com API key stored in the MEETUP_KEY environment variable
.get_api_key <- function() {
  api_key <- Sys.getenv("MEETUP_KEY")
  if (api_key == "") {
    stop("You have not set a MEETUP_KEY environment variable.\nIf you do not yet have a meetup.com API key, you can retrieve one here:\n  * https://secure.meetup.com/meetup_api/key/",
         call. = FALSE)
  }
  return(api_key)
}


#updated  find_groups() to retrieve optional fields from Meetup API
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
    lat = purrr::map_dbl(res, "lat"),
    lon = purrr::map_dbl(res, "lon"),
    city = purrr::map_chr(res, "city"),
    state = purrr::map_chr(res, "state", .null = NA),
    country = purrr::map_chr(res, "localized_country_name"),
    timezone = purrr::map_chr(res, "timezone", .null = NA),
    join_mode = purrr::map_chr(res, "join_mode", .null = NA),
    visibility = purrr::map_chr(res, "visibility", .null = NA),
    who = purrr::map_chr(res, "who", .null = NA),
    organizer_id = purrr::map_int(res, c("organizer", "id"), .default = NA),
    organizer_name = purrr::map_chr(res, c("organizer", "name"),.default = NA),
    category_id = purrr::map_int(res, c("category", "id"), .null = NA),
    category_name = purrr::map_chr(res, c("category", "name"), .null = NA),
    resource = res
  )
}

# custom function that uses the updated find_groups() to retrieve rladies groups
get_rladies <- function() {
meetup_api_key <- Sys.getenv("MEETUP_KEY")
 
# retrieve both past and upcoming event counts while finding groups
 all_rladies_groups <- find_groups(text = "r-ladies", fields = "past_event_count, upcoming_event_count, last_event", api_key = meetup_api_key)

# Cleanup
rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies|r ladies",  x = all_rladies_groups$name, ignore.case = TRUE), ]  
 
 past_event_counts <- purrr::map_dbl(rladies_groups$resource, "past_event_count", .default = 0)
 upcoming_event_counts <- purrr::map_dbl(rladies_groups$resource, "upcoming_event_count", .default = 0)
 last <- lapply(rladies_groups$resource, "[[", "last_event")
  last_event <- .date_helper(purrr::map_dbl(last, "time", .default = 0))
  last_event <- as.Date(last_event)
  days_since_last_event  <- as.integer(Sys.Date() - last_event)
  
  # add a full urlname, past_events and upcoming_events as another column
 rladies_groups$fullurl <- paste0("https://www.meetup.com/", rladies_groups$urlname, "/")
 rladies_groups$url <- paste0("<a href='", rladies_groups$fullurl, "'>", rladies_groups$name, "</a>") 
 rladies_groups$past_events <- past_event_counts
 rladies_groups$upcoming_events <- upcoming_event_counts
  rladies_groups$last_event <- last_event
  rladies_groups$days_since_last_event <- days_since_last_event
  rladies_groups[grepl("America",rladies_groups$timezone),]$timezone <- "Latin America"
  rladies_groups[grepl("US|Canada",rladies_groups$timezone),]$timezone <- "US/Canada"
  rladies_groups[grepl("Europe",rladies_groups$timezone),]$timezone <- "Europe"
  rladies_groups[grepl("Africa",rladies_groups$timezone),]$timezone <- "Africa"
  rladies_groups[grepl("Asia",rladies_groups$timezone),]$timezone <- "Asia"
  rladies_groups[grepl("Australia|Pacific/Auckland",rladies_groups$timezone),]$timezone <- "Australia"

  colnames(rladies_groups)[colnames(rladies_groups) == 'timezone'] <- 'region'

 
  # obtain cumulative count of chapters over the years and save in JSON
 datecreated <- sort(as.Date(rladies_groups$created))
  rladies_groups$created <-  as.Date(rladies_groups$created)
  count_date <- table(datecreated)
  # generate new vector of all days in the time frame
newdate <- seq(datecreated[1], datecreated[length(datecreated)], by = "days") 

count_newdate <- table(newdate)
actindex <- match(names(count_newdate),names(count_date),nomatch = 0)
days <- function(actindex,daycount){
  n <- length(actindex)
  x <- rep(NA,times=n)
  zero <- 0
  for (i in 1:n){
    if (actindex[i]==0) {
      zero <- zero +1
      x[i] <- 0
    } else {
      x[i] <- daycount[i-zero]
    }			
  }
  return(x)
}
 alldaycount <- array(days(actindex,count_date))   # construct vector with number of new chapters per day
 names(alldaycount) <- names(count_newdate) # name entries by consecutive dates.
 cumsum_rladies <- data.frame(newdate,cumsum(alldaycount))
 rownames(cumsum_rladies) <- c()
 colnames(cumsum_rladies) <- c("datecreated", "Freq")
 cumulative_rladies <- toJSON(cumsum_rladies, pretty = TRUE)
  writeLines(cumulative_rladies, "docs/data/rladies_cumulative.json")
  
  
  # otain summaries around rladies groups and save in JSON
  rladies_chapters <- dim(rladies_groups)[1]
  rladies_countries <- length(unique(rladies_groups$country))
  rladies_city <- length(unique(rladies_groups$city))
  rladies_members <- sum(rladies_groups$members)
  rladies_past_events <- sum(rladies_groups$past_events)
  rladies_upcoming_events <- sum(rladies_groups$upcoming_events)
  average_member_chapter <- floor(rladies_members / rladies_chapters)
  average_chapter_country <- floor(rladies_chapters / rladies_countries)
  average_event_chapter <- floor(rladies_past_events / rladies_chapters)

  rladies_df <- data.frame(chapters = rladies_chapters, countries = rladies_countries,
                         city = rladies_city, members = rladies_members, past_events = rladies_past_events,
                         upcoming_events = rladies_upcoming_events, avgchapter = average_chapter_country,
                         avgevent = average_event_chapter, avgmember = average_member_chapter)
   
  # specify columns to retain
  col_to_keep <- c("name", "city", "country",  "region", "members", "fullurl", "created", "past_events", "upcoming_events")
  rladies_groups2 <- rladies_groups[col_to_keep]
  write.csv(rladies_groups2, "docs/data/rladies.csv")   
  
   #for leaflet map save to geoJSON
  col_to_keep <- c("name", "url", "created", "members","past_events","upcoming_events", "last_event", "days_since_last_event", "lat","lon")
  rladies_map_data <- rladies_groups[col_to_keep]
  leafletR::toGeoJSON(data = rladies_map_data, dest = "docs/data/")
  
 # select latin american groups
  latam <- sort(unique(rladies_groups[grep("Latin America", rladies_groups$region),]$country))
latam_groups <- rladies_groups[rladies_groups$country %in% latam,]
lt <- dim(latam_groups)[1]
lt_members <- sum(latam_groups$members)

# Europe
europe <- sort(unique(rladies_groups[grep("Europe", rladies_groups$region),]$country))
eu_groups <- rladies_groups[rladies_groups$country %in% europe,]
eu <- dim(eu_groups)[1]
eu_members <- sum(eu_groups$members)

  
  # USA and Canada
  uscan <- sort(unique(rladies_groups[grep("US/Canada", rladies_groups$region),]$country))
uscangroups <- rladies_groups[rladies_groups$country %in% uscan,]
us_canada <- dim(uscangroups)[1]
us_can_members <- sum(uscangroups$members)
  
  
  # Africa
  africa <- sort(unique(rladies_groups[grep("Africa", rladies_groups$region),]$country))
africa_groups <- rladies_groups[rladies_groups$country %in% africa,]
af <- dim(africa_groups)[1]
af_members <- sum(africa_groups$members)

# Asia
asia <- sort(unique(rladies_groups[grep("Asia", rladies_groups$region),]$country))
asia_groups <- rladies_groups[rladies_groups$country %in% asia,]
as <- dim(asia_groups)[1]
as_members <- sum(asia_groups$members)

#  Australia/Oceania
australia <- sort(unique(rladies_groups[grep("Australia", rladies_groups$region),]$country))
australia_groups <- rladies_groups[rladies_groups$country %in% australia,]
au <- dim(australia_groups)[1]
au_members <- sum(australia_groups$members)
  
  continent_df <- data.frame(latinAm = lt, us_can = us_canada, eur = eu, afr = af, asia = as, aus = au,
                             latinAm_m = lt_members, us_can_m = us_can_members, eur_m = eu_members, afr_m = af_members, asia_m = as_members, aus_m = au_members)
  
  rladies_json <- jsonlite::toJSON(list(rladies_df,continent_df), auto_unbox = FALSE, pretty = TRUE)
  writeLines(rladies_json, "docs/data/rladies_summary.json")
    
}
get_rladies()
