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

find_groups <- function(text = NULL, topic_id = NULL, radius = "global", fields = NULL, api_key = NULL) {
  api_method <- "find/groups"
  # If topic_id is a vector, change it to single string of comma separated values
  if(length(topic_id) > 1){
    topic_id <- paste(topic_id, collapse = ",")
  }
  # If fields is a vector, change it to single string of comma separated values
  if(length(fields) > 1){
    fields <- paste(fields, collapse = ",")
  }
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


get_rugs <- function() {
  meetup_api_key <- Sys.getenv("MEETUP_KEY")
  # retrieve both past and upcoming event counts, last_event and topics while finding groups that mention 'r-project-for-statisctical-computing'
  all_ruser_groups <- find_groups(text = "r-project-for-statistical-computing", fields = "past_event_count, upcoming_event_count, last_event, topics", api_key = meetup_api_key)

  # retrieve all data science groups
  all_ds_groups <- find_groups(text = "data-science", fields = "past_event_count, upcoming_event_count, last_event, topics", api_key = meetup_api_key)
 
  # Just check if groups are listed multiple times with different leading or trailing spaces
  trim.strings <- function(x, side = "both") { 
    if (is.na(match(side, c("both", "leading", "trailing")))) { 
      side <- "both" 
   } 
   if (side == "leading") { 
      sub("^\\s+", "", x)
   } else {
     if (side == "trailing") {
       sub("\\s+$", "", x)
     } else gsub("^\\s+|\\s+$", "", x)
   } 
  } 
  
  r_user_groups1 <- all_ds_groups[grepl("-user-|-r-|phillyr|boston-user|r-users-sydney|rug|scotland-data|bioconductor|r-data|data-mining|satrday", tolower(all_ds_groups$urlname)),]
r_user_groups2 <- all_ds_groups[grepl("r user|r-user|r-ladies|r ladies|rladies|r-lab|phillyr|rug|bioconductor|r-data|rug", tolower(all_ds_groups$urlname)),]
combined_ruser_groups1  <- rbind(r_user_groups1, r_user_groups2)
filtered_group1 <- combined_ruser_groups1[grepl("-r-|r-user|r-lab|rug|scotland-data|programming-in-r|r-programming-|-using-r|r-language|r-project-for-statistical", tolower(combined_ruser_groups1$resource)),]
combined_ruser_groups2 <-  rbind(filtered_group1, all_ruser_groups)
total_ruser_groups <- combined_ruser_groups2[!duplicated(trim.strings(combined_ruser_groups2$urlname)),]

#Groups to filter out: Rapidminer user group, Looker user group, Jupyter user group, SQL Server User group, Biomarker Labs, 
#(note that these are other data-science user group names that end with r, they produce a combination of 'r-user' in urlnames)
r_groups <- total_ruser_groups[!grepl("rapidminer|looker|jupyter|sql-server|biomarker-labs|strugglers", tolower(total_ruser_groups$urlname)),]

  
  datecreated <- sort(as.Date(r_groups$created))
  r_groups$created <-  datecreated
  past_event_counts <- purrr::map_dbl(r_groups$resource, "past_event_count", .default = 0)
  upcoming_event_counts <- purrr::map_dbl(r_groups$resource, "upcoming_event_count", .default = 0)
  last <- lapply(r_groups$resource, "[[", "last_event")
  last_event <- .date_helper(purrr::map_dbl(last, "time", .default = 0))
  last_event <- as.Date(last_event)
  days_since_last_event  <- as.integer(Sys.Date() - last_event)
  
  # add a full urlname, past_events and upcoming_events as another column
   r_groups$fullurl <- paste0("https://www.meetup.com/", r_groups$urlname, "/")
   r_groups$url <- paste0("<a href='", r_groups$fullurl, "'>", r_groups$name, "</a>") 
   r_groups$past_events <- past_event_counts
   r_groups$upcoming_events <- upcoming_event_counts
  r_groups$last_event <- last_event
  r_groups$days_since_last_event <- days_since_last_event
  # specify columns to retain
  col_to_keep <- c("name", "city", "country",  "timezone", "members", "fullurl", "created", "past_events", "upcoming_events")
  r_groups2 <- r_groups[col_to_keep]
  write.csv(r_groups2, "docs/data/rugs.csv")   
  
}

get_rugs()
