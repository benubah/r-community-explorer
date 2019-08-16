library(httr)
library(jsonlite)

# Wrapper for messages, spotted in googlesheets3
spf <- function(...) stop(sprintf(...), call. = FALSE)
#internals.R from rladies/meetupr package
# This helper function makes a single call, given the full API endpoint URL
# Used as the workhorse function inside .fetch_results() below
.quick_fetch <- function(api_url,
                         event_status = NULL,
                         offset = 0,
                         api_key = NULL,
                         ...) {
  
  # list of parameters
  parameters <- list(status = event_status, # you need to add the status
                     # otherwise it will get only the upcoming event
                     offset = offset,
                     ...                    # other parameters
  )
  # Only need API keys if OAuth is disabled...
  if (!getOption("meetupr.use_oauth")) {
    parameters <- append(parameters, list(key = get_api_key()))
  }
  
  req <- httr::GET(url = api_url,          # the endpoint
                   query = parameters,
                   config = meetup_token()
  )
  
  httr::stop_for_status(req)
  reslist <- httr::content(req, "parsed")
  
  if (length(reslist) == 0) {
    #stop("Zero records match your filter. Nothing to return.\n",
    #     call. = FALSE)
    return(list(result = list(), headers = req$headers))
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


#-------------------------------------------------

##---------------------------------------
# updated get_events() function to retrieve the optional field `event_hosts`

get_events <- function(urlname, event_status = "upcoming", fields = NULL, api_key = NULL) {
  if (!is.null(event_status) &&
      !event_status %in% c("cancelled", "draft", "past", "proposed", "suggested", "upcoming")) {
    stop(sprintf("Event status %s not allowed", event_status))
  }
  # If event_status contains multiple statuses, we can pass along a comma sep list
  if (length(event_status) > 1) {
    event_status <- paste(event_status, collapse = ",")
  }
  api_method <- paste0(urlname, "/events")
  res <- .fetch_results(api_method, api_key, event_status, fields = fields)
  tibble::tibble(
    id = purrr::map_chr(res, "id"),  #this is returned as chr (not int)
    name = purrr::map_chr(res, "name"),
    created = .date_helper(purrr::map_dbl(res, "created")),
    status = purrr::map_chr(res, "status"),
    time = .date_helper(purrr::map_dbl(res, "time")),
    local_date = as.Date(purrr::map_chr(res, "local_date")),
    local_time = purrr::map_chr(res, "local_time", .null = NA),
    # TO DO: Add a local_datetime combining the two above?
    waitlist_count = purrr::map_int(res, "waitlist_count"),
    yes_rsvp_count = purrr::map_int(res, "yes_rsvp_count"),
    venue_id = purrr::map_int(res, c("venue", "id"), .null = NA),
    venue_name = purrr::map_chr(res, c("venue", "name"), .null = NA),
    venue_lat = purrr::map_dbl(res, c("venue", "lat"), .null = NA),
    venue_lon = purrr::map_dbl(res, c("venue", "lon"), .null = NA),
    venue_address_1 = purrr::map_chr(res, c("venue", "address_1"), .null = NA),
    venue_city = purrr::map_chr(res, c("venue", "city"), .null = NA),
    venue_state = purrr::map_chr(res, c("venue", "state"), .null = NA),
    venue_zip = purrr::map_chr(res, c("venue", "zip"), .null = NA),
    venue_country = purrr::map_chr(res, c("venue", "country"), .null = NA),
    description = purrr::map_chr(res, c("description"), .null = NA),
    link = purrr::map_chr(res, c("link")),
    #added because of error when res is null
    resource = res
  )
}


##---------------------------------------------------------------------------------------
# new function, get_hosts() is used to extract `event_hosts` data only out of a complete event's data

get_hosts <- function(urlname, event_status = "past", fields = NULL, api_key = NULL) {
  if (!is.null(event_status) &&
      !event_status %in% c("cancelled", "draft", "past", "proposed", "suggested", "upcoming")) {
    stop(sprintf("Event status %s not allowed", event_status))
  }
  # If event_status contains multiple statuses, we can pass along a comma sep list
  if (length(event_status) > 1) {
    event_status <- paste(event_status, collapse = ",")
  }
  api_method <- paste0(urlname, "/events")
  res <- .fetch_results(api_method, api_key, event_status, fields = fields)
  #return(res)
  return(lapply(res, "[[", "event_hosts"))
}


##-----------------------------------------------------------------------------------------------
# 

get_rladies_hosts <- function(){

# To retrieve all R-Ladies groups urlname so that we can use their urlnames to get their events using lapply
meetup_api_key <- ""# Sys.getenv("MEETUP_KEY")

# retrieve all groups
all_rladies_groups <- find_groups(text = "r-ladies", fields = "past_event_count, upcoming_event_count", api_key = meetup_api_key)

# Cleanup
rladies_groups <- all_rladies_groups[grep(pattern = "rladies|r-ladies|r ladies",  x = all_rladies_groups$name, ignore.case = TRUE), ]

past_event_counts <- purrr::map_dbl(rladies_groups$resource, "past_event_count", .default = 0)
upcoming_event_counts <- purrr::map_dbl(rladies_groups$resource, "upcoming_event_count", .default = 0)

# add a full urlname, past_events and upcoming_events as another column
rladies_groups$fullurl <- paste0("https://www.meetup.com/", rladies_groups$urlname, "/")
rladies_groups$past_events <- past_event_counts
rladies_groups$upcoming_events <- upcoming_event_counts

# order groups in descending order by past_events column, note top 10 groups
rladies_groups <- rladies_groups[order(-rladies_groups$past_events),]

#remove groups for which event_hosts are not made public, and remove groups with 0 events
restricted_groups <- c("rladies-natal", "rladies-xalapa")
rladies_groups <- rladies_groups[!(rladies_groups$past_events==0 | rladies_groups$urlname==restricted_groups),]

urlnames <- rladies_groups$urlname 

#use lapply to call the get_hosts() function for all urlnames
all_past_events <- lapply(urlnames[1:30], get_hosts, event_status = "past", fields = "event_hosts", api_key = meetup_api_key)

hostnames  <- c()
for (i in 1:length(all_past_events)) {
  for (j in 1:length( all_past_events[[i]] )) {
    for (k in 1:length( all_past_events[[i]][[j]] )) {
      hostnames <- c(all_past_events[[i]][[j]][[k]][["name"]], hostnames)
    }
  }
}

hostcount <- c()
for (i in 1:length(all_past_events)) {
  for (j in 1:length( all_past_events[[i]] )) {
    for (k in 1:length( all_past_events[[i]][[j]] )) {
      hostcount <- c(all_past_events[[i]][[j]][[k]][["host_count"]], hostcount)
    }
  }
}
hostsdf <- data.frame(hostnames, hostcount)

# remove duplicate records
uniqhosts = hostsdf[!duplicated(hostsdf$hostnames), ]

# order the records in descending order to see top hosts
uniqhosts <- uniqhosts[order(-uniqhosts$hostcount), ]

write.csv(uniqhosts, "docs/data/rladies_hosts.csv")   
}


# Credit to Jenny Bryan and the Googlesheets3 package for this pattern of
# OAuth handling, see https://github.com/jennybc/googlesheets/blob/master/R/gs_auth.R
#
# environment to store credentials
.state <- new.env(parent = emptyenv())

#' Authorize \code{meetupr}
#'
#' Authorize \code{meetupr} via the OAuth API. You will be directed to a web
#' browser, asked to sign in to your Meetup account, and to grant \code{meetupr}
#' permission to operate on your behalf. By default, these user credentials are
#' cached in a file named \code{.httr-oauth} in the current working directory,
#' from where they can be automatically refreshed, as necessary.
#'
#' Most users, most of the time, do not need to call this function explicitly --
#' it will be triggered by the first action that requires authorization. Even
#' when called, the default arguments will often suffice. However, when
#' necessary, this function allows the user to
#'
#' \itemize{
#'   \item TODO: force the creation of a new token
#'   \item TODO: retrieve current token as an object, for possible storage to an
#'   \code{.rds} file
#'   \item TODO: read the token from an object or from an \code{.rds} file
#'   \item TODO: provide your own app key and secret -- this requires setting up
#'   a new project in \href{https://console.developers.google.com}{Google Developers Console}
#'   \item TODO: prevent caching of credentials in \code{.httr-oauth}
#' }
#'
#' In a direct call to \code{meetup_auth}, the user can provide the token, app
#' key and secret explicitly and can dictate whether interactively-obtained
#' credentials will be cached in \code{.httr_oauth}. If unspecified, these
#' arguments are controlled via options, which, if undefined at the time
#' \code{meetupr} is loaded, are defined like so:
#'
#' \describe{
#'   \item{key}{Set to option \code{meetupr.client_id}, which defaults to a
#'   client ID that ships with the package}
#'   \item{secret}{Set to option \code{meetupr.client_secret}, which defaults to
#'   a client secret that ships with the package}
#'   \item{cache}{Set to option \code{meetupr.httr_oauth_cache}, which defaults
#'   to \code{TRUE}}
#' }
#'
#' To override these defaults in persistent way, predefine one or more of them
#' with lines like this in a \code{.Rprofile} file:
#' \preformatted{
#' options(meetupr.client_id = "FOO",
#'         meetupr.client_secret = "BAR",
#'         meetupr.httr_oauth_cache = FALSE)
#' }
#' See \code{\link[base]{Startup}} for possible locations for this file and the
#' implications thereof.
#'
#' More detail is available from
#' \href{https://www.meetup.com/meetup_api/auth/#oauth2-resources}{Authenticating
#' with the Meetup API}.
#'
#' @param token optional; an actual token object or the path to a valid token
#'   stored as an \code{.rds} file
#' @param new_user logical, defaults to \code{FALSE}. Set to \code{TRUE} if you
#'   want to wipe the slate clean and re-authenticate with the same or different
#'   Google account. This disables the \code{.httr-oauth} file in current
#'   working directory.
#' @param key,secret the "Client ID" and "Client secret" for the application;
#'   defaults to the ID and secret built into the \code{googlesheets} package
#' @param cache logical indicating if \code{googlesheets} should cache
#'   credentials in the default cache file \code{.httr-oauth}
#' @param verbose logical; do you want informative messages?
#'
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' gs_auth()
#'
#' ## store token in an object and then to file
#' ttt <- gs_auth()
#' saveRDS(ttt, "ttt.rds")
#'
#' ## load a pre-existing token
#' gs_auth(token = ttt)       # from an object
#' gs_auth(token = "ttt.rds") # from .rds file
#' }
meetup_auth <- function(token = NULL,
                        new_user = FALSE,
                        key = getOption("meetupr.client_id"),
                        secret = getOption("meetupr.client_secret"),
                        cache = getOption("meetupr.httr_oauth_cache"),
                        verbose = TRUE) {
  
  if (new_user) {
    meetup_deauth(clear_cache = TRUE, verbose = verbose)
  }
  
  token = readRDS("meetup_token.rds")
  
  if (is.null(token)) {
    
    message(paste0('Meetup is moving to OAuth *only* as of 2019-08-15. Set\n',
                   '`meetupr.use_oauth = FALSE` in your .Rprofile, to use\nthe ',
                   'legacy `api_key` authorization.'))
    
    meetup_app       <- httr::oauth_app("meetup", key = key, secret = secret)
    meetup_endpoints <- httr::oauth_endpoint(
      authorize = 'https://secure.meetup.com/oauth2/authorize',
      access    = 'https://secure.meetup.com/oauth2/access'
    )
    meetup_token <- httr::oauth2.0_token(meetup_endpoints, meetup_app,
                                         cache = cache)
    
    stopifnot(is_legit_token(meetup_token, verbose = TRUE))
    .state$token <- meetup_token
    
  } else if (inherits(token, "Token2.0")) {
    
    stopifnot(is_legit_token(token, verbose = TRUE))
    .state$token <- token
    
  } else if (inherits(token, "character")) {
    
    meetup_token <- try(suppressWarnings(readRDS(token)), silent = TRUE)
    if (inherits(meetup_token, "try-error")) {
      spf("Cannot read token from alleged .rds file:\n%s", token)
    } else if (!is_legit_token(meetup_token, verbose = TRUE)) {
      spf("File does not contain a proper token:\n%s", token)
    }
    .state$token <- meetup_token
    
  } else {
    spf(paste0("Input provided via 'token' is neither a token,\n",
               "nor a path to an .rds file containing a token."))
  }
  
  invisible(.state$token)
  
}

#' Produce Meetup token
#'
#' If token is not already available, call \code{\link{meetup_auth}} to either
#' load from cache or initiate OAuth2.0 flow. Return the token -- not "bare"
#' but, rather, prepared for inclusion in downstream requests.
#'
#' @return a \code{request} object (an S3 class provided by \code{httr})
#'
#' @keywords internal
meetup_token <- function(verbose = FALSE) {
  if (getOption("meetupr.use_oauth")) {
    if (!token_available(verbose = verbose)) meetup_auth(verbose = verbose)
    httr::config(token = .state$token)
  } else {
    httr::config()
  }
}

#' Check token availability
#'
#' Check if a token is available in \code{\link{meetupr}}'s internal
#' \code{.state} environment.
#'
#' @return logical
#'
#' @keywords internal
token_available <- function(verbose = TRUE) {
  
  if (is.null(.state$token)) {
    if (verbose) {
      if (file.exists(".httr-oauth")) {
        message("A .httr-oauth file exists in current working ",
                "directory.\nWhen/if needed, the credentials cached in ",
                ".httr-oauth will be used for this session.\nOr run ",
                "meetup_auth() for explicit authentication and authorization.")
      } else {
        message("No .httr-oauth file exists in current working directory.\n",
                "When/if needed, 'meetupr' will initiate authentication ",
                "and authorization.\nOr run meetup_auth() to trigger this ",
                "explicitly.")
      }
    }
    return(FALSE)
  }
  
  TRUE
  
}

#' Suspend authorization
#'
#' Suspend \code{\link{meetupr}}'s authorization to place requests to the Meetup
#' APIs on behalf of the authenticated user.
#'
#' @param clear_cache logical indicating whether to disable the
#'   \code{.httr-oauth} file in working directory, if such exists, by renaming
#'   to \code{.httr-oauth-SUSPENDED}
#' @param verbose logical; do you want informative messages?
#' @export
#' @family auth functions
#' @examples
#' \dontrun{
#' gs_deauth()
#' }
meetup_deauth <- function(clear_cache = TRUE, verbose = TRUE) {
  
  if (clear_cache && file.exists(".httr-oauth")) {
    if (verbose) {
      message("Disabling .httr-oauth by renaming to .httr-oauth-SUSPENDED")
    }
    file.rename(".httr-oauth", ".httr-oauth-SUSPENDED")
  }
  
  if (token_available(verbose = FALSE)) {
    if (verbose) {
      message("Removing google token stashed internally in 'meetupr'.")
    }
    rm("token", envir = .state)
  } else {
    message("No token currently in force.")
  }
  
  invisible(NULL)
  
}

#' Check that token appears to be legitimate
#'
#' @keywords internal
is_legit_token <- function(x, verbose = FALSE) {
  
  if (!inherits(x, "Token2.0")) {
    if (verbose) message("Not a Token2.0 object.")
    return(FALSE)
  }
  
  if ("invalid_client" %in% unlist(x$credentials)) {
    # shouldn't happen if id and secret are good
    if (verbose) {
      message("Authorization error. Please check client_id and client_secret.")
    }
    return(FALSE)
  }
  
  if ("invalid_request" %in% unlist(x$credentials)) {
    # in past, this could happen if user clicks "Cancel" or "Deny" instead of
    # "Accept" when OAuth2 flow kicks to browser ... but httr now catches this
    if (verbose) message("Authorization error. No access token obtained.")
    return(FALSE)
  }
  
  TRUE
  
}


#' Store a legacy API key in the .state environment
#'
#' @keywords internal
set_api_key <- function(x = NULL) {
  
  if (is.null(x)) {
    key <- Sys.getenv("MEETUP_KEY")
    if (key == "") {
      spf(paste0("You have not set a MEETUP_KEY environment variable.\nIf you ",
                 "do not yet have a meetup.com API key, use OAuth2\ninstead, ",
                 "as API keys are now deprecated - see here:\n",
                 "* https://www.meetup.com/meetup_api/auth/"))
    }
    .state$legacy_api_key <- key
  } else {
    .state$legacy_api_key <- x
  }
  
  invisible(NULL)
  
}

#' Get the legacy API key from the .state environment
#'
#' @keywords internal
get_api_key <- function() {
  
  if (is.null(.state$legacy_api_key)) {
    set_api_key()
  }
  
  .state$legacy_api_key
  
}


options(meetupr.httr_oauth_cache=TRUE)
options(meetupr.use_oauth = TRUE)
get_rladies_hosts()
