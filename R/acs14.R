#' Fetch data from the 2010-2014 American Community Survey
#'
#'
#' @export
acs14 <- function(api_key, geography = 'us', table = 'B01001_001E', state = NULL, county = NULL, tract = NULL) {

  if (Sys.getenv('ACS14_API') != '') {

    api_key <- Sys.getenv('ACS14_API')

  }

  if (!is.null(state)) {

    if (state != '*') {

      state <- validate_state(state)

    }

    if (length(state) > 1) {

      stop("This package does not yet support multiple states.  To obtain all states, leave the `state` parameter blank.",
           call. = FALSE)

    }

  }

  if (!is.null(county)) {

    if (county[1] != '*') {

      county <- sapply(county, function(x) validate_county(state, x))

      }

    if (length(county) > 1) {

        county <- paste(county, sep = '', collapse = ',')

      }

  }

  if (length(table) > 1) {

    table <- paste(table, sep = '', collapse = ',')

  }

  if (geography %in% c('us', 'region', 'division')) {

    call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                   table,
                   '&for=',
                   geography,
                   ':*&key=',
                   api_key)

    return(load_data(call))

  }

  if (geography == 'state') {

    if (is.null(state)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*&key=',
                     api_key)

      return(load_data(call))

    } else {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':',
                     state,
                     '&key=',
                     api_key)

      return(load_data(call))

    }

  }

  if (geography == 'county') {

    # Getting all counties here for the US
    if (is.null(county) & is.null(state)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*&key=',
                     api_key)

      return(load_data(call))

    }

    if (is.null(state) & !is.null(county)) {

      stop("Please supply a valid state for which you'd like to request counties.")

    }

    if (!is.null(state) & is.null(county)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*&in=state:',
                     state,
                     '&key=',
                     api_key)

      return(load_data(call))


    }

    if (!is.null(state) & !is.null(county)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':',
                     county,
                     '&in=state:',
                     state,
                     '&key=',
                     api_key)

      return(load_data(call))

    }

  }

  if (geography == 'tract') {

    # Getting all tracts here for the US
    if (is.null(county) & is.null(state)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*&key=',
                     api_key)

      return(load_data(call))

    }

    if (is.null(state) & !is.null(county)) {

      stop("Please supply a valid state for which you'd like to request tracts.")

    }

    if (!is.null(state) & is.null(county)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*&in=state:',
                     state,
                     '&key=',
                     api_key)

      return(load_data(call))


    }

    if (!is.null(state) & !is.null(county)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*',
                     '&in=state:',
                     state,
                     '+county:',
                     county,
                     '&key=',
                     api_key)

      return(load_data(call))

    }


  }

  if (geography == 'block group') {

    geography <- 'block+group'

    # Getting all tracts here for the US
    if (is.null(county) & is.null(state)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*&key=',
                     api_key)

      return(load_data(call))

    }

    if (is.null(state) & !is.null(county)) {

      stop("Please supply a valid state for which you'd like to request block groups.")

    }

    if (!is.null(state) & is.null(county)) {

      stop("Please supply a valid county for which you'd like to request block groups.")


    }

    if (!is.null(state) & !is.null(county)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     table,
                     '&for=',
                     geography,
                     ':*',
                     '&in=state:',
                     state,
                     '+county:',
                     county,
                     '&key=',
                     api_key)

      return(load_data(call))

    }


  }


}


set_api_key <- function(api_key) {

  Sys.setenv(ACS14_API = api_key)

}
