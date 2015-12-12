#' Fetch data from the 2010-2014 American Community Survey
#'
#' Lightweight interface to the US Census Bureau's API for the 2010-2014 American Community Survey.
#'
#' @param api_key The Census API key of the user; can be set beforehand with the set_api_key function.
#' @param geography The geography for which the user would like to request data.  Available geographies include 'us', 'region', 'division', 'state', 'county', 'tract', and 'block group'.  Defaults to 'us'.
#' @param variable A character string or vector of character strings representing the variable name(s) for which the user would like to request data.  Defaults to 'B01001_001E', the estimate for Total Population.
#' @param state The state for which data will be requested.  Defaults to NULL.
#' @param county The county for which data will be requested.  Defaults to NULL.
#'
#' @export
acs14 <- function(api_key = NULL, geography = 'us', variable = 'B01001_001E', state = NULL, county = NULL) {

  if (!(geography %in% c('us', 'region', 'division', 'state', 'county', 'tract', 'block group'))) {

    stop("Unsupported geography.  Currently available geographies in the acs14lite package are 'us', 'region', 'division', 'state', 'county', 'tract', and 'block group'.  ")

  }

  if (Sys.getenv('ACS14_API') != '') {

    api_key <- Sys.getenv('ACS14_API')

  } else if (is.null(api_key)) {

    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `set_api_key` function to use it throughout your acs14lite session.')

  }

  if (!is.null(state)) {

    if (state != '*') {

      state <- validate_state(state)

    }

    if (length(state) > 1) {

      stop("This package does not yet support multiple states.  To obtain all states, leave the `state` argument blank.",
           call. = FALSE)

    }

  }

  if (!is.null(county)) {

    if (county[1] != '*') {

      county <- sapply(county, function(x) validate_county(state, x))

      }

    if (length(county) > 1) {

        stop('This package does not yet support multiple counties.  To obtain all counties, leave the `county` argument blank.')

      }

  }

  if (length(variable) > 1) {

    variable <- paste(variable, sep = '', collapse = ',')

  }

  if (geography %in% c('us', 'region', 'division')) {

    call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                   variable,
                   '&for=',
                   geography,
                   ':*&key=',
                   api_key)

    return(load_data(call))

  }

  if (geography == 'state') {

    if (is.null(state)) {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     variable,
                     '&for=',
                     geography,
                     ':*&key=',
                     api_key)

      return(load_data(call))

    } else {

      call <- paste0('http://api.census.gov/data/2014/acs5?get=NAME,',
                     variable,
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
                     variable,
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
                     variable,
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
                     variable,
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
                     variable,
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
                     variable,
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
                     variable,
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
                     variable,
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
                     variable,
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

#' Set the Census API key
#'
#' @export
set_api_key <- function(api_key) {

  Sys.setenv(ACS14_API = api_key)

}
