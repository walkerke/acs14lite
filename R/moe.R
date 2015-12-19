#' Calculate the margin of error for a derived sum
#'
#' @param vec A vector of numbers that represent margins of error, or column names from a data frame that represent margins of error for estimates in that data frame.
#' @param df The data frame on which to perform the calculation, if applicable.
#' @export
moe_sum <- function(vec, df = NULL) {

  if (is.numeric(vec)) { # a single vector of numbers

    s <- sapply(vec, function(x) x^2)

    result <- sqrt(sum(s, na.rm = TRUE))

    return(result)

  } else if (is.character(vec)) { # assuming here data frame columns

    internal_fun <- function(values) {

      s <- sapply(values, function(x) x^2)

      result <- sqrt(sum(s, na.rm = TRUE))

      return(result)

    }

    sub <- df[,vec]

    result <- apply(sub, 1, function(x) internal_fun(x))

    return(as.vector(result))

  }

}

#' Calculate the margin of error for a derived proportion
#'
#' @export
moe_prop <- function(num, denom, moe_num, moe_denom) {

  prop <- num / denom

  x <- moe_num^2 - (prop^2 * moe_denom^2)

  result <- sqrt(x) / denom

  return(result)
}


#' Calculate the margin of error for a derived ratio
#'
#' @export
moe_ratio <- function(num, denom, moe_num, moe_denom) {

  r2 <- (num / denom)^2

  mn2 <- moe_num^2

  md2 <- moe_denom^2

  result <- (sqrt(mn2 + (r2 * md2))) / denom

  return(result)

}

#' Calculate the margin of error for a derived product
#'
#' @export
moe_product <- function(a, b, moe_a, moe_b) {

  p1 <- (a^2 * moe_b^2)

  p2 <- (b^2 * moe_a^2)

  result <- sqrt(p1 + p2)

  return(result)

}












