#' Count Decimal Places in a Numeric String
#'
#' @description
#' Determines the number of decimal places in a character representation of a
#' number. This is used to infer the rounding precision of reported statistics,
#' which is essential for computing plausible value ranges.
#'
#' @param statistic Character scalar or vector. Numeric value(s) represented
#'   as character strings (e.g., \code{"3.14"}, \code{".05"}, \code{"2.00"})
#'
#' @return Integer vector of the same length as \code{statistic}, giving the
#'   number of decimal places in each element. Returns \code{0L} for whole
#'   numbers without a decimal point, and \code{NA_integer_} for \code{NA}
#'   inputs
#'
#' @details
#' The function counts digits after the decimal point, including trailing zeros.
#' For example:
#' \itemize{
#'   \item \code{"3"} → 0 decimal places
#'   \item \code{"3.1"} → 1 decimal place
#'   \item \code{"3.10"} → 2 decimal places (trailing zero counts)
#'   \item \code{".984"} → 3 decimal places
#'   \item \code{NA} → \code{NA_integer_}
#' }
#'
#' This precision information is used by \code{\link{validate_descriptive}} and
#' \code{\link{validate_p}} to construct plausible intervals around rounded
#' values.
#'
#' @examples
#' # Whole number
#' decimal_place("3")
#'
#' # Leading decimal point
#' decimal_place(".984")
#'
#' # Trailing zeros count
#' decimal_place("2.00")
#'
#' # Vector input with NA
#' decimal_place(c("3.14", NA, "6.8"))
#'
#' @seealso
#'   \code{\link{validate_descriptive}} and \code{\link{validate_p}} which
#'   use this function to determine rounding precision
#'
#' @export
decimal_place <- function(statistic) {
  vapply(statistic, FUN.VALUE = integer(1), function(elem) {
    if (is.na(elem)) return(NA_integer_)
    if (grepl("\\.", elem)) {
      return(nchar(strsplit(elem, "\\.")[[1]][2]))
    }
    else
      return(0L)
  }, USE.NAMES = FALSE)
}

#' Validate and Expand Descriptive Statistics to Account for Rounding
#'
#' @description
#' Converts reported descriptive statistics (means, standard deviations, or
#' other continuous measures) into plausible intervals that account for
#' rounding precision. This is a foundational function used throughout the
#' package to handle uncertainty in rounded reported values.
#'
#' @param statistic Character scalar or vector. Reported descriptive statistic(s)
#'   as character strings (e.g., \code{"2.14"}, \code{"10.5"}). Must be
#'   character to allow inference of rounding precision
#' @param allow_na Logical. If \code{TRUE}, permits \code{NA} values in the
#'   input (which produce \code{NA} in all output fields). Default is \code{FALSE}
#'
#' @return A list with three numeric vectors of equal length:
#'   \describe{
#'     \item{minimum}{Lower bound of the plausible interval for each statistic}
#'     \item{original}{The reported (central) value for each statistic}
#'     \item{maximum}{Upper bound of the plausible interval for each statistic}
#'   }
#'
#' @details
#' The function operates as follows:
#' \enumerate{
#'   \item Determines the number of decimal places in the reported value using
#'     \code{\link{decimal_place}}
#'   \item Constructs a plausible interval assuming the value was rounded to
#'     the nearest unit in the last decimal place
#'   \item Uses the interval \eqn{[\text{value} - 0.5 \times 10^{-d},
#'     \text{value} + 0.5 \times 10^{-d}]} where \eqn{d} is the number of
#'     decimal places
#' }
#'
#' For example:
#' \itemize{
#'   \item \code{"2.14"} → [2.135, 2.14, 2.145] (rounded to nearest 0.01)
#'   \item \code{"10.5"} → [10.45, 10.5, 10.55] (rounded to nearest 0.1)
#'   \item \code{"3"} → [2.5, 3.0, 3.5] (rounded to nearest 1)
#' }
#'
#' This interval represents all true values that would round to the reported
#' value under standard rounding rules.
#'
#' @section Use in Package:
#' This function is called internally by:
#' \itemize{
#'   \item \code{\link{t_slicer}} to handle means and standard deviations
#'   \item \code{\link{range_killer}} to handle means, SDs, and min/max values
#'   \item \code{\link{anova_one_descriptive}} to handle group means and SDs
#' }
#'
#' @examples
#' # Single value
#' validate_descriptive("2.14")
#' # Returns: list(minimum = 2.135, original = 2.14, maximum = 2.145)
#'
#' # Multiple values
#' validate_descriptive(c("2.14", "3.2"))
#'
#' # With NA values (requires allow_na = TRUE)
#' validate_descriptive(c("2.14", NA, "3.2"), allow_na = TRUE)
#'
#' # Different precisions
#' validate_descriptive("10")     # Integer: ±0.5
#' validate_descriptive("10.0")   # One decimal: ±0.05
#' validate_descriptive("10.00")  # Two decimals: ±0.005
#'
#' @seealso
#'   \code{\link{validate_p}} for p-value validation with special handling,
#'   \code{\link{decimal_place}} for determining rounding precision,
#'   \code{\link{t_slicer}}, \code{\link{range_killer}}, and
#'   \code{\link{anova_one_descriptive}} which use this function
#'
#' @export
validate_descriptive <- function (statistic, allow_na = FALSE)
{
  name <- substitute(statistic)
  if(typeof(name) != "symbol")
    name <- "statistic"
  if (!is.character(statistic)) {
    stop(paste(name, "must be a character string or character vector."))
  }
  suppressWarnings({
    statistic_num <- as.numeric(statistic)
  })
  if (any(is.na(statistic)) && !allow_na) {
    stop(paste(name, "contains NA; set allow_na = TRUE to permit NA values."))
  }
  if (any(is.na(statistic_num) & !is.na(statistic))) {
    stop(paste(name, "must be parseable as numeric (or NA)."))
  }
  dp <- decimal_place(statistic)
  statistic_range <- list(
    minimum = statistic_num - 5*10^(-dp-1),
    original = statistic_num,
    maximum = statistic_num + 5*10^(-dp-1)
  )
  return(statistic_range)
}

#' Validate and Expand P-Values to Account for Rounding and Truncation
#'
#' @description
#' Converts reported p-values into plausible intervals that account for
#' rounding precision and inequality notation (e.g., \code{"<.001"}). This
#' function handles the special conventions used for reporting p-values in
#' scientific literature.
#'
#' @param p Character scalar or vector. Reported p-value(s) as character
#'   strings. Can include standard decimal notation (e.g., \code{".05"},
#'   \code{"0.123"}) or inequality notation (e.g., \code{"<.001"}, \code{"< 0.05"})
#' @param allow_na Logical. If \code{TRUE}, permits \code{NA} values in the
#'   input (which produce \code{NA} in all output fields). Default is \code{FALSE}
#'
#' @return A list with three numeric vectors of equal length:
#'   \describe{
#'     \item{minimum}{Lower bound of the plausible interval. For \code{"<"}
#'       values, this is effectively 0 (\code{.Machine$double.xmin})}
#'     \item{original}{The reported (central) value. For \code{"<"} values,
#'       this is slightly less than the threshold to avoid the boundary}
#'     \item{maximum}{Upper bound of the plausible interval. For \code{"<"}
#'       values, this is the reported threshold}
#'   }
#'
#' @details
#' The function handles two types of p-value notation:
#'
#' \strong{Standard notation} (e.g., \code{".05"}, \code{"0.123"}):
#' \itemize{
#'   \item Determines decimal precision using \code{\link{decimal_place}}
#'   \item Constructs interval \eqn{[\max(0, p - 0.5 \times 10^{-d}),
#'     \min(1, p + 0.5 \times 10^{-d})]}
#'   \item Example: \code{".05"} → [0.045, 0.05, 0.055]
#' }
#'
#' \strong{Inequality notation} (e.g., \code{"<.001"}, \code{"< 0.05"}):
#' \itemize{
#'   \item Minimum is set to \code{.Machine$double.xmin} (effectively 0)
#'   \item Original is set to slightly below the threshold
#'   \item Maximum is the reported threshold
#'   \item Example: \code{"<.001"} → [~0, ~0.001, 0.001]
#' }
#'
#' The function ensures all values remain in the valid p-value range [0, 1].
#'
#' @section Use in Package:
#' This function is called internally by:
#' \itemize{
#'   \item \code{\link{t_slicer}} to handle reported p-values for consistency
#'     checking
#'   \item \code{\link{carlisle}} to handle p-values for aggregation testing
#' }
#'
#' @examples
#' # Standard p-value notation
#' validate_p(".05")
#' # Returns approximately: list(minimum = 0.045, original = 0.05, maximum = 0.055)
#'
#' # Inequality notation (less than)
#' validate_p("<.001")
#' # Returns approximately: list(minimum = ~0, original = ~0.001, maximum = 0.001)
#'
#' # Vector input with mixed notation
#' validate_p(c(".14", "<.001", "0.58"))
#'
#' # With NA values (requires allow_na = TRUE)
#' validate_p(c(".14", "<.001", NA, ".58"), allow_na = TRUE)
#'
#' # Different precisions
#' validate_p(".1")    # One decimal: ±0.05
#' validate_p(".10")   # Two decimals: ±0.005
#' validate_p(".100")  # Three decimals: ±0.0005
#'
#' @seealso
#'   \code{\link{validate_descriptive}} for general descriptive statistics,
#'   \code{\link{decimal_place}} for determining rounding precision,
#'   \code{\link{t_slicer}} and \code{\link{carlisle}} which use this function
#'
#' @export
validate_p <- function(p, allow_na = FALSE)
{
  name <- substitute(p)
  if(typeof(name) != "symbol")
    name <- "p"
  if (!is.character(p)) {
    stop(paste(name, "must be a character string or character vector."))
  }
  if (any(is.na(p)) && !allow_na) {
    stop(paste(name, "contains NA; set allow_na = TRUE to permit NA values."))
  }
  # Deal with "<.X"
  numeric_parts <- vapply(p, function(elem) {
    if (is.na(elem)) return(NA_character_)
    sub(".*<", "", elem)
  }, FUN.VALUE = character(1L), USE.NAMES = FALSE)
  suppressWarnings({
    numeric_values <- as.numeric(numeric_parts)
  })
  if (any(is.na(numeric_values) & !is.na(numeric_parts))) {
    stop(paste(name, "must be parseable as numeric (or NA)."))
  }
  if(any(numeric_values > 1 | numeric_values < 0, na.rm = TRUE))
  {
    stop(paste(name, "values must be between 0 and 1."))
  }
  dp <- decimal_place(numeric_parts)
  p_range <- list(
    minimum = ifelse(grepl("<", p, fixed = TRUE),
                     .Machine$double.xmin,
                     pmax(0, numeric_values - 5*10^(-dp-1))),
    original = ifelse(grepl("<", p, fixed = TRUE),
                      numeric_values - .Machine$double.xmin,
                      numeric_values),
    maximum = ifelse(grepl("<", p, fixed = TRUE),
                     numeric_values,
                     pmin(1, numeric_values + 5*10^(-dp-1)))
  )
  return(p_range)
}
