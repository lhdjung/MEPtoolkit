#' Results Container for Range-Based Robustness Analysis
#'
#' @description
#' An R6 class that stores results from \code{\link{range_killer}}, which
#' assesses the sensitivity of reported descriptive statistics to removal of
#' extreme values. The container holds bounds on the mean and standard deviation
#' after removing the minimum and maximum observations, accounting for rounding
#' uncertainty in the reported values.
#'
#' @details
#' Each field contains numeric vectors where each element corresponds to one
#' input case. The results follow the standard format of \code{c(min, RIVETS, max)}
#' for both the mean and standard deviation after removing extreme values.
#'
#' When recalculated statistics are impossible (e.g., negative variance),
#' the SD fields contain \code{-1} as a flag value rather than a valid statistic.
#' This typically indicates inconsistency in the original reported values.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{Min_mean}}{Numeric vector. Lower bound on the sample mean after
#'     removing one minimum and one maximum value from the original sample}
#'   \item{\code{RIVETS_mean}}{Numeric vector. Mean recalculated using the
#'     reported (central) values, with extremes removed}
#'   \item{\code{Max_mean}}{Numeric vector. Upper bound on the sample mean after
#'     removing one minimum and one maximum value from the original sample}
#'   \item{\code{Min_sd}}{Numeric vector. Lower bound on the sample standard
#'     deviation after removing extremes. Set to \code{-1} when the resulting
#'     variance is negative (indicating an impossible/inconsistent combination)}
#'   \item{\code{RIVETS_sd}}{Numeric vector. SD recalculated using the reported
#'     (central) values, with extremes removed. Set to \code{-1} when impossible}
#'   \item{\code{Max_sd}}{Numeric vector. Upper bound on the sample standard
#'     deviation after removing extremes. Set to \code{-1} when impossible}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(Min_mean = NULL, RIVETS_mean = NULL, Max_mean = NULL,
#'     Min_sd = NULL, RIVETS_sd = NULL, Max_sd = NULL)}}{Constructor that
#'     initializes a new \code{Range_killer_res} object. All arguments are
#'     optional and default to \code{NULL}}
#'   \item{\code{print(n = NULL)}}{Print results to console. If \code{n} is
#'     \code{NULL} (default), prints all cases; if \code{n} is an integer or
#'     vector, prints only the specified case(s). Impossible SD values are
#'     displayed as "Impossible"}
#' }
#'
#' @examples
#' # Typical usage: returned automatically by range_killer()
#' result <- range_killer(
#'   m = "10.5", s = "2.3", n = 20,
#'   min = "5.2", max = "15.8"
#' )
#' print(result)
#'
#' # Manual construction (rarely needed)
#' rk <- Range_killer_res$new(
#'   Min_mean = 10.2,
#'   RIVETS_mean = 10.5,
#'   Max_mean = 10.8,
#'   Min_sd = 1.9,
#'   RIVETS_sd = 2.1,
#'   Max_sd = 2.3
#' )
#'
#' @seealso
#'   \code{\link{range_killer}} for generating these results
#'
#' @name Range_killer_res
#' @docType class
#' @keywords classes
#' @export
Range_killer_res <- R6::R6Class("Range_killer_res",
                                public = list(
                                  #' @field Min_mean Numeric vector. Lower bound on the sample mean after
                                  #'   removing the observed minimum and maximum values (worst-case minimum).
                                  Min_mean = NULL,
                                  #' @field RIVETS_mean Numeric vector. Mean recalculated using the original
                                  #'   reported values (the "reported" or RIVETS case).
                                  RIVETS_mean = NULL,
                                  #' @field Max_mean Numeric vector. Upper bound on the sample mean after
                                  #'   removing the observed minimum and maximum values (worst-case maximum).
                                  Max_mean = NULL,
                                  #' @field Min_sd Numeric vector. Lower bound on the sample standard
                                  #'   deviation after removing the observed minimum and maximum values
                                  #'   (or \code{-1} for impossible / inconsistent combinations).
                                  Min_sd = NULL,
                                  #' @field RIVETS_sd Numeric vector. SD recalculated using the original
                                  #'   reported values (or \code{-1} if the recalculated variance is negative).
                                  RIVETS_sd = NULL,
                                  #' @field Max_sd Numeric vector. Upper bound on the sample standard
                                  #'   deviation after removing the observed minimum and maximum values
                                  #'   (or \code{-1} for impossible / inconsistent combinations).
                                  Max_sd = NULL,
                                  #' @description Create a new \code{Range_killer_res} object.
                                  #' @param Min_mean Numeric vector for minimum mean values
                                  #' @param RIVETS_mean Numeric vector for reported mean values
                                  #' @param Max_mean Numeric vector for maximum mean values
                                  #' @param Min_sd Numeric vector for minimum SD values
                                  #' @param RIVETS_sd Numeric vector for reported SD values
                                  #' @param Max_sd Numeric vector for maximum SD values
                                  #' @return A new \code{Range_killer_res} object.
                                  initialize = function(Min_mean = NULL,
                                                        RIVETS_mean = NULL,
                                                        Max_mean = NULL,
                                                        Min_sd = NULL,
                                                        RIVETS_sd = NULL,
                                                        Max_sd = NULL) {
                                    self$Min_mean <- Min_mean
                                    self$RIVETS_mean <- RIVETS_mean
                                    self$Max_mean <- Max_mean
                                    self$Min_sd <- Min_sd
                                    self$RIVETS_sd <- RIVETS_sd
                                    self$Max_sd <- Max_sd
                                  },
                                  #' @description Print a human-readable summary for one or more cases.
                                  #'   By default prints all cases. Passing \code{n} as an integer vector
                                  #'   selects which cases to display.
                                  #' @param n Integer or vector specifying which case(s) to display.
                                  #'   Default is \code{NULL} (all cases)
                                  print = function(n=NULL) {
                                    cat("Results from range_killer\n")
                                    if(is.null(n) || n>length(self$RIVETS_mean) || n<=0)
                                      n <- 1:(length(self$RIVETS_mean))
                                    for(i in n) {
                                      cat("\n\n--- Line", i,"---\n")
                                      cat("---------------------------------------\n")
                                      cat("Minimum mean: ", self$Min_mean[i], "\n")
                                      cat("RIVETS mean:  ", self$RIVETS_mean[i], "\n")
                                      cat("Maximum mean: ", self$Max_mean[i], "\n")
                                      cat("Minimum sd: ", if(self$Min_sd[i] <0) "Impossible" else self$Min_sd[i], "\n")
                                      cat("RIVETS sd:  ", if(self$RIVETS_sd[i] <0) "Impossible" else self$RIVETS_sd[i], "\n")
                                      cat("Maximum sd: ", if(self$Max_sd[i] <0) "Impossible" else self$Max_sd[i], "\n")
                                      cat("---------------------------------------\n")
                                    }
                                  }
                                )
)

#' Assess Robustness of Descriptive Statistics to Removal of Extreme Values
#'
#' @description
#' Evaluates the sensitivity of reported summary statistics (mean and standard
#' deviation) to the removal of extreme observations. This function computes
#' bounds on what the mean and SD would be if the minimum and maximum values
#' were removed from the sample, accounting for rounding uncertainty in all
#' reported statistics. Useful for detecting reporting errors or assessing the
#' influence of outliers on published results.
#'
#' @param m Character scalar or vector. Reported sample mean(s). Can be formatted
#'   numbers (e.g., \code{"10.5"}) that may represent rounded values
#' @param s Character scalar or vector. Reported sample standard deviation(s)
#'   (same format as \code{m})
#' @param n Numeric scalar or vector. Sample size(s) (integers ≥ 4). Must be at
#'   least 4 because the analysis removes 2 observations and requires degrees of
#'   freedom for SD calculation
#' @param min Character scalar or vector. Reported minimum value(s) in the sample
#'   (same format as \code{m})
#' @param max Character scalar or vector. Reported maximum value(s) in the sample
#'   (same format as \code{m})
#' @param data Data frame, optional. If provided, other arguments are interpreted
#'   as column names (quoted or unquoted) within \code{data}
#' @param output Logical. If \code{TRUE}, prints the result before returning.
#'   Default is \code{FALSE}
#'
#' @return An object of class \code{\link{Range_killer_res}} containing six
#'   numeric vectors:
#'   \describe{
#'     \item{Min_mean}{Minimum possible mean after removing extremes}
#'     \item{RIVETS_mean}{Mean recalculated from reported (central) values}
#'     \item{Max_mean}{Maximum possible mean after removing extremes}
#'     \item{Min_sd}{Minimum possible SD after removing extremes (or \code{-1}
#'       if impossible)}
#'     \item{RIVETS_sd}{SD recalculated from reported (central) values (or
#'       \code{-1} if impossible)}
#'     \item{Max_sd}{Maximum possible SD after removing extremes (or \code{-1}
#'       if impossible)}
#'   }
#'
#' @details
#' The function performs the following steps for each case:
#' \enumerate{
#'   \item Converts each reported statistic (mean, SD, min, max) into a plausible
#'     interval using \code{\link{validate_descriptive}}, which accounts for
#'     rounding precision
#'   \item Generates all combinations of minimum/original/maximum values for the
#'     four statistics (3^4 = 81 combinations per case)
#'   \item For each combination, recalculates the mean and SD that would result
#'     if the minimum and maximum observations were removed from the sample
#'   \item Records the minimum and maximum recalculated values across all
#'     combinations, plus the values computed from the reported (central)
#'     statistics
#' }
#'
#' The recalculations use standard algebraic identities:
#' \itemize{
#'   \item \strong{New mean}: \eqn{\bar{x}' = \frac{n\bar{x} - (x_{min} + x_{max})}{n - 2}}
#'   \item \strong{Original sum of squares}: \eqn{SS = (n-1)s^2 + n\bar{x}^2}
#'   \item \strong{New sum of squares}: \eqn{SS' = SS - (x_{min}^2 + x_{max}^2)}
#'   \item \strong{New variance}: \eqn{s'^2 = \frac{SS' - (n-2)\bar{x}'^2}{n-3}}
#'   \item \strong{New SD}: \eqn{s' = \sqrt{s'^2}} (or \code{-1} if \eqn{s'^2 < 0})
#' }
#'
#' When the recalculated variance is negative, this indicates an impossible or
#' inconsistent combination of statistics, and the SD is set to \code{-1} as a
#' flag value.
#'
#' @section Use Cases:
#' \itemize{
#'   \item \strong{Error detection}: If recalculated statistics are impossible
#'     (SD = \code{-1}), the original reported values may be inconsistent or
#'     contain errors
#'   \item \strong{Outlier influence}: Large differences between original and
#'     trimmed statistics suggest strong influence of extreme values
#'   \item \strong{Robustness assessment}: Narrow ranges around RIVETS values
#'     indicate statistics that are relatively stable to removal of extremes
#'   \item \strong{Reporting verification}: Can help verify whether reported
#'     ranges (min/max) are plausible given reported mean and SD
#' }
#'
#' @section Assumptions and Limitations:
#' \itemize{
#'   \item Sample size must be at least 4 (n ≥ 4) to allow removal of 2
#'     observations while maintaining sufficient degrees of freedom (n-3 ≥ 1)
#'   \item The function assumes the reported minimum and maximum represent actual
#'     observed values, not theoretical or truncated ranges
#'   \item Very small samples (n = 4 or 5) may be highly sensitive to rounding,
#'     producing wide bounds
#'   \item The \code{-1} flag for impossible SDs is a sentinel value; do not
#'     interpret it as a meaningful statistic
#' }
#'
#' @examples
#' # Single case: assess robustness of reported statistics
#' result <- range_killer(
#'   m = "10.5", s = "2.3", n = 20,
#'   min = "5.2", max = "15.8",
#'   output = TRUE
#' )
#'
#' # Multiple cases from a data frame
#' df <- data.frame(
#'   mean = c("10.5", "8.2"),
#'   sd = c("2.3", "1.9"),
#'   sample_size = c(20, 25),
#'   minimum = c("5.2", "4.1"),
#'   maximum = c("15.8", "12.3")
#' )
#' result <- range_killer(
#'   m = mean, s = sd, n = sample_size,
#'   min = minimum, max = maximum,
#'   data = df
#' )
#' print(result)
#'
#' # Check for impossible statistics (inconsistent reporting)
#' suspicious <- range_killer(
#'   m = "10.0", s = "0.5", n = 10,
#'   min = "5.0", max = "15.0"  # Range too wide for this mean/SD
#' )
#' # If RIVETS_sd is -1, the reported values are inconsistent
#'
#' @seealso
#'   \code{\link{Range_killer_res}} for the result object structure,
#'   \code{\link{validate_descriptive}} for the validation function
#'
#' @importFrom rlang enquo
#' @keywords robust
#' @export
range_killer <- function(m, s, n, min, max, data = NULL, output = FALSE) {
  # Input validation
  if(!is.null(data)) {
    if(!is.data.frame(data)) stop("`data` must be a data.frame.")
    m <- resolve(enquo(m), data); s <- resolve(enquo(s), data);
    min <- resolve(enquo(min), data); max <- resolve(enquo(max), data);
    n <- resolve(enquo(n), data);
  }
  L <- length_check(list(m=m, s=s, min=min, max=max, n=n))
  m <- validate_descriptive(m)
  s <- validate_descriptive(s)
  min <- validate_descriptive(min)
  max <- validate_descriptive(max)
  res_m_min <- c()
  res_m_max <- c()
  res_s_min <- c()
  res_s_max <- c()
  res_m_RIVETS <- c()
  res_s_RIVETS <- c()

  for (j in seq_len(L)) {
    m_range <- c(m$minimum[j], m$original[j], m$maximum[j])
    s_range <- c(s$minimum[j], s$original[j], s$maximum[j])
    min_range <- c(min$minimum[j], min$original[j], min$maximum[j])
    max_range <- c(max$minimum[j], max$original[j], max$maximum[j])
    m_grid <- c()
    s_grid <- c()

    cases_grid <- expand.grid(m = m_range, s = s_range, min = min_range, max = max_range)

    for(i in seq_len(nrow(cases_grid)))
    {
      # our new mean is (sum of values minus min&max) / (sample size minus two)
      m_prime <- (n[j] * cases_grid[i, "m"] - (cases_grid[i, "min"] + cases_grid[i, "max"])) / (n[j] - 2)

      # easiest way to explain this is sum of squares, let's get the original one
      SS <- (n[j] - 1) * cases_grid[i, "s"]^2 + n[j] * cases_grid[i, "m"]^2

      # and we just remove the squared values
      SSprime <- SS - (cases_grid[i, "min"]^2 + cases_grid[i, "max"]^2)

      # variance  is now...
      var_prime <- (SSprime-(n[j]-2)*m_prime^2) / (n[j]-3)

      s_prime <- if(var_prime>=0) sqrt(var_prime) else -1

      m_grid <- c(m_grid, m_prime)
      s_grid <- c(s_grid, s_prime)
    }

    res_m_min <- c(res_m_min, min(m_grid))
    res_m_max <- c(res_m_max, max(m_grid))
    res_s_min <- c(res_s_min, min(s_grid))
    res_s_max <- c(res_s_max, max(s_grid))

    # our new mean is (sum of values minus min&max) / (sample size minus two)
    m_prime <- (n[j] * m$original[j] - (min$original[j] + max$original[j])) / (n[j] - 2)

    # easiest way to explain this is sum of squares, let's get the original one
    SS <- (n[j] - 1) * s$original[j]^2 + n[j] * m$original[j]^2

    # and we just remove the squared values
    SSprime <- SS - (min$original[j]^2 + max$original[j]^2)

    # variance  is now...
    var_prime <- (SSprime-(n[j]-2)*m_prime^2) / (n[j]-3)

    s_prime <- if(var_prime>=0) sqrt(var_prime) else -1

    res_m_RIVETS <- c(res_m_RIVETS, m_prime)
    res_s_RIVETS <- c(res_s_RIVETS, s_prime)
  }

  # Return value
  result <- Range_killer_res$new(Min_mean = res_m_min,
                                 RIVETS_mean = res_m_RIVETS,
                                 Max_mean = res_m_max,
                                 Min_sd = res_s_min,
                                 RIVETS_sd = res_s_RIVETS,
                                 Max_sd = res_s_max)

  if(output == TRUE)
  {
    print(result)
  }

  return(result)
}
