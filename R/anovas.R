#' Results Container for One-Way ANOVA Reconstruction
#'
#' @description
#' An R6 class that stores results from ANOVA reconstruction functions (currently
#' \code{\link{anova_one_descriptive}}), which reconstruct omnibus F-test p-values
#' from reported descriptive statistics. The container holds p-value ranges that
#' account for rounding uncertainty in the reported means and standard deviations.
#'
#' @details
#' This object stores reconstructed ANOVA p-values in a three-element format:
#' \itemize{
#'   \item \strong{Min}: Minimum possible p-value across all rounding-consistent
#'     parameter combinations
#'   \item \strong{RIVETS}: P-value computed from the reported (central) values
#'   \item \strong{Max}: Maximum possible p-value across all rounding-consistent
#'     parameter combinations
#' }
#'
#' The \code{Function} field stores the name of the function that generated the
#' results, which is useful for record-keeping when multiple ANOVA reconstruction
#' methods might be added in the future.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{Min}}{Numeric scalar. Minimum reconstructed p-value from the
#'     ANOVA F-test}
#'   \item{\code{RIVETS}}{Numeric scalar. P-value reconstructed using the reported
#'     (central) descriptive statistics}
#'   \item{\code{Max}}{Numeric scalar. Maximum reconstructed p-value from the
#'     ANOVA F-test}
#'   \item{\code{Function}}{Character scalar. Name of the function that produced
#'     these results (e.g., \code{"anova_one_descriptive"})}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(Min = NULL, RIVETS = NULL, Max = NULL, Function = NULL)}}{
#'     Constructor that initializes a new \code{Anova_res} object. All arguments
#'     are optional and default to \code{NULL}}
#'   \item{\code{print()}}{Print results to console in a formatted layout,
#'     displaying the source function and the min/RIVETS/max p-values}
#' }
#'
#' @examples
#' # Typical usage: returned automatically by anova_one_descriptive()
#' result <- anova_one_descriptive(
#'   m = c("2.51", "3.12"),
#'   s = c("0.58", "0.34"),
#'   n = c(40, 40)
#' )
#' print(result)
#'
#' # Manual construction (rarely needed)
#' ar <- Anova_res$new(
#'   Min = 0.001,
#'   RIVETS = 0.005,
#'   Max = 0.015,
#'   Function = "anova_one_descriptive"
#' )
#'
#' @seealso
#'   \code{\link{anova_one_descriptive}} for generating these results
#'
#' @name Anova_res
#' @docType class
#' @keywords classes
#' @export
Anova_res <- R6::R6Class("Anova_res",
                         public = list(
                           #' @field Min Minimum p value from the reconstructed
                           #' ANOVA
                           Min = NULL,
                           #' @field RIVETS p value from the reconstructed
                           #' ANOVA using RIVETS values
                           RIVETS = NULL,
                           #' @field Max Maximum p value from the reconstructed
                           #' ANOVA
                           Max = NULL,
                           #' @field Function The function that was called to create those results
                           #' (DEV: in case we end up having several anova functions)
                           Function = NULL,
                           #' @description
                           #' Create a new Anova_res object
                           #' @param Min Numeric scalar for minimum p-value
                           #' @param RIVETS Numeric scalar for RIVETS p-value
                           #' @param Max Numeric scalar for maximum p-value
                           #' @param Function Character scalar naming the source function
                           #' @return A new `Anova_res` object
                           initialize = function(Min = NULL,
                                                 RIVETS = NULL,
                                                 Max = NULL,
                                                 Function = NULL) {
                             self$Min <- Min
                             self$RIVETS <- RIVETS
                             self$Max <- Max
                             self$Function <- Function
                           },
                           #' @description
                           #' Pretty printing of anovas results
                           print = function() {
                             cat("Results from ", self$Function, "\n")
                             cat("---------------------------------------\n")
                             cat("Minimum p value: ", self$Min, "\n")
                             cat("RIVETS p value:  ", self$RIVETS, "\n")
                             cat("Maximum p value: ", self$Max, "\n")
                             cat("---------------------------------------\n")
                           }
                         )
)

#' Reconstruct One-Way ANOVA F-Test from Group Descriptive Statistics
#'
#' @description
#' Reconstructs the omnibus F-test p-value from a one-way ANOVA using only the
#' reported means, standard deviations, and sample sizes for each group. This
#' allows verification of reported ANOVA results when only descriptive statistics
#' are published. The function accounts for rounding uncertainty by computing a
#' range of plausible p-values.
#'
#' @param m Character vector. Reported mean for each group. Must be character
#'   (not numeric) to allow inference of rounding precision (e.g., \code{"2.5"}
#'   vs \code{"2.50"})
#' @param s Character vector. Reported standard deviation for each group. Must
#'   be character for the same reason as \code{m}. Length must equal length of
#'   \code{m}
#' @param n Numeric vector. Sample size for each group (integers ≥ 1). Length
#'   must equal length of \code{m}
#' @param output Logical. If \code{TRUE}, prints the result before returning.
#'   Default is \code{FALSE}
#'
#' @return An object of class \code{\link{Anova_res}} containing:
#'   \describe{
#'     \item{Min}{Minimum possible p-value from the reconstructed ANOVA}
#'     \item{RIVETS}{P-value computed from the reported (central) values}
#'     \item{Max}{Maximum possible p-value from the reconstructed ANOVA}
#'     \item{Function}{Character string \code{"anova_one_descriptive"}}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts each reported mean and SD to a plausible interval using
#'     \code{\link{validate_descriptive}}, which accounts for rounding precision
#'   \item Generates all combinations of minimum/original/maximum values for all
#'     group means and SDs (3^(2k) combinations where k is the number of groups)
#'   \item For each combination, reconstructs the one-way ANOVA using standard
#'     formulas:
#'     \itemize{
#'       \item Grand mean: \eqn{\bar{X}_{..} = \frac{\sum n_i \bar{X}_i}{\sum n_i}}
#'       \item Between-groups mean square: \eqn{MS_B = \frac{\sum n_i(\bar{X}_i - \bar{X}_{..})^2}{k-1}}
#'       \item Within-groups mean square: \eqn{MS_W = \frac{\sum (n_i-1)s_i^2}{\sum n_i - k}}
#'       \item F-statistic: \eqn{F = \frac{MS_B}{MS_W}}
#'       \item P-value: \eqn{P(F_{k-1, N-k} > F_{observed})}
#'     }
#'     where k is the number of groups and N is the total sample size
#'   \item Records the minimum and maximum p-values across all combinations,
#'     plus the p-value from the reported (central) values
#' }
#'
#' The reconstruction assumes:
#' \itemize{
#'   \item All groups are independent
#'   \item Reported SDs are sample standard deviations (not standard errors)
#'   \item The original analysis used a standard one-way ANOVA (not Welch's
#'     heteroscedastic variant)
#' }
#'
#' @section Use Cases:
#' \itemize{
#'   \item \strong{Verification}: Check whether a reported ANOVA p-value is
#'     consistent with the reported descriptive statistics
#'   \item \strong{Missing p-values}: Estimate ANOVA results when only
#'     descriptive statistics are published
#'   \item \strong{Error detection}: Identify potential transcription errors or
#'     inconsistencies in reported results
#'   \item \strong{Meta-analysis}: Extract effect sizes or p-values from papers
#'     that only report means and SDs
#' }
#'
#' @section Interpretation:
#' \itemize{
#'   \item If a reported ANOVA p-value falls outside the [Min, Max] range,
#'     this suggests inconsistency or potential errors in the reported results
#'   \item Wide ranges indicate high sensitivity to rounding (common with
#'     small sample sizes or highly rounded values)
#'   \item The RIVETS value represents the best estimate given the reported
#'     central values
#' }
#'
#' @section Assumptions and Limitations:
#' \itemize{
#'   \item Assumes equal variance across groups (standard ANOVA assumption).
#'     For heteroscedastic alternatives, consider Welch's ANOVA instead
#'   \item Requires at least 2 groups (k ≥ 2)
#'   \item Very small sample sizes (e.g., n < 5 per group) increase sensitivity
#'     to rounding
#'   \item Cannot detect whether original analysis used different variants
#'     (e.g., Type I vs Type III sums of squares for unbalanced designs,
#'     though this distinction only matters with covariates)
#'   \item The number of combinations grows exponentially with the number of
#'     groups (3^(2k)), which may slow computation for many groups (k > 10)
#' }
#'
#' @examples
#' # Two-group comparison
#' result <- anova_one_descriptive(
#'   m = c("2.51", "3.12"),
#'   s = c("0.58", "0.34"),
#'   n = c(40, 40),
#'   output = TRUE
#' )
#'
#' # Three-group comparison
#' anova_one_descriptive(
#'   m = c("10.5", "12.3", "11.8"),
#'   s = c("2.1", "2.4", "1.9"),
#'   n = c(30, 30, 30)
#' )
#'
#' # Verify published ANOVA results
#' reported_means <- c("5.2", "6.8", "7.1", "5.9")
#' reported_sds <- c("1.2", "1.5", "1.3", "1.4")
#' sample_sizes <- c(25, 25, 25, 25)
#' result <- anova_one_descriptive(
#'   m = reported_means,
#'   s = reported_sds,
#'   n = sample_sizes
#' )
#' # Compare result$RIVETS to the published p-value
#' # If published p-value is outside [result$Min, result$Max], flag inconsistency
#'
#' @seealso
#'   \code{\link{Anova_res}} for the result object structure,
#'   \code{\link{validate_descriptive}} for the validation function,
#'   \code{\link{t_slicer}} for two-group comparisons
#'
#' @importFrom stats pf
#' @export
anova_one_descriptive <- function(m, s, n, output=FALSE)
{
  L <- length_check(list(m=m, s=s, n=n))
  means <- validate_descriptive(m)
  SDs <- validate_descriptive(s)

  means_RIVETS <- means$original
  SDs_RIVETS <- SDs$original
  p_value <- c()

  # Degrees of freedom for between and within
  dfb <- L - 1
  dfw <- sum(n) - L

  # Min-max calculations
  grid_list <- list()
  for(i in seq_len(L)) {
    grid_list[[i]] <- c(means$minimum[i], means$original[i], means$maximum[i])
    grid_list[[i+L]] <- c(SDs$minimum[i], SDs$original[i], SDs$maximum[i])
  }

  cases_grid <- expand.grid(grid_list)
  for(i in seq_len(nrow(cases_grid))) {
    overall_mean <- sum(n*cases_grid[i,][1:L])/sum(n)
    MSb <- sum(n * (cases_grid[i,][1:L] - overall_mean)^2)/dfb
    MSw <-  sum((n-1)*cases_grid[i,][(L+1):(2*L)]^2)/dfw
    F_value <- MSb/MSw
    p_value <- c(p_value, pf(F_value, dfb, dfw, lower.tail = FALSE))
  }

  # RIVETS calculations
  overall_mean <- sum(n*means_RIVETS)/sum(n)
  MSb <- sum(n * (means_RIVETS - overall_mean)^2)/dfb
  MSw <-  sum((n-1)*SDs_RIVETS^2)/dfw
  F_value <- MSb/MSw
  p_value_RIVETS <- pf(F_value, dfb, dfw, lower.tail = FALSE)

  result <- Anova_res$new(Min = min(p_value),
                          RIVETS = p_value_RIVETS,
                          Max = max(p_value),
                          Function = "anova_one_descriptive")

  if(output == TRUE)
  {
    print(result)
  }

  return(result)
}
