#' Results Container for Carlisle Test of Baseline P-Value Aggregation
#'
#' @description
#' An R6 class that stores results from \code{\link{carlisle}}, which aggregates
#' multiple p-values to test for unusual concentration. The container holds
#' combined p-value ranges accounting for rounding uncertainty.
#'
#' @details
#' This object stores aggregated p-values in three-element vectors following
#' the format \code{c(min, RIVETS, max)}:
#' \itemize{
#'   \item \strong{min}: Minimum combined p-value across all rounding-consistent
#'     parameter combinations
#'   \item \strong{RIVETS}: Combined p-value computed from the reported
#'     (central) values
#'   \item \strong{max}: Maximum combined p-value across all rounding-consistent
#'     parameter combinations
#' }
#'
#' The object can store results from two different scenarios:
#' \enumerate{
#'   \item When aggregating p-values directly: only \code{Provided_p} is populated
#'   \item When aggregating from \code{\link{t_slicer}} output: both \code{Student}
#'     and \code{Welch} are populated with separate results for each t-test variant
#' }
#'
#' @section Fields:
#' \describe{
#'   \item{\code{Student}}{Numeric vector of length 3, \code{c(min, RIVETS, max)},
#'     containing combined p-values when aggregating Student's (pooled-variance)
#'     t-test results. \code{NULL} when created from direct p-values}
#'   \item{\code{Welch}}{Numeric vector of length 3, \code{c(min, RIVETS, max)},
#'     containing combined p-values when aggregating Welch's (unequal-variance)
#'     t-test results. \code{NULL} when created from direct p-values}
#'   \item{\code{Provided_p}}{Numeric vector of length 3, \code{c(min, RIVETS, max)},
#'     containing combined p-values when aggregating directly provided p-values.
#'     \code{NULL} when created from \code{T_slicer_res} output}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(Student = NULL, Welch = NULL, Provided_p = NULL)}}{Constructor
#'     that initializes a new \code{Carlisle_res} object. All arguments are
#'     optional and default to \code{NULL}}
#'   \item{\code{print()}}{Print results to console in a formatted layout,
#'     displaying RIVETS values and min/max combinations}
#' }
#'
#' @examples
#' # Typical usage: returned automatically by carlisle()
#' result <- carlisle(c(".05", ".03", ".02", ".08"))
#' print(result)
#'
#' # Manual construction from direct p-values (rarely needed)
#' cr1 <- Carlisle_res$new(Provided_p = c(0.012, 0.045, 0.098))
#' print(cr1)
#'
#' # Manual construction with Student and Welch results (rarely needed)
#' cr2 <- Carlisle_res$new(
#'   Student = c(0.001, 0.020, 0.150),
#'   Welch = c(0.002, 0.025, 0.160)
#' )
#' print(cr2)
#'
#' @seealso
#'   \code{\link{carlisle}} for generating these results,
#'   \code{\link{t_slicer}} for the upstream t-test analysis
#'
#' @name Carlisle_res
#' @docType class
#' @keywords classes
#' @export
Carlisle_res <- R6::R6Class("Carlisle_res",
                            public = list(
                              #' @field Student Carlisle min/RIVETS/max test results using
                              #' the range of Student p values from a T_slicer_res object
                              Student = NULL,
                              #' @field Welch Carlisle min/RIVETS/max test results using
                              #' the range of Welch p values from a T_slicer_res object
                              Welch = NULL,
                              #' @field Provided_p Carlisle min/RIVETS/max test results using
                              #' the range of p values manually provided
                              Provided_p = NULL,
                              #' @description
                              #' Create a new Carlisle_res object
                              #' @param Student Numeric vector of length 3 to initialize \code{Student}
                              #' @param Welch Numeric vector of length 3 to initialize \code{Welch}
                              #' @param Provided_p Numeric vector of length 3 to initialize \code{Provided_p}
                              #' @return A new `Carlisle_res` object
                              initialize = function(Student = NULL,
                                                    Welch = NULL,
                                                    Provided_p = NULL) {
                                self$Student <- Student
                                self$Welch <- Welch
                                self$Provided_p <- Provided_p
                              },
                              #' @description
                              #' Pretty printing of Carlisle results
                              print = function() {
                                cat("Results from Carlisle\n")
                                cat("---------------------------------------\n")
                                if(is.null(self$Provided_p)) {
                                  cat("RIVETS p values:\n")
                                  cat("      Student: p =", self$Student[2], "\n")
                                  cat("      Welch:   p =", self$Welch[2], "\n\n")
                                  cat("Min/max p value combinations:\n")
                                  cat("  - Minimum\n")
                                  cat("      Student: p =", self$Student[1], "\n")
                                  cat("      Welch:   p =", self$Welch[1], "\n\n")
                                  cat("  - Maximum\n")
                                  cat("      Student: p =", self$Student[3], "\n")
                                  cat("      Welch:   p =", self$Welch[3], "\n")
                                }
                                else {
                                  cat("RIVETS p values            :", self$Provided_p[2], "\n")
                                  cat("minimum p value combination:", self$Provided_p[1], "\n")
                                  cat("maximum p value combination:", self$Provided_p[3], "\n")
                                }
                                cat("---------------------------------------\n")
                              }
                            )
)

#' Test for Unusual Concentration in Baseline P-Values (Carlisle Test)
#'
#' @description
#' Aggregates a set of baseline comparison p-values to detect unusual patterns
#' that may indicate systematic imbalances, reporting errors, or data anomalies.
#' Commonly used to screen randomized controlled trials for problems in their
#' baseline characteristics table (Table 1). The function uses Stouffer's method
#' (inverse-normal combination) and accounts for rounding uncertainty by
#' computing a range of possible combined p-values.
#'
#' @param p Either a character vector of reported p-values (e.g., \code{".03"},
#'   \code{"0.12"}, \code{"<.001"}) or a \code{\link{T_slicer_res}} object
#'   returned by \code{\link{t_slicer}}. When a character vector is provided,
#'   each element is treated as a potentially rounded value. When a
#'   \code{T_slicer_res} object is provided, the function aggregates the p-value
#'   ranges for both Student's and Welch's t-tests separately
#' @param output Logical. If \code{TRUE}, prints the result before returning.
#'   Default is \code{FALSE}
#'
#' @return An object of class \code{\link{Carlisle_res}} containing:
#'   \describe{
#'     \item{When \code{p} is a character vector:}{\code{Provided_p}, a numeric
#'       vector of length 3 with \code{c(min, RIVETS, max)} combined p-values}
#'     \item{When \code{p} is a \code{T_slicer_res} object:}{\code{Student} and
#'       \code{Welch}, each a numeric vector of length 3 with
#'       \code{c(min, RIVETS, max)} combined p-values for the respective t-test
#'       variant}
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts each input p-value to a plausible interval using
#'     \code{\link{validate_p}}, which accounts for rounding precision
#'   \item Generates all combinations of minimum/maximum values across the
#'     set of p-values
#'   \item For each combination, computes the combined p-value using Stouffer's
#'     Z-score method: transforms each p-value to a Z-score via the inverse
#'     normal distribution, sums the Z-scores, divides by the square root of
#'     the number of tests, and converts back to a p-value
#'   \item Records the minimum and maximum combined p-values across all
#'     combinations, plus the combined p-value from the reported (central) values
#'   \item When input is from \code{t_slicer}, performs the aggregation
#'     separately for Student's and Welch's t-test p-value ranges
#' }
#'
#' The Stouffer method assumes:
#' \itemize{
#'   \item Independence of the individual tests (correlated variables like
#'     height and weight can violate this)
#'   \item All individual null hypotheses are true (appropriate for baseline
#'     comparisons in randomized trials)
#'   \item P-values are uniformly distributed under the null
#' }
#'
#' @section Interpretation:
#' \itemize{
#'   \item \strong{Very small combined p-values} (e.g., < 0.001) suggest an
#'     unusual concentration of small individual p-values, which may indicate:
#'     \itemize{
#'       \item Genuine systematic differences between groups (potential
#'         randomization failure)
#'       \item Reporting errors (e.g., confusing standard deviations with
#'         standard errors)
#'       \item Data fabrication or manipulation
#'     }
#'   \item \strong{Very large combined p-values} (e.g., > 0.999) suggest an
#'     unusual concentration of large individual p-values, which may indicate
#'     groups are implausibly similar
#'   \item This test is a screening tool, not definitive proof of problems
#'   \item Consider the independence assumption: highly correlated baseline
#'     variables will make the test anticonservative (too many false positives)
#' }
#'
#' @section Limitations:
#' \itemize{
#'   \item The test assumes independence; including correlated variables
#'     (e.g., weight, BMI, and body surface area together) inflates Type I error
#'   \item The test is sensitive to the number of comparisons: more comparisons
#'     increase the chance of detecting departures from uniformity
#'   \item Cannot distinguish between different causes of unusual patterns
#'     (genuine imbalance vs. errors vs. fabrication)
#' }
#'
#' @examples
#' # Test a set of suspiciously small p-values
#' result <- carlisle(c(".05", ".03", ".02", ".08"), output = TRUE)
#'
#' # Test a set of suspiciously large p-values
#' carlisle(c(".99", ".95", ".98", ".90"), output = TRUE)
#'
#' # Use with t_slicer output to test both Student and Welch variants
#' df <- data.frame(
#'   m1 = c("1.20", "1.25"),
#'   s1 = c("1.2", "1.25"),
#'   n1 = c(60, 60),
#'   m2 = c("2.1", "2.15"),
#'   s2 = c("2.5", "2.55"),
#'   n2 = c(30, 30)
#' )
#' t_result <- t_slicer(
#'   m1 = m1, s1 = s1, n1 = n1,
#'   m2 = m2, s2 = s2, n2 = n2,
#'   data = df
#' )
#' carlisle(t_result, output = TRUE)
#'
#' @references
#' Carlisle, J. B. (2012). The analysis of 168 randomised controlled trials to
#' test data integrity. \emph{Anaesthesia}, 67(5), 521-537.
#'
#' Stouffer, S. A., Suchman, E. A., DeVinney, L. C., Star, S. A., & Williams,
#' R. M., Jr. (1949). \emph{The American Soldier: Adjustment during Army life}
#' (Vol. 1). Princeton University Press.
#'
#' @seealso
#'   \code{\link{Carlisle_res}} for the result object structure,
#'   \code{\link{t_slicer}} for computing p-value ranges from descriptive
#'   statistics,
#'   \code{\link{validate_p}} for the p-value validation function
#'
#' @importFrom stats qnorm pnorm
#' @export
carlisle <- function(p, output = FALSE) {

  if(class(p)[1]!="T_slicer_res") {
    res_list <- list()
    carlisle_res <- c()

    p <- validate_p(p)

    for(i in seq_len(length(p$original))) {
      res_list[[i]] <- c(p$minimum[i], p$maximum[i])
    }
    cases_grid <- expand.grid(res_list)

    for(i in seq_len(nrow(cases_grid))) {
      carlisle_res <- c(carlisle_res, 1 - pnorm(sum(sapply(unlist(cases_grid[i,]), qnorm))/sqrt(length(unlist(cases_grid[i,])))))
    }
    carlisle_RIVETS <- 1 - pnorm(sum(sapply(p$original, qnorm))/sqrt(length(p$original)))

    result <- Carlisle_res$new(Provided_p = c(min(carlisle_res), carlisle_RIVETS, max(carlisle_res)))
  }
  else {
    res_Student <- list()
    res_Welch <- list()
    carlisle_Student_res <- c()
    carlisle_Welch_res <- c()

    for(i in seq_len(length(p$Student_RIVETS))) {
      res_Student[[i]] <- c(p$Student_min[i], p$Student_max[i])
    }

    cases_grid <- expand.grid(res_Student)


    for(i in seq_len(nrow(cases_grid))) {
      carlisle_Student_res <- c(carlisle_Student_res, 1 - pnorm(sum(sapply(unlist(cases_grid[i,]), qnorm))/sqrt(length(unlist(cases_grid[i,])))))
    }
    carlisle_Student_RIVETS <- 1 - pnorm(sum(sapply(p$Student_RIVETS, qnorm))/sqrt(length(p$Student_RIVETS)))

    for(i in seq_len(length(p$Welch_RIVETS))) {
      res_Welch[[i]] <- c(p$Welch_min[i], p$Welch_max[i])
    }

    cases_grid <- expand.grid(res_Welch)
    carlisle_Welch_res <- c()

    for(i in seq_len(nrow(cases_grid))) {
      carlisle_Welch_res <- c(carlisle_Welch_res, 1 - pnorm(sum(sapply(unlist(cases_grid[i,]), qnorm))/sqrt(length(unlist(cases_grid[i,])))))
    }
    carlisle_Welch_RIVETS <- 1 - pnorm(sum(sapply(p$Welch_RIVETS, qnorm))/sqrt(length(p$Welch_RIVETS)))

    result <- Carlisle_res$new(Student = c(min(carlisle_Student_res), carlisle_Student_RIVETS, max(carlisle_Student_res)),
                               Welch = c(min(carlisle_Welch_res), carlisle_Welch_RIVETS, max(carlisle_Welch_res)))

  }

  if(output==TRUE) {
    print(result)
  }
  return(result)
}
