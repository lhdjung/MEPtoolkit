#' Results Container for Two-Sample t-Test Consistency Analysis
#'
#' @description
#' An R6 class that stores results from \code{\link{t_slicer}}, including
#' p-value ranges for Student's (pooled-variance) and Welch's (unequal-variance)
#' two-sample t-tests, consistency flags when reported p-values are provided,
#' and diagnostic visualizations.
#'
#' @details
#' This container holds three p-value summaries per test type:
#' \itemize{
#'   \item \strong{min}: Minimum plausible p-value across all rounding-consistent
#'     parameter combinations
#'   \item \strong{RIVETS}: P-value computed from the reported (central) descriptive
#'     statistics
#'   \item \strong{max}: Maximum plausible p-value across all rounding-consistent
#'     parameter combinations
#' }
#'
#' When reported p-values are supplied to \code{t_slicer()}, the consistency
#' fields (\code{Student} and \code{Welch}) indicate whether the reported
#' p-interval overlaps with the corresponding test's plausibility interval.
#'
#' @section Fields:
#' \describe{
#'   \item{\code{Student_RIVETS}}{Numeric vector of p-values computed from
#'     reported descriptive statistics using Student's t-test (pooled variance)}
#'   \item{\code{Welch_RIVETS}}{Numeric vector of p-values computed from
#'     reported descriptive statistics using Welch's t-test (unequal variance)}
#'   \item{\code{Student_min}}{Numeric vector of minimum plausible p-values
#'     for Student's t-test}
#'   \item{\code{Student_max}}{Numeric vector of maximum plausible p-values
#'     for Student's t-test}
#'   \item{\code{Welch_min}}{Numeric vector of minimum plausible p-values
#'     for Welch's t-test}
#'   \item{\code{Welch_max}}{Numeric vector of maximum plausible p-values
#'     for Welch's t-test}
#'   \item{\code{Student}}{Logical vector indicating consistency with Student's
#'     t-test. \code{TRUE} = reported p-interval overlaps plausibility interval,
#'     \code{FALSE} = no overlap, \code{NA} = no reported p-value}
#'   \item{\code{Welch}}{Logical vector indicating consistency with Welch's
#'     t-test (same interpretation as \code{Student})}
#'   \item{\code{Plots}}{List of \pkg{ggplot2} objects, one per case, visualizing
#'     p-value ranges and reported p-intervals (when available)}
#' }
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(Student_RIVETS, Welch_RIVETS, Student_min, Student_max,
#'     Welch_min, Welch_max, Student, Welch, Plots)}}{Constructor that initializes
#'     a new \code{T_slicer_res} object. All arguments are optional and default
#'     to \code{NULL}}
#'   \item{\code{print(n = NULL)}}{Print results to console. If \code{n} is
#'     \code{NULL} (default), prints all cases; if \code{n} is an integer or
#'     vector, prints only the specified case(s)}
#'   \item{\code{plot(n = NULL)}}{Display diagnostic plots. If \code{n} is
#'     \code{NULL} (default), arranges all plots in a grid using
#'     \code{\link[cowplot]{plot_grid}}; if \code{n} is a single integer,
#'     displays only that case's plot}
#' }
#'
#' @examples
#' # Typical usage: returned automatically by t_slicer()
#' result <- t_slicer(m1 = "1.20", s1 = "1.2", n1 = 60,
#'                    m2 = "2.10", s2 = "2.5", n2 = 30,
#'                    p = ".08")
#' print(result)
#' \dontrun{
#' plot(result)
#' }
#'
#' # Manual construction (rarely needed)
#' tr <- T_slicer_res$new(
#'   Student_RIVETS = c(0.12, 0.05),
#'   Welch_RIVETS = c(0.11, 0.06),
#'   Student_min = c(0.08, 0.02),
#'   Student_max = c(0.20, 0.10),
#'   Welch_min = c(0.07, 0.03),
#'   Welch_max = c(0.22, 0.12),
#'   Student = c(TRUE, FALSE),
#'   Welch = c(TRUE, FALSE),
#'   Plots = list(NULL, NULL)
#' )
#'
#' @seealso \code{\link{t_slicer}} for generating these results,
#'   \code{\link{validate_descriptive}} and \code{\link{validate_p}} for
#'   the underlying validation functions
#'
#' @name T_slicer_res
#' @docType class
#' @keywords classes
#' @importFrom cowplot plot_grid
#' @export
T_slicer_res <- R6::R6Class("T_slicer_res",
                            public = list(
                              #' @field Student_RIVETS p-value(s) from Student's t-test using
                              #' RIVETS stats
                              Student_RIVETS = NULL,
                              #' @field Welch_RIVETS p-value(s) from Welch's t-test using
                              #' RIVETS stats
                              Welch_RIVETS = NULL,
                              #' @field Student_min p-value(s) from Student's t-test using
                              #' minimum possible values
                              Student_min = NULL,
                              #' @field Student_max p-value(s) from Student's t-test using
                              #' maximum possible values
                              Student_max = NULL,
                              #' @field Welch_min p-value(s) from Welch's t-test using
                              #' minimum possible values
                              Welch_min = NULL,
                              #' @field Welch_max p-value(s) from Welch's t-test using
                              #' maximum possible values
                              Welch_max = NULL,
                              #' @field Student are statistics consistent with a Student's t-test?
                              Student = NULL,
                              #' @field Welch are statistics consistent with a Welch's t-test?
                              Welch = NULL,
                              #' @field Plots the resulting plot
                              Plots = NULL,
                              #' @description
                              #' Create a new T_slicer_res object
                              #' @param Student_RIVETS Numeric vector to initialise \code{Student_RIVETS}.
                              #' @param Welch_RIVETS Numeric vector to initialise \code{Welch_RIVETS}.
                              #' @param Student_min Numeric vector to initialise \code{Student_min}.
                              #' @param Student_max Numeric vector to initialise \code{Student_max}.
                              #' @param Welch_min Numeric vector to initialise \code{Welch_min}.
                              #' @param Welch_max Numeric vector to initialise \code{Welch_max}.
                              #' @param Student Logical vector to initialise \code{Student}.
                              #' @param Welch Logical vector to initialise \code{Welch}.
                              #' @param Plots List of \pkg{ggplot2} objects to initialise \code{Plots}.
                              #' @return An R6 object of class \code{T_slicer_res} for programmatic access and
                              #' human inspection (via \code{print} and \code{plot}).
                              initialize = function(Student_RIVETS, Welch_RIVETS, Student_min,
                                                    Student_max, Welch_min, Welch_max,
                                                    Student, Welch, Plots) {
                                self$Student_RIVETS <- Student_RIVETS
                                self$Welch_RIVETS <- Welch_RIVETS
                                self$Student_min <- Student_min
                                self$Student_max <- Student_max
                                self$Welch_min <- Welch_min
                                self$Welch_max <- Welch_max
                                self$Student <- Student
                                self$Welch <- Welch
                                self$Plots <- Plots
                              },
                              #' @description
                              #' Pretty printing of t_slicer results
                              #' @param n if NULL (default), outputs results for all
                              #' lines. Otherwise, only the one from the specified line.
                              print = function(n=NULL) {
                                cat("Results from t_slicer\n")
                                if(is.null(n) || n>length(self$Student) || n<=0)
                                  n <- 1:(length(self$Student))
                                for(i in n) {
                                  cat("\n\n--- Line", i,"---\n")
                                  cat("---------------------------------------\n")
                                  cat("RIVETS p values:\n")
                                  cat("      Student: p =", self$Student_RIVETS[i], "\n")
                                  cat("      Welch:   p =", self$Welch_RIVETS[i], "\n\n")
                                  cat("Min/max p values:\n")
                                  cat("  - Minimum\n")
                                  cat("      Student: p =", self$Student_min[i], "\n")
                                  cat("      Welch:   p =", self$Welch_min[i], "\n\n")
                                  cat("  - Maximum\n")
                                  cat("      Student: p =", self$Student_max[i], "\n")
                                  cat("      Welch:   p =", self$Welch_max[i], "\n")
                                  cat("---------------------------------------\n")
                                  cat("Student's t:  ", if(is.na(self$Student[i])) "N/A\n"
                                      else if (self$Student[i] == TRUE) "Consistent\n"
                                      else "Inconsistent\n")
                                  cat("Welch's t:    ", if(is.na(self$Welch[i])) "N/A\n"
                                      else if (self$Welch[i] == TRUE) "Consistent\n"
                                      else "Inconsistent\n")
                                }
                              },
                              #' @description
                              #' Plots the results.
                              #' @param n if NULL (default), plots a grid of all graphs for all
                              #' lines. Otherwise, plots the graph for the specified line.
                              #' @importFrom cowplot plot_grid
                              plot = function(n = NULL) {
                                if(is.null(n) || n>length(self$Student) || n<=0)
                                  plot_grid(plotlist = self$Plots, ncol=2)
                                else {
                                  plot(self$Plots[[n]])
                                }
                              }
                            )
)

#' Test Consistency of Reported t-Test Results with Descriptive Statistics
#'
#' @description
#' Determines which two-sample t-test variant (Student's pooled-variance or
#' Welch's unequal-variance) is consistent with reported summary statistics.
#' When p-values are provided, the function checks whether they align with
#' either test. The analysis accounts for rounding imprecision by computing
#' plausible intervals around reported values.
#'
#' @param m1 Character scalar or vector. Reported mean(s) for group 1. Can be
#'   formatted numbers (e.g., \code{"1.23"}) that may represent rounded values
#' @param s1 Character scalar or vector. Reported standard deviation(s) for
#'   group 1 (same format as \code{m1})
#' @param n1 Numeric scalar or vector. Sample size(s) for group 1 (integers ≥ 2)
#' @param m2 Character scalar or vector. Reported mean(s) for group 2 (same
#'   format as \code{m1})
#' @param s2 Character scalar or vector. Reported standard deviation(s) for
#'   group 2 (same format as \code{s1})
#' @param n2 Numeric scalar or vector. Sample size(s) for group 2 (integers ≥ 2)
#' @param p Character scalar or vector, optional. Reported p-value(s). Can be
#'   \code{NA} or omitted when no p-value is reported. Default is \code{NULL}
#' @param data Data frame, optional. If provided, the other arguments are
#'   interpreted as column names (quoted or unquoted) within \code{data}
#' @param output Logical. If \code{TRUE}, prints a summary and displays
#'   diagnostic plots before returning the result object. Default is \code{FALSE}
#'
#' @return An object of class \code{\link{T_slicer_res}} containing:
#'   \describe{
#'     \item{Student_min, Student_RIVETS, Student_max}{Minimum, reported-value,
#'       and maximum plausible p-values for Student's t-test (pooled variance)}
#'     \item{Welch_min, Welch_RIVETS, Welch_max}{Analogous p-values for
#'       Welch's t-test (unequal variance)}
#'     \item{Student}{Logical vector indicating whether reported p-value is
#'       consistent with Student's t-test (\code{TRUE} = consistent,
#'       \code{FALSE} = inconsistent, \code{NA} = no reported p)}
#'     \item{Welch}{Logical vector indicating consistency with Welch's t-test
#'       (same interpretation as \code{Student})}
#'     \item{Plots}{List of \pkg{ggplot2} objects visualizing p-value ranges
#'       and reported p-intervals for each case}
#'   }
#'
#' @details
#' The function performs the following steps for each case:
#' \enumerate{
#'   \item Converts each reported descriptive statistic (means and SDs) into a
#'     plausible interval using \code{\link{validate_descriptive}}, which
#'     accounts for rounding precision
#'   \item Generates all combinations of minimum/maximum values for the four
#'     descriptive statistics (two means, two SDs)
#'   \item Computes two-sample t-test p-values for each combination using
#'     \code{\link[BSDA]{tsum.test}} with both \code{var.equal = TRUE}
#'     (Student's t-test) and \code{var.equal = FALSE} (Welch's t-test)
#'   \item Records the minimum and maximum p-values across all combinations,
#'     plus the p-value computed from the reported (central) values (RIVETS)
#'   \item If a reported p-value is provided, converts it to an interval via
#'     \code{\link{validate_p}} and checks whether it overlaps with the
#'     plausibility intervals for each test
#'   \item Creates a diagnostic plot showing the p-value ranges and, when
#'     applicable, the reported p-interval as a colored rectangle (green for
#'     consistent, red for inconsistent)
#' }
#'
#' This function is particularly useful for:
#' \itemize{
#'   \item Verifying which t-test variant was likely used in published analyses
#'   \item Detecting potential errors in reported statistics
#'   \item Preparing inputs for more detailed consistency analyses (e.g.,
#'     \code{carlisle} tests)
#' }
#'
#' @section Assumptions and Limitations:
#' \itemize{
#'   \item Sample sizes should be integers ≥ 2. Very small samples (n < 5)
#'     increase sensitivity to rounding and may produce unstable results
#'   \item All reported values are assumed to be rounded to the precision
#'     indicated by their string representation
#'   \item The function assumes independent samples and approximately normal
#'     distributions within groups (standard t-test assumptions)
#' }
#'
#' @examples
#' # Single comparison with reported p-value
#' result <- t_slicer(
#'   m1 = "1.20", s1 = "1.2", n1 = 60,
#'   m2 = "2.10", s2 = "2.5", n2 = 30,
#'   p = ".08",
#'   output = TRUE
#' )
#'
#' # Multiple comparisons from a data frame
#' df <- data.frame(
#'   m1 = c("1.20", "1.25"),
#'   s1 = c("1.2", "1.25"),
#'   n1 = c(60, 60),
#'   m2 = c("2.10", "2.15"),
#'   s2 = c("2.5", "2.55"),
#'   n2 = c(30, 30),
#'   p = c(".08", NA)
#' )
#' result <- t_slicer(
#'   m1 = m1, s1 = s1, n1 = n1,
#'   m2 = m2, s2 = s2, n2 = n2,
#'   p = p, data = df
#' )
#' print(result)
#' \dontrun{
#' plot(result)
#' }
#'
#' @seealso
#'   \code{\link{T_slicer_res}} for the result object structure,
#'   \code{\link{validate_descriptive}} and \code{\link{validate_p}} for
#'   validation functions,
#'   \code{\link[BSDA]{tsum.test}} for the underlying t-test implementation
#'
#' @import ggplot2
#' @importFrom BSDA tsum.test
#' @importFrom rlang enquo
#' @importFrom ggnewscale new_scale
#' @export
t_slicer <- function (m1, s1, n1, m2, s2, n2, p = NULL, data = NULL, output = FALSE)
{
  # Input validation
  if(!is.null(data)) {
    if(!is.data.frame(data)) stop("`data` must be a data.frame.")
    m1 <- resolve(enquo(m1), data); s1 <- resolve(enquo(s1), data);
    n1 <- resolve(enquo(n1), data); m2 <- resolve(enquo(m2), data);
    s2 <- resolve(enquo(s2), data); n2 <- resolve(enquo(n2), data);
    p  <- resolve(enquo(p), data)
  }
  L <- length_check(list(m1=m1, s1=s1, m2=m2, s2=s2, n1=n1, n2=n2, p=p))
  m1 <- validate_descriptive(m1)
  s1 <- validate_descriptive(s1)
  m2 <- validate_descriptive(m2)
  s2 <- validate_descriptive(s2)
  p <- if(!is.null(p)) validate_p(p, allow_na = TRUE)
  else validate_p(rep(NA_character_, L), allow_na = TRUE)

  plots <- list()
  Student_pass <- c()
  Welch_pass <- c()
  Student_min <- c()
  Student_RIVETS <- c()
  Student_max <- c()
  Welch_min <- c()
  Welch_RIVETS <- c()
  Welch_max <- c()

  for (j in seq_len(L)) {
    m1_range <- c(m1$minimum[j], m1$maximum[j])
    m1_num <- m1$original[j]
    s1_range <- c(s1$minimum[j], s1$maximum[j])
    s1_num <- s1$original[j]
    m2_range <- c(m2$minimum[j], m2$maximum[j])
    m2_num <- m2$original[j]
    s2_range <- c(s2$minimum[j], s2$maximum[j])
    s2_num <- s2$original[j]
    p_range <- c(p$minimum[j], p$maximum[j])
    p_num <- p$original[j]

    cases_grid <- expand.grid(m1 = m1_range, s1 = s1_range, m2 = m2_range, s2 = s2_range)

    # RIVETS values
    twelch <- tsum.test(mean.x = m1_num, s.x = s1_num, n.x = n1[j], mean.y = m2_num, s.y = s2_num, n.y = n2[j], var.equal = FALSE)$p.value
    tstudent <- tsum.test(mean.x = m1_num, s.x = s1_num, n.x = n1[j], mean.y = m2_num, s.y = s2_num, n.y = n2[j], var.equal = TRUE)$p.value

    # Get all possible min and max combinations for means and sds for g1 and g2
    p_twelch_grid <- c()
    p_tstudent_grid <- c()
    for(i in seq_len(nrow(cases_grid)))
    {
      p_twelch_grid <- c(p_twelch_grid, tsum.test(mean.x = cases_grid[i, "m1"], s.x = cases_grid[i, "s1"], n.x = n1, mean.y = cases_grid[i, "m2"], s.y = cases_grid[i, "s2"], n.y = n2, var.equal = FALSE)$p.value)
      p_tstudent_grid <- c(p_tstudent_grid, tsum.test(mean.x = cases_grid[i, "m1"], s.x = cases_grid[i, "s1"], n.x = n1, mean.y = cases_grid[i, "m2"], s.y = cases_grid[i, "s2"], n.y = n2, var.equal = TRUE)$p.value)
    }

    Student_min <- c(Student_min, min(p_tstudent_grid))
    Student_RIVETS <- c(Student_RIVETS, tstudent)
    Student_max <- c(Student_max, max(p_tstudent_grid))
    Welch_min <- c(Welch_min, min(p_twelch_grid))
    Welch_RIVETS <- c(Welch_RIVETS, twelch)
    Welch_max <- c(Welch_max, max(p_twelch_grid))

    # Plot
    plot_data <- data.frame(
      Student = c(Student_min[j], Student_RIVETS[j], Student_max[j]),
      Welch = c(Welch_min[j], Welch_RIVETS[j], Welch_max[j]),
      Value = c("Minimum", "Mid", "Maximum")
    )

    if(!is.na(p_num)) {
      rect_data <- data.frame(
        Reported = c("Student", "Welch"),
        # Right now rect X coordinates are hard-coded, but would have to be changed
        # if more tests are added
        Xmin = c(0.65, 1.65),
        Xmax = c(1.35, 2.35),
        Ymin = c(p_range[1], p_range[1]),
        Ymax = c(p_range[2], p_range[2])
      )
      if(p_range[1] <= Student_max[j] && p_range[2] >= Student_min[j]) {
        color_rect_student <- "green"
        Student_pass <- c(Student_pass, TRUE)
      }
      else {
        color_rect_student <- "red"
        Student_pass <- c(Student_pass, FALSE)
      }
      if(p_range[1] <= Welch_max[j] && p_range[2] >= Welch_min[j]) {
        color_rect_welch <- "green"
        Welch_pass <- c(Welch_pass, TRUE)
      }
      else {
        color_rect_welch <- "red"
        Welch_pass <- c(Welch_pass, FALSE)
      }
    }
    else {
      Student_pass <- c(Student_pass, NA)
      Welch_pass <- c(Welch_pass, NA)
    }
    plots[[j]] <- ggplot(plot_data) +
      geom_point(aes(x="Welch", y=.data$Welch, shape=.data$Value, fill=.data$Value), size = 5) +
      geom_point(aes(x="Student", y=.data$Student, shape=.data$Value, fill=.data$Value), size = 5) +
      theme_minimal() +
      labs(title = "T-Test Results", y = "Potential p-values", x = "Tests") +
      theme(axis.text = element_text(size=12),
            axis.title = element_text(size=13, face="bold")) +
      scale_fill_manual(
        name = "Value",
        values = c('Minimum' = "red", 'Mid' = "blue", 'Maximum' = "red")) +
      scale_shape_manual(
        name = "Value",
        values = c('Minimum' = 24, 'Mid' = 21, 'Maximum' = 25))
    if(!is.na(p_num)) {
      plots[[j]] <- plots[[j]] + new_scale("fill") +
        geom_rect(
          data = rect_data,
          aes(xmin=.data$Xmin, xmax=.data$Xmax, ymin=.data$Ymin,
              ymax=.data$Ymax, fill=.data$Reported),
          alpha=.5, color="black") +
        scale_fill_manual(
          values = c('Student' = color_rect_student, 'Welch' = color_rect_welch))
    }

  }

  # Return value
  result <- T_slicer_res$new(Student_RIVETS = Student_RIVETS,
                             Student = Student_pass, Welch = Welch_pass,
                             Welch_max = Welch_max,
                             Welch_RIVETS = Welch_RIVETS,
                             Welch_min = Welch_min,
                             Student_max = Student_max,
                             Student_min = Student_min,
                             Plots = plots)

  if(output == TRUE)
  {
    plot(result)
    print(result)
  }

  return(result)
}
