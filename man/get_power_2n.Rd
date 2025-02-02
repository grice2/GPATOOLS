% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_power_2n.R
\name{get_power_2n}
\alias{get_power_2n}
\title{Compute GPA's Persuasion Test Power Stasitics}
\usage{
get_power_2n(
  data,
  treatment,
  survey_q,
  prop = "percent",
  n1,
  n2,
  alt = "greater",
  control = "Control"
)
}
\arguments{
\item{data}{Data containing survey response data (pre-processed by hypos_prep or equivalent)}

\item{treatment}{Column specifying treatments of interest (e.g., ad_name)}

\item{survey_q}{The survey question of interest}

\item{prop}{Column containing proportions for the response of interest (default to be "responses")}

\item{n1}{Sample size for treatments}

\item{n2}{Sample size for control}

\item{alt}{Alternative hypothesis; can be "greater", "less", or "two.sided" (default to be "greater")}
}
\value{
A new data frame containing input data plus an additional column for power values
}
\description{
This function generates the power values associated with
several treatments against a control, for a single survey question, where
the sample size is different between treatment and control.
}
\details{
Developer note: users can take advantages of the "control" parameter to  perform hypothesis testing in the event no control variable is present.
}
\examples{
get_power_2n(data = df, treatment = "ads", suvey_q = "Q4",  n1=1200, n2=600, alt="greater", control = "Control")
get_power_2n(data = df, treatment = "ads", suvey_q = "Q4",  n1=1200, n2=600, alt="greater", control = "Naturalization")
}
