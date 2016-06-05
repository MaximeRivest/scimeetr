\name{find_rec_com}
\alias{find_rec_com}
\title{
Find and add record community Id
}
\description{
to come...
}
\usage{
find_rec_com(cocitation_df, WOS_dataframe)
}
\arguments{
  \item{cocitation_df}{
This \code{cocitation_df} is the cocitation-matrix obtained from the function \code{make_cocit_df}
}
  \item{WOS_dataframe}{
A Web of Science dataframe that is the result of the function \code{import_wos_files()}
}
}
\details{
none
}
\value{
A dataframe that is exactly the same as the \code{WOS_dataframe} used as input except for a new column (the 63rd) called \code{record_community}.
}
\author{
Maxime Rivest
}
\seealso{
\code{\link{help}}, \code{\link{make_cocit_df}}, \code{\link{import_wos_files}}
}
\examples{
data("wos_df_example")
my_cocit_df <- make_cocit_df(WOS_dataframe = wos_df_example)
my_updated_wos_df <- find_rec_com(cocitation_df = my_cocit_df, WOS_dataframe = wos_df_example)
}
\keyword{ manip }
