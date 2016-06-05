\name{make_cocit_df}
\alias{make_cocit_df}
\title{
Make a Co-citation dataframe
}
\description{
This function make a dataframe of co-citations.
}
\usage{
make_cocit_df(WOS_dataframe)
}
\arguments{
  \item{WOS_dataframe}{
a Web of Science dataframe that is the result of the function \code{import_wos_files()}
}
}
\details{
to come...
}
\value{
a dataframe with 5 columns
}
\references{
none
}
\author{
Maxime Rivest
}
\note{
none
}
\seealso{
\code{\link{import_wos_files}},
}
\examples{
data("wos_df_example")
my_cocit_df <- make_cocit_df(WOS_dataframe = wos_df_example)
}
\keyword{ manip }
