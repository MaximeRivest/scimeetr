\name{clean_adresses}
\alias{clean_adresses}
\title{
 Cleans countries and universities names
}
\description{
This extract, clean and add the countries and universities names to a WOS_dataframe
}
\usage{
clean_adresses(WOS_dataframe)
}
\arguments{
  \item{WOS_dataframe}{
a community updated dataframe which is
the result of the \code{import_wos_files()} function
}
}
\value{
A dataframe that is exactly the same as the \code{WOS_dataframe} used as input except for two new columns (the 64th and 65th) called \code{Universities and countries}.
}
\author{
Maxime Rivest
}
\seealso{
\code{\link{help}}, \code{\link{make_cocit_df}}, \code{\link{import_wos_files}},
\code{\link{make_suit_freq_df}}, \code{\link{find_rec_com}}, \code{\link{scimeetr}}
}
\examples{
data("wos_df_example")
my_cocit_df <- make_cocit_df(WOS_dataframe = wos_df_example)
my_updated_wos_df <- find_rec_com(cocitation_df = my_cocit_df, WOS_dataframe = wos_df_example)
my_updated_clean_wos_df <- clean_adresses(my_updated_wos_df) 
}
\keyword{ manip }