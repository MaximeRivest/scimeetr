\name{make_suit_freq_df}
\alias{make_suit_freq_df}
\title{
Make a Suit of freq dataframe
}
\description{
This function makes a suit of dataframe for which there is a column of factors,
a column of frequencies and a column of relative frequencies. There is one
dataframe by community plus one for all the records. There is frequency 
dataframe for most cited papers, most prolific authors, most said keyword by
the community and many others. See details for more information.
}
\usage{
make_suit_freq_df(WOS_dataframe)
}
\arguments{
  \item{WOS_dataframe}{
\code{WOS_dataframe} a adress cleaned and community updated dataframe which is
the result of successively the \code{import_wos_files()} function, the
\code{make_cocit_df()} function, the \code{find_rec_com()} function and the
\code{clean_adresses()} function. 
}
}
\details{
none
}
\value{
a list of 8 elementes and n dataframe per element. Where n is the number of
communities identified.

\item{AU}{List of frequency dataframe of the most prolific authors}
\item{CR}{List of frequency dataframe of the most cited articles}
\item{SO}{List of frequency dataframe of the most published in journals}
\item{JI}{List of frequency dataframe of the most published in journals}
\item{PY}{List of frequency dataframe of the most published in years}
\item{WC}{List of frequency dataframe of the most assigned to categories}
\item{SC}{List of frequency dataframe of the most assigned to research area}
\item{KW}{List of frequency dataframe of the most mentionned keywords}
\item{UN}{List of frequency dataframe of the most prolific universities}
\item{CO}{List of frequency dataframe of the most prolific countries}

}
\author{
Maxime Rivest
}
\seealso{
\code{\link{help}}, \code{\link{make_cocit_df}}, \code{\link{import_wos_files}},
\code{\link{clean_adresses}}, \code{\link{find_rec_com}}, \code{\link{scimeetr}}
}
\examples{
data("wos_df_example")
my_cocit_df <- make_cocit_df(WOS_dataframe = wos_df_example)
my_updated_wos_df <- find_rec_com(cocitation_df = my_cocit_df, WOS_dataframe = wos_df_example)
my_updated_clean_wos_df <- clean_adresses(my_updated_wos_df)
my_suit_of_freq_df <- make_suit_freq_df(my_updated_clean_wos_df)
}
\keyword{ manip }
