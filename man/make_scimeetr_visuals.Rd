\name{make_scimeetr_visuals}
\alias{make_scimeetr_visuals}
\title{
Visualize scimeetr tables
}
\description{
to come
}
\usage{
make_scimeetr_visuals(WOS_dataframe, suit_freq_tables, table_length, tables_type_to_include)
}
\arguments{
  \item{WOS_dataframe}{
\code{WOS_dataframe} a adress cleaned and community updated dataframe which is
the result of successively the \code{import_wos_files()} function, the
\code{make_cocit_df()} function, the \code{find_rec_com()} function and the
\code{clean_adresses()} function. 
}
  \item{suit_freq_tables}{
 a list that is the result of the function \code{suit_freq_tables}
}
  \item{table_length}{
an integer equal to the lenght that you want you output table to be
}
  \item{tables_type_to_include}{
a character equal to either "all", "table" or "community"
}
}
\value{
write file in current directory
}
\author{
Maxime Rivest
}
\seealso{
\code{\link{help}}, \code{\link{make_cocit_df}}, \code{\link{import_wos_files}},
\code{\link{clean_adresses}}, \code{\link{find_rec_com}}, \code{\link{scimeetr}},
\code{\link{make_suit_freq_df}}
}
\examples{
data("wos_df_example")
my_cocit_df <- make_cocit_df(WOS_dataframe = wos_df_example)
my_updated_wos_df <- find_rec_com(cocitation_df = my_cocit_df, WOS_dataframe = wos_df_example)
my_updated_clean_wos_df <- clean_adresses(my_updated_wos_df)
my_suit_of_freq_df <- make_suit_freq_df(my_updated_clean_wos_df)
make_scimeetr_visuals(WOS_dataframe = my_updated_clean_wos_df,
  suit_freq_tables = my_suit_of_freq_df,
  table_length = 8,
  tables_type_to_include = "table")
}
\keyword{ manip }