\name{import_wos_files}
\alias{import_wos_files}
\title{
Import and Merge Web of Science Files into a Data Frame
}
\description{
This function imports and if there is more than one file in the user selected directory automatically merges Web of Science files. This is all put into a data frame. Moreover, all duplicated records are automatically removed.
}
\usage{
import_wos_files(files_directory)
}
\arguments{
  \item{files_directory}{
a character vector giving the \bold{folder} path in which all the Web of    Science files to be imported into a data frame can be found. This folder   should contain \bold{only} the files to be imported.
}
}
\details{
No details for now.
}
\value{
A data frame of 62 columns and a number of row equal to the number of unique records.
}
\references{
No references for now.
}
\author{
Maxime Rivest
}
\note{
No notes for now.
}
\seealso{
\code{\link{help}} for more information about general help systems
}
\examples{
\dontrun{Since this exemple shows how to load WOS from your system we need to run the following line to find the path to the exemple file}
fpath <- system.file("extdata", package="scimeetr")
fpath <- paste(fpath, "/wos_folder/", sep = "")
\dontrun{Then we can run the actual example}
wos_df <- import_wos_files(files_directory = fpath)
}
\keyword{ manip }