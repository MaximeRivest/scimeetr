import_wos_files <-
function (files_directory) 
{
    WOS_file <- NULL
    folder_content <- list.files(files_directory)
    files_quantity <- length(folder_content)
    for (files in 1:files_quantity) {
        full_file_path <- paste(files_directory, folder_content[files], 
            sep = "")
        if (files == 1) {
            fileName <- full_file_path
            v_char <- readChar(full_file_path, file.info(full_file_path)$size)
            v_char <- iconv(v_char, from = "UTF-8", to = "ASCII", 
                sub = "")
            writeChar(v_char, con = full_file_path, eos = NULL)
            WOS_table <- read.delim2(full_file_path, header = T, 
                fileEncoding = "ASCII", row.names = NULL, quote = "", 
                stringsAsFactors = FALSE, comment.char = "")
        }
        else {
            fileName <- full_file_path
            v_char <- readChar(full_file_path, file.info(full_file_path)$size)
            v_char <- iconv(v_char, from = "UTF-8", to = "ASCII", 
                sub = "")
            writeChar(v_char, con = full_file_path, eos = NULL)
            WOS_table_temp <- read.delim2(full_file_path, header = T, 
                fileEncoding = "ASCII", row.names = NULL, quote = "", 
                stringsAsFactors = FALSE, comment.char = "")
            WOS_table <- rbind(WOS_table, WOS_table_temp)
        }
    }
    column_names <- names(WOS_table)[2:length(names(WOS_table))]
    WOS_table <- WOS_table[, 1:(ncol(WOS_table) - 1)]
    names(WOS_table) <- column_names
    WOS_table <- WOS_table[!duplicated(WOS_table), ]
    return(WOS_table)
}
