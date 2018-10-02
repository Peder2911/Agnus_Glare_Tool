#' A function for turning a body-data.frame into a sentence-data.frame 
#'
#' This function turns a data.frame containing multiple-sentence text, into a 
#' data frame containing the sentences on separate rows, while keeping metadata.
#' @param col Which column contains the sentences? 
#' @keywords NLP 
#' @export
#' @examples
#' as.sentence.df() 

as.sentence.df <- function(df,col = 'body'){

        print(names(df))
        dfs <- apply(df,1,function(row){

                sentence <- row[col]%>%
                        tokenizers::tokenize_sentences(simplify = TRUE)

                notSent <- names(row)[names(row)!=col]

                rest <- row[notSent]%>%
                        lapply(rep,length(sentence))
                
                out <- data.frame(rest,sentence,stringsAsFactors = FALSE)

                        })
        dfs <- dfs %>%
        bind_rows()
                }
   
