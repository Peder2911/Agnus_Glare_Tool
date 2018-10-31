#' Spread Text Rows 
#'
#' This function applies FUN to each text cell in col, which returns a list of results.
#' This list is then spread into separate rows, each retaining the original values in the
#' other rows. Originally used to split sentences using a tokenizer.
#' @param FUN Function to apply
#' @param col Column to apply to
#' @param ... Arguments to FUN
#' @keywords Text processing 
#' @export
#' @examples
#' SpreadTextRows(iris$species,FUN) 

spreadTextRows <- function(df,col = 'body',FUN, ...){

        dfs <- apply(df,1,function(row){

                text <- FUN(row[col], ...)%>%
			unlist()
		text <- text[text != '']

                otherNames <- names(row)[names(row)!=col]
                other <- row[otherNames]%>%
                        lapply(rep,length(text))
                
                out <- data.frame(other,stringsAsFactors = FALSE)
		out[[col]] <- text
		out

                        })
        dfs <- dfs %>%
        	dplyr::bind_rows()
	dfs
                }
   
