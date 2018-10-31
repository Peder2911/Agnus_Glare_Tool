#' Fuzzily Score Sentences 
#'
#' Scores each string in df[col] by calculating string distance between it and each string in 
#' lookup. Adds the best match and score to data frame.
#' @param df Data frame to apply to 
#' @param col Column in data frame to apply to 
#' @param lookup A character vector of strings to match against 
#' @param verbose print timing
#' @keywords Text processing 
#' @export
#' @examples
#' mtcars$nstring <- rownames(mtcars)
#' check <- c('Porch','Auto','Apples','Zanzibar')
#' fuzzScore(mtcars,'nstring',check) 

fuzzScore <- function(df,col,lookup,verbose = FALSE){
	
	mark1 <- Sys.time()

	sentences <- df[col] %>% unlist(use.names = FALSE)

	ncores <- parallel::detectCores() - 1
	cluster <- parallel::makeCluster(ncores,type = 'FORK')

	distances <- parallel::parSapply(cluster,sentences,function(s){
		stringdist::stringdist(s,lookup,nthread = 1)
		}) %>% as.data.frame(stringsAsFactors = FALSE)

	parallel::stopCluster(cluster)

	indices <- lapply(distances,function(c) which(c == min(c))[1]) %>% unlist()
	scores <- lapply(distances,function(c) min(c)) %>% unlist()
	orig <- names(distances)
	match <- lookup[indices]

	mark2 <- Sys.time()
	timediff <- mark2-mark1
	if(verbose){
		writeLines(paste('This took:',format(mindiff)))
		}

	cbind(df,data.frame(s = scores,m = match))

}


