#' get EBI url for an RUN fastq directory
#' 
#' @param run_accessions character() vector of SRR, DRR, or ERR accessions
#' 
#' @return named vector with the parent directory 
#' containing the fastq files for each accession
#' 
#' @examples 
#' accession2url(c('SRR000237','SRR123456'))
#' 
#' 
#' @export
accession2url <- function(run_accessions) {
  run_accessions = unique(run_accessions)
  prefix <- "ftp://ftp.sra.ebi.ac.uk/vol1/fastq"
  dir1 <- paste0("/",substr(run_accessions,1,6))
  dir2 <- ifelse(nchar(run_accessions) == 9, "",
                 ifelse(nchar(run_accessions) == 10, paste0("/00",substr(run_accessions,10,10)),
                        ifelse(nchar(run_accessions) == 11, paste0("/0",substr(run_accessions,10,11)),
                               paste0("/",substr(run_accessions,10,12)))))
  vals = paste0(prefix,dir1,dir2,"/",run_accessions,"/")
  names(vals) = run_accessions
  vals
}

#' return a data.frame directory listing 
#' 
#' Given a set of ftp locations, return the 
#' directory listing as a data.frame.
#' 
#' Note that this function must loop over the URLs, 
#' so it may be slow and inefficient. 
#' 
#' @param url character() vector of ftp urls
#' 
#' @importFrom curl curl
#'
#' @examples 
#' ftp_dir_listing(accession2url(c('SRR000237','SRR000238', 'SRR925811')))
#'
#' @export
ftp_dir_listing <- function(url) {
  do.call(rbind, lapply(url, function(u) {
    tmp = read.table(curl(u), header=FALSE)
    tmp$url = paste0(u,tmp[,ncol(tmp)])
    tmp
  }))
}
