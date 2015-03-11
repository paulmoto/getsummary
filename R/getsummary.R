#' Summarize an article
#' 
#' @param text character string to be summarized
#' @param n number of sentences in summary
#' @return two summaries of the text
#' @export
#'
#'
#'
require(openNLP)
require(NLP)
require(stringr)
require(igraph)

getsummary<-function(text,arg2)
{
  
  text<-str_replace_all(text,"[\n]"," ")
  sent_annotate<-Maxent_Sent_Token_Annotator()
  sent<-annotate(text,sent_annotate)
  origsent<-vector()
  for(i in 1:length(sent)){
    origsent[i]<-substr(text,sent$start[i],sent$end[i])
  }
  
  sentences<-str_replace_all(tolower(origsent),"[[:punct:]]","")
  origsent<-origsent[duplicated(sentences)==F]
  sentences<-sentences[duplicated(sentences)==F]
  sword<- strsplit(sentences," ")
  words<- unique(unlist(sword))
  words<-words[!words==""]
  
  dict<-data.frame(words)
  for (i in 1:length(sword)){
    for(j in 1:nrow(dict)){
      dict[j,i+1]<-length(grep(sprintf("^%s$",dict[j,1]),sword[[i]]))
    }
  }
  
  adj<-matrix(NA,length(sentences),length(sentences))
  
  #Jaccard Similarity: intersect (dict[,i],dict[,j]) / union (dict[,i],dict[,j])
  for(i in 1:nrow(adj)){
    for(j in 1:ncol(adj)){int<-vector()
                          uni<-vector()
                          for (n in 1:nrow(dict)) {
                            int[n]<- min(dict[n,i+1],dict[n,j+1])
                            uni[n]<- max(dict[n,i+1],dict[n,j+1])
                          }
                          adj[i,j]<-sqrt(sum(int^2))/sqrt(sum(uni^2))}
  }
  diag(adj)<-1
  
  
  
  
  g<-graph.adjacency(adj,weighted=T,mode='undirected')
  pgr<-page.rank(g)$vector
  summ<-data.frame(origsent,pgr)[order(-pgr),]
  #n sentence summary

  summary<-as.character(summ[1:arg2,1][order(row.names(summ))])
  summary<-summary[!is.na(summary)]
    
  return(cat("Summary 1: ",summary))
}