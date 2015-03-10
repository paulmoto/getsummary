#' Summarize an article
#' 
#' @param text character string to be summarized
#' @return two summaries of the text
#' @export
#'
#'
#'
require(openNLP)
require(NLP)
require(stringr)
require(igraph)

getsummary<-function(text)
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
  
  
  cosadj<-matrix(NA,length(sentences),length(sentences))
  #cosine distance = angle between vectors [0,pi/2]
  for(i in 1:nrow(cosadj)){
    for(j in 1:ncol(cosadj)){
      if (!i==j){cosadj[i,j]<-1/acos((dict[,i+1]%*%dict[,j+1])/((sqrt(sum(dict[,i+1]^2)))*(sqrt(sum(dict[,j+1]^2)))))}
    }
  }
  diag(cosadj)<-0
  
  
  g<-graph.adjacency(adj,weighted=T,mode='undirected')
  pgr<-page.rank(g)$vector
  summ<-data.frame(origsent,pgr)[order(-pgr),]
  #n sentence summary
  n<-3
  summary<-as.character(summ[1:n,1][order(row.names(summ))])
  summary<-summary[!is.na(summary)]
  
  g2<-graph.adjacency(cosadj,weighted=T,mode='undirected')
  pgr2<-page.rank(g2)$vector
  summ2<-data.frame(origsent,pgr2)[order(-pgr2),]
  #n sentence summary
  n<-3
  summary2<-as.character(summ2[1:n,1][order(row.names(summ2))])
  summary2<-summary2[!is.na(summary2)]
  
  return(cat("Summary 1: ",summary,"\n\n", "Summary 2: ",summary2))
}