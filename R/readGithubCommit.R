### readGithubCommit.R --- 
#----------------------------------------------------------------------
## author: Brice Ozenne
## created: okt  3 2017 (12:01) 
## Version: 
## last-updated: okt  3 2017 (15:38) 
##           By: Brice Ozenne
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

#' @title Existing commits
#' 
#' @description Get the existing commits corresponding to a repo on Github
#'
#' @param repo Repository address in the format username/repo
#' @param keep.author should the author be returned
#' @param keep.name should the name of each commit be returned
#' @param keep.time should the time at which the commit was done be returned
#' @param seq.commit the sequence of commit to return
#' @param rev.commit shouidl the order of the commit be reverested (last becomes first).
#' @param trace should the progression in geting the name/time of the commit be displayed
#' @param ... additional arguments to be passed to \code{read_html}.
#'  
#' @keywords function github
#' @examples 
#' readGithubCommit("bozenne/BuyseTest")
#' readGithubCommit("bozenne/BuyseTest",rev.commit = TRUE)
#'
#' @export
readGithubCommit <- function(repo, seq.commit = 1:5, rev.commit = FALSE,
                              keep.author = TRUE, keep.name = TRUE, keep.time = TRUE,  trace = TRUE, ...){
  
  # as in devtools:::github_remote
  meta <- parse_git_repo(repo)
  meta <- github_resolve_ref(do.call(`%||%`, list(meta$ref,"master")), meta)
  if (is.null(meta$username)) {
      stop("Unknown username. Possible mispecification of repo. Have you included the user name?")
  }
 
  #### get all commits
  pathURL <- file.path("https://github.com",meta$username,meta$repo,"commits")
  links <- readHtmlLinks(pathURL, ...)
  
  # rm travis
  index.travis <- grep("https://travis-ci.org/",links)
  if(length(index.travis)>0){
    links <- links[-index.travis]
  }
  
  pathURL <- file.path(meta$username,meta$repo,"tree")
  index.commits <- grep(pathURL,links, value = FALSE)
  author.commits <- links[index.commits - 2]
  author.commits <- unlist(lapply(strsplit(author.commits, split = "author="),"[[",2))
  
  commits <- links[index.commits]
  commits <- unlist(lapply(strsplit(commits, split = "tree/"),"[[",2))
  
  if(rev.commit){commits <- rev(commits)}
  seq.commit <- intersect(seq.commit, seq_len(length(commits)))
  author.commits <- author.commits[seq.commit]
  commits <- commits[seq.commit]
  n.commits <- length(commits)
  
  #### get additional informations
  if(keep.name || keep.time){ 
    
    name.commits <- vector(length = n.commits)
    time.commits <- vector(length = n.commits)
    
    if(trace){
      cat("Find the name of the ",paste(seq.commit, collapse = " "),if(rev.commit){" first"}else{" last"}," commits \n", sep = "")
      pb <- utils::txtProgressBar(0, n.commits, style = 3)
      on.exit(close(pb))
    }
    
      for(iterC in 1:n.commits){
      pathURL <- file.path("https://github.com",meta$username,meta$repo,"commit",commits[iterC])
      
       commit.content <- xml2::read_html(pathURL, ...)
       
        name.commit <- grep("property=\"og:title\"",commit.content %>>% rvest::html_nodes("meta"), value = TRUE)
        name.commits[iterC] <- strsplit(gsub("<meta content=\"","",name.commit), split = " . ")[[1]][1]
        time.commit <- grep("property=\"og:updated_time\"",commit.content %>>% rvest::html_nodes("meta"), value = TRUE)
        time.commit <- strsplit(gsub("<meta content=\"","",time.commit), split = "\" property=")[[1]][1]
        time.commits[iterC] <- as.character(zoo::as.Date(as.numeric(time.commit)/(24*3600))) # must be zoo and not base
      
      if(trace){utils::setTxtProgressBar(pb,iterC)}
    }
  }
 
  
  #### export
  if(keep.name){commits <- cbind(commits, name = name.commits)}
  if(keep.time){commits <- cbind(commits, time = time.commits)}
  if(keep.author){commits <- cbind(commits, author = author.commits)}
  
  return(commits)
}

#----------------------------------------------------------------------
### readGithubCommit.R ends here
