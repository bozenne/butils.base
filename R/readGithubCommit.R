### readGithubCommit.R --- 
#----------------------------------------------------------------------
## author: Brice Ozenne
## created: okt  3 2017 (12:01) 
## Version: 
## last-updated: May 12 2024 (11:51) 
##           By: Brice Ozenne
##     Update #: 45
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

## * readGithubCommit (documentation)
#' @title Existing commits
#' @description Get the existing commits corresponding to a repo on Github
#'
#' @param repo Repository address in the format username/repo
#' @param seq.commit the sequence of commit to return
#' @param rev.commit shouidl the order of the commit be reverested (last becomes first).
#' @param keep.author should the author be returned
#' @param keep.name should the name of each commit be returned
#' @param keep.time should the time at which the commit was done be returned
#' @param trace should the progression in geting the name/time of the commit be displayed
#' @param ... additional arguments to be passed to \code{read_html}.
#'  
#' @examples
#' readGithubCommit("bozenne/BuyseTest")
#' readGithubCommit("bozenne/BuyseTest", rev.commit = TRUE)
#' 

## * readGithubCommit (code)
#' @export
readGithubCommit <- function(repo, seq.commit = 1:5, rev.commit = FALSE, 
                             keep.author = TRUE, keep.name = TRUE, keep.time = TRUE,  trace = TRUE, ...){
  
    ## ** extract information (remotes:::install_remote)
    host <- "https://api.github.com"
    out <- remotes_parse_git_repo(repo)
    username <- out$username
    package <- out$repo

    ## check that the repo exists
    lastCommit <- remotes_github_commit(username = username, repo = package, host = host)
 
    ## ** get all commits
    pathURL <- file.path("https://github.com",username,package,"commits")
    links <- readHtmlLinks(pathURL, xpath = ".//a", ...)
  
    ## ##  ** remove travis
    ## index.travis <- grep("https://travis-ci.org/", links)
    ## if(length(index.travis)>0){
    ## links <- links[-index.travis]
    ## }

    ## ** keep only links containing with the username and package
    ## tree is chosen instead of commit to remove duplicates and avoid FP
    pathURL <- file.path(username, package, "tree")
    index.commits <- grep(pathURL, links, value = FALSE)
    tree.commits <- links[index.commits]

    ## ** reorder commit
    if(rev.commit){
        tree.commits <- rev(tree.commits)
    }

    ## ** only select a subset of comits
    tree.commits <- tree.commits[seq.commit]
    n.commits <- length(tree.commits)
    commits <- data.frame(commits = unlist(lapply(strsplit(tree.commits, split = "tree/"),"[[",2)))

    ## ** get additional informations
    if(keep.name || keep.time){ 
    
        name.commits <- vector(length = n.commits, mode = "character")
        time.commits <- vector(length = n.commits, mode = "character")
        author.commits <- vector(length = n.commits, mode = "character")
    
        if(trace){
            cat("Find the name of each commit \n", sep = "")
            pb <- utils::txtProgressBar(0, n.commits, style = 3)
            on.exit(close(pb))
        }
    
        for(iterC in 1:n.commits){
            pathURL <- file.path("https://github.com",username,package,"commit",commits[iterC,"commits"])
            commit.content <- xml2::read_html(pathURL, ...)
            commit.content2  <- xml2::xml_find_all(commit.content, xpath = ".//meta") ## rvest::html_nodes(commit.content, "meta")

            xml2::as_list(xml2::read_xml("<foo> a <b /><c><![CDATA[<d></d>]]></c></foo>"))
            name.commit <- grep("property=\"og:title\"", commit.content2, value = TRUE)
            name.commits[iterC] <- strsplit(gsub("<meta property=\"og:title\" content=\"","",name.commit), split = " . ")[[1]][1]
            time.commit <- grep("property=\"og:updated_time\"", commit.content2, value = TRUE)
            time.commit <- strsplit(gsub("<meta property=\"og:updated_time\" content=\"","",time.commit), split = "\"")[[1]][1]
            time.commits[iterC] <- as.character(zoo::as.Date(as.numeric(time.commit)/(24*3600))) # must be zoo and not base
      
            if(trace){utils::setTxtProgressBar(pb,iterC)}
        }
    }
 
  
  #### export
  if(keep.name){commits <- cbind(commits, name = name.commits)}
  if(keep.author){commits <- cbind(commits, author = author.commits)}
  if(keep.time){commits <- cbind(commits, time = time.commits)}
  
  return(commits)
}


#----------------------------------------------------------------------
### readGithubCommit.R ends here
