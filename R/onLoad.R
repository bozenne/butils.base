'.onAttach' <- function(lib, pkg="butils") {
  desc <- utils::packageDescription(pkg)
  packageStartupMessage(desc$Package, " version ",desc$Version)  
  options(error = function()revTraceback(max.lines = 5))
}

## installGithubCommit
`remotes_parse_git_repo` <- get("parse_git_repo", envir = asNamespace("remotes"), inherits = FALSE)
`remotes_github_commit` <- get("github_commit", envir = asNamespace("remotes"), inherits = FALSE)
`remotes_build_url` <- get("build_url", envir = asNamespace("remotes"), inherits = FALSE)
`remotes_download` <- get("download", envir = asNamespace("remotes"), inherits = FALSE)
`remotes_source_pkg` <- get("source_pkg", envir = asNamespace("remotes"), inherits = FALSE)

