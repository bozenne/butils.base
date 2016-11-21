'.onAttach' <- function(lib, pkg="butils") {
  desc <- utils::packageDescription(pkg)
  packageStartupMessage(desc$Package, " version ",desc$Version)
}



`%||%` <- get("%||%", envir = asNamespace("devtools"), inherits = FALSE)
different_sha <- get("different_sha", envir = asNamespace("devtools"), inherits = FALSE)
github_remote <- get("github_remote", envir = asNamespace("devtools"), inherits = FALSE)
github_resolve_ref <- get("github_resolve_ref", envir = asNamespace("devtools"), inherits = FALSE)
install_packages <- get("install_packages", envir = asNamespace("devtools"), inherits = FALSE)
is.remote <- get("is.remote", envir = asNamespace("devtools"), inherits = FALSE)
is_windows <- get("is_windows", envir = asNamespace("devtools"), inherits = FALSE)
local_sha <- get("local_sha", envir = asNamespace("devtools"), inherits = FALSE)
parse_git_repo <- get("parse_git_repo", envir = asNamespace("devtools"), inherits = FALSE)
remote_download <- get("remote_download", envir = asNamespace("devtools"), inherits = FALSE)
remote_metadata <- get("remote_metadata", envir = asNamespace("devtools"), inherits = FALSE)
remote_package_name <- get("remote_package_name", envir = asNamespace("devtools"), inherits = FALSE)
remote_sha <- get("remote_sha", envir = asNamespace("devtools"), inherits = FALSE)
source_pkg <- get("source_pkg", envir = asNamespace("devtools"), inherits = FALSE)

