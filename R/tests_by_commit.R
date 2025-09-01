
# This script checks each commit and runs testthat tests to identify problematic commits

library(testthat)
library(git2r)
library(jsonlite)

#' Close any open file handles that might lock files
close_file_handles <- function() {
  # Close any open connections
  tryCatch({
    # Close all open file connections
    all_cons <- showConnections(all = TRUE)
    if (nrow(all_cons) > 0) {
      sapply(as.numeric(rownames(all_cons)), function(x) {
        tryCatch(close(getConnection(x)), error = function(e) NULL)
      })
    }
    
    # Force garbage collection
    gc()
    
    # Small delay to allow file handles to release
    Sys.sleep(0.5)
  }, error = function(e) {
    # Silently continue if cleanup fails
  })
}

#' Safely checkout a commit with retry logic
safe_checkout <- function(repo, target, max_retries = 3) {
  for (attempt in 1:max_retries) {
    tryCatch({
      close_file_handles()
      checkout(repo, target, force = TRUE)
      return(TRUE)
    }, error = function(e) {
      if (attempt == max_retries) {
        stop("Failed to checkout after ", max_retries, " attempts. Error: ", e$message)
      }
      cat("Checkout attempt", attempt, "failed. Retrying in 2 seconds...\n")
      close_file_handles()
      Sys.sleep(2)
    })
  }
  return(FALSE)
}

#' Run tests for a specific commit and return results
#' 
#' @param repo_path Path to the git repository
#' @param commit_sha SHA of the commit to test
#' @return List with test results and metadata
run_tests_for_commit <- function(repo_path = ".", 
                                commit_sha) {
  
  cat("Testing commit:", commit_sha, "\n")
  
  # Store current branch/commit to restore later
  repo <- repository(repo_path)
  current_head <- commits(repo, n = 1)[[1]]$sha
  
  # Store original working directory
  original_wd <- getwd()

  tryCatch({
    # Ensure we're in the repo directory
    setwd(repo_path)

    # Safely checkout the specific commit
    safe_checkout(repo, commit_sha)
    
    # Clean up any existing file handles before running tests
    close_file_handles()

    # run tests
    results <- as.data.frame(test_dir('tests/testthat', stop_on_failure = F, reporter = 'minimal')) |> 
      select(file, context, test, failed)

    test_results <- tibble(
      success = !any(results$failed == 1),
      results = list(results)
    )
    
    # Clean up after tests
    close_file_handles()

    # Get commit metadata
    commit_obj <- lookup(repo, commit_sha)
    commit_info <- tibble(
      sha = commit_sha,
      date = as.character(commit_obj$author$when)
    )
    
    # Combine results
    out <- bind_cols(commit_info, test_results)
    
    return(out)
    
  }, error = function(e) {
    cat("Error during commit testing:", e$message, "\n")
    return(list(
      commit = list(sha = commit_sha)
    ))
  }, finally = {
    # Ensure we clean up file handles before restoring
    close_file_handles()
    
    # Restore original HEAD with retry logic
    tryCatch({
      safe_checkout(repo, current_head)
    }, error = function(e) {
      cat("Warning: Could not restore original HEAD. You may need to manually checkout your branch.\n")
      cat("Error:", e$message, "\n")
    })
    
    # Restore working directory
    setwd(original_wd)
  })
}

#' Get all commits in reverse chronological order
#' 
#' @param repo_path Path to the git repository
#' @param branch Branch to analyze (default: current branch)
#' @param max_commits Maximum number of commits to analyze (default: all)
#' @param max_date Character for date after which commits are not pulled
#' @param min_date Character for date before which commits are not pulled
#' @return Vector of commit SHAs
get_commit_history <- function(repo_path = ".", branch = NULL, max_commits = NULL, 
                              max_date = NULL, min_date = NULL) {
  repo <- repository(repo_path)

  # Get commits
  commits <- rev(commits(repo, n = max_commits))

  if(!is.null(max_date) | !is.null(min_date))
    dts <- lapply(commits, function(x) as.Date(as.POSIXct(x$author$when))) |> 
      unlist()

  # remove those after max_date if provided
  if(!is.null(max_date)){

    max_date <- as.Date(max_date)
    torm <- which(dts > max_date)
    commits <- commits[-torm]

  }

  # remove those before max_date if provided
  if(!is.null(min_date)){

    min_date <- as.Date(min_date)
    torm <- which(dts < min_date)
    commits <- commits[-torm]

  }

  commit_shas <- sapply(commits, function(x) x$sha)
  
  return(commit_shas)
}

#' Main function to run tests across commit history
#' 
#' @param repo_path Path to the git repository
#' @param max_commits Maximum number of commits to test
#' @param max_date Character for date beyond which commits are not pulled
#' @param min_date Character for date before which commits are not pulled
#' @param branch Specific branch to test (optional)
run_commit_test_analysis <- function(repo_path = ".", 
                                   max_commits = NULL,
                                   max_date = NULL,
                                   min_date = NULL,
                                   branch = NULL) {

  # Get commit history
  commits <- get_commit_history(repo_path, branch, max_commits, max_date, min_date)
  cat("Found", length(commits), "commits to test\n\n")
  
  # Initialize results tracking
  all_results <- list()

  # Test each commit
  for (i in seq_along(commits)) {
    commit_sha <- commits[i]
    cat("Progress:", i, "/", length(commits), "\n")
    
    result <- run_tests_for_commit(repo_path, commit_sha)
    all_results[[commit_sha]] <- result
    
    cat("\n")
  }

  # Generate summary report
  out <- bind_rows(all_results)
    
  return(out)
}

#' Clean up any temporary files or state
cleanup_repository_state <- function(repo_path = ".") {
  cat("Cleaning up repository state...\n")
  
  # Close file handles
  close_file_handles()
  
  # Check if we're in a detached HEAD state and offer to fix it
  repo <- repository(repo_path)
  
  tryCatch({
    current_branch <- repository_head(repo)
    if (is.null(current_branch) || current_branch$type != "local") {
      cat("Warning: You may be in a detached HEAD state.\n")
      cat("Consider running: git checkout main (or your default branch)\n")
    }
  }, error = function(e) {
    cat("Could not determine repository state.\n")
  })
}

# Run the analysis with your parameters
res <- run_commit_test_analysis('.', max_date = '2021-06-14', min_date = '2021-05-14')


# Function to safely test an app at a specific commit
test_app_at_commit <- function(commit_sha, app_file = "index.Rmd") {
  library(git2r)
  browser()
  repo <- repository(".")
  current_branch <- repository_head(repo)
  
  # Create a temporary branch for testing
  temp_branch_name <- paste0("temp-test-", substr(commit_sha, 1, 8))
  
  tryCatch({
    # Create and checkout temporary branch at the commit
    checkout(repo, commit_sha)
    branch_create(commit_sha, name = temp_branch_name)
    checkout(repo, temp_branch_name)
    
    # Now run the app
    cat("Testing app at commit:", commit_sha, "\n")
    cat("Temporary branch:", temp_branch_name, "\n")
    
    # Run your app
    rmarkdown::run(app_file)
    
  }, finally = {
    # Cleanup: return to original branch and delete temp branch
    checkout(repo, current_branch)
    tryCatch({
      branch_delete(repository_head(repo, "temp-test-"))
    }, error = function(e) {
      cat("Could not delete temporary branch:", temp_branch_name, "\n")
    })
  })
}
