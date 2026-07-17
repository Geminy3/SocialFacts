#' @export
.onLoad <- function(lib, pkg) {
  # Disable OpenMP parallelism to avoid pthread_mutex_init errors
  # This can occur when multiple packages try to use OpenMP simultaneously
  # or when thread limits are too restrictive
  Sys.unsetenv("OMP_NUM_THREADS")
  Sys.setenv("OMP_NUM_THREADS" = "1")
}
