#' Move Nextcloud Files
#'
#' @param paths_df dataframe with columns \code{raw} and \code{imported} with
#' full path to source and target file
#' @param overwrite overwrite files if these exist in target directory (default:
#' TRUE)
#' @param dbg debug messages (default: TRUE)
#'
#' @return moves file to target file
#' @export
#' @importFrom kwb.nextcloud move_file_or_folder

move_nextcloud_files <- function(paths_df,
                                 overwrite = FALSE,
                                 dbg = TRUE) {
  stopifnot(check_env_nextcloud())
  stopifnot(nrow(paths_df) != 0)
  res <- sapply(seq_len(nrow(paths_df)), function(idx) {
    kwb.nextcloud::move_file_or_folder(
      path = paths_df$raw[idx],
      path_target = paths_df$imported[idx],
      overwrite = overwrite,
      dbg = dbg
    )
  })
}