#' Reshape the Header files
#'
#' @param hdr An output from \code{\link{read_dicom_header}}
#' @param keep_tags tags to keep in the data
#'
#' @return A wide \code{data.frame} of the information needed
#' for filtering
#' @export
#'
#' @importFrom tidyr spread
#' @importFrom dplyr mutate filter select distinct group_by
#' @importFrom dplyr ungroup slice
wide_hdr = function(
  hdr,
  keep_tags = NULL
) {


  # (0040,a040)
  if (!is.null(keep_tags)) {
    hdr = hdr %>%
    filter(tag %in% keep_tags)
  }
  hdr = hdr %>%
    select(file, tag, name, value)


  # this should remove completely identical tags
  hdr = hdr %>%
    distinct()


  hdr = hdr %>%
    group_by(file, tag) %>%
    filter(!is.na(value)) %>%
    mutate(value = paste(value, collapse = "; ")) %>%
    dplyr::slice(1) %>%
    ungroup()

  hdr = hdr %>%
    group_by(file, name) %>%
    filter(!is.na(value)) %>%
    mutate(value = paste(value, collapse = "; ")) %>%
    mutate(ind = seq(n())) %>%
    dplyr::slice(1) %>%
    ungroup()


  wide = hdr %>%
    select(file, name, value) %>%
    spread(key = name, value = value)


  return(wide)
}
