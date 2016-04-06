
# This function takes a vector v and converts it to a list where the elements
# at positions given by hdr_indices are the headers. 
# 
# Elements before the first header are discarded (with a warning). 
# A NULL content is associated with headers with no subsequent elements.
# 
# Example:
# > nest_vector(c("Header 1", "1.1", "1.2", "Header 2", "2.1"), c(1, 4))
#
# $`Header 1`
# [1] "1.1" "1.2"
# $`Header 2`
# [1] "2.1"

nest_vector <- function(v, hdr_indices) {
    if (!is.numeric(hdr_indices)) stop("hdr_indices must be a numeric vector")
    hdr_indices <- sort(unique(hdr_indices))
    
    if (hdr_indices[1] > 1) 
        warning("elements before first header will be discarded")
    
    # End position of each sub-list
    end_indices <- c(hdr_indices[-1] - 1, length(v))
    
    list_out <- lapply(seq_along(hdr_indices), function(i) {
        if (hdr_indices[i] == end_indices[i]) {
            NULL
        } else {
            v[(hdr_indices[i] + 1):end_indices[i]]
        }
    })
    names(list_out) <- v[hdr_indices]
    list_out
}