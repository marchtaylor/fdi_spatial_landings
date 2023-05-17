# extract mesh size ranges
mesh_size_extractor <- function(data) {
    mesh_size_split <- strsplit(data$mesh_size_range, "D")
    mesh_sizes <- suppressWarnings(as.numeric(do.call("c", mesh_size_split)))
    mesh_size_range <- range(unique(mesh_sizes), na.rm = TRUE)
    mesh_size_split <- lapply(mesh_size_split, FUN = function(x) {
        if (length(x) < 2) {
            c(mesh_size_range)
        } else {
            x
        }
    })
    mesh_size_split <- lapply(mesh_size_split, FUN = function(x) {
        if (x[2] == "XX") {
            c(x[1], mesh_size_range[2])
        } else {
            x
        }
    })
    data$mesh_size_min <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x) {
        x[1]
    })))
    data$mesh_size_max <- as.numeric(unlist(lapply(mesh_size_split, FUN = function(x) {
        x[2]
    })))
    return(data)
}