app_dir <- function(app = NULL, create = FALSE, dir = data_dir()) {
    path <- dir_create(file.path(dir, "apps"))
    if (is.null(app)) {
        path
    } else {
        path <- file.path(path, app)
        if (create) dir_create(path) else path
    }
}

bin_dir <- function(dir = data_dir()) dir_create(file.path(dir, "bin"))
