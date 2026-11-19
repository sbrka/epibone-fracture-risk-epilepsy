epibone_entry_file <- normalizePath(sys.frame(1)$ofile, mustWork = TRUE)
epibone_entry_root <- dirname(epibone_entry_file)
Sys.setenv(AOU_EPIBONE_ROOT = epibone_entry_root)

source(file.path(epibone_entry_root, "R", "00_setup.R"), local = FALSE)
epibone_initialize()
source(file.path(epibone_entry_root, "R", "01_definitions.R"), local = FALSE)
source(file.path(epibone_entry_root, "R", "06_validate.R"), local = FALSE)
epibone_package_safe_results()
