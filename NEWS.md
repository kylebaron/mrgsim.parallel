# mrgsim.parallel (development version)

# mrgsim.parallel 0.3.0

- Fix anchor links to functions in other packages (#25). 

- Fixed bug in `tag` argument to `bg_mrgsim_d()`; simulations are now
  properly stored in `.locker/.tag` (#23). 

- Add support for saving background sims in parquet format (#22).

- Add method to create a new stream from a data frame (#20).


# mrgsim.parallel 0.2.0

- Add `bg_mrgsim_d()` to simulate data sets in background R processes (#16)

- Add `new_stream()` to create file stream objects (#16)

- Add tools for manipulating file stream objects: `locate_stream()`, 
  `format_stream()`, `ext_stream()` (#16)
  
- Add `file_set()` as a lower-level function for creating file sets (#16)

- Add `write_stream()` generic and methods for writing `fst`, `feather`, `qs`,
  `rds` file formats (#16)
  
- Add utilities for managing locker storage space: `setup_locker()`, 
  `reset_locker()`, `noreset_locker()`, `version_locker()` (#16)

- Add some utility functions for working with outputs written in `fst` format:
  `internalize_fst()`, `head_fst()`, `list_fst()` (#16)
