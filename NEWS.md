# mrgsim.parallel (development version)

- Add `bg_mrgsim_d()` to simulate data sets in background R processes (#16)

- Add `new_stream()` to create file stream objects (#16)

- Add tools for manipulating file stream objects: `locate_stream()`, 
  `format_stream()`, `ext_stream()` (#16)
  
- Add `file_set()` as a lower-level function for creating file sets (#16)

- Add `write_stream()` generic and methods for writing `fst`, `feather`, `qs`,
  `rds` file formats (#16)
  
- Add utilities for managing locker storage space: `setup_locker()`, 
  `reset_locker()`, `noreset_locker()` (#16)

- Add some utility functions for working with outputs written in `fst` format:
  `internalize_fst()`, `head_fst()`, `list_fst()` (#16)
  