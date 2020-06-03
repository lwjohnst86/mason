# mason 0.3.0

## New features

- Added a Partial Least Squares design with settings and construction
- Added tidying/scrubbing for PLS analysis (#31)

## Bug fixes

- Fix bugs introduced by latest dplyr version 1.0.0 and tibble version

## Improvements

- Re-factored the underlying `gather-group_by-do` method of running models on each
combination of x and y. Now uses `purrr::map()` over a list of all possible formulas.
Computing time and memory usage is much much lower now (#22).
- Remove old code

# mason 0.2.6

## Bug fixes and other changes

- Fixed changes to new `mutate` functions from dplyr version update
- Removed assertive package dependency
- Fixed broom package changes

# mason 0.2.5

## Hotfixes

* Fixed printing issue so that printing shows a `tibble`.

## Additions

* Added a `NEWS.md` file to track changes to the package.




