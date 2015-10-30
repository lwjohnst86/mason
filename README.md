# Use masonry, build a (data) structure!

Using a standard interface, create common data results structures, such as from
a linear regression or correlation. Design the analysis, lay the foundation,
build the rough structure, and polish it up.

This project is still in development.

# Installation

So far this package can only be installed from GitHub. Use the code:

    devtools::install_github('lwjohnst86/mason')
    ## Load it by:
    library(mason)

# Problems?

If there are problems,
[create an issue](https://github.com/lwjohnst86/mason/issues) and descriptive
the problem.

# Typical usage

The typical usage for this package would flow like this:

    design(data_set, 'stat_technique') %>%
        lay_base() %>%
        build() %>%
        polish()

With each function having slightly different arguments depending on the
statistical technique used.

