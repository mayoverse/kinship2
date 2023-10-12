# NEWS

NEWS file for the Pedixplorer package

## Changes in version 0.99.0

- Kinship2 is renamed to Pedixplorer and hosted on Bioconductor.
- Pedigree is now a S4 object, all functions are updated to work with
the new class
- Pedigree constructor now takes a data.frame as input for the Pedigree
informations and for the special relationship.
The two data.fram are normalized before being used.
- plot.pedigree support ggplot generation, mark and label can be added
to the plot.
The plot is now generated in two steps ped_to_plotdf() and plot_fromdf().
This allows the user to modify the plot before it is generated.
- All documentation are now generated with Roxygen
- New function available: generate_aff_inds, generate_colors,
is_informative, min_dist_inf, normData, num_child, useful_inds
- All functions renamed to follow the snake\_case convention
- All parameters renamed to follow the snake\_case convention
- All test now use testthat files
- Vignettes have been updated to reflect the new changes
