
# Faux XL Charting in R (fxl)

The _fxl_ package was designed to support transparent, replicable, and efficently-drawn figure in single-case research design. In addition to be easily archived, effortlessly drawn, and automated, the R ecosystem supports the drawing of publication-quality figures that exceed traditional tools (e.g., Microsoft Excel). For instance, the figures output by _fxl_ can be lossless by default using a variety of vector-based drawing formats as well as publication-quality rasters.

Features include:
 - pixel-perfect phase change lines (yes, across different plots/facets)
 - Support for various single-subject designs (reversal, multiple-baseline, multielement)
 - Various annotations common in single-subject designs (e.g., arrows, brackets, style condition changes)
 - Options to output figures in various formats
 - Functional programming style (modeled after ggplot)

### Version
------
1.0.0 (beta)

### Changelog
------
* 1.0.0 - Initial submission to CRAN

### Features/Usage
------
Publication-quality annotations and flexibility

Annotated, Hybrid Designs (MBD + Individual Reversals)
![Gilroy et al 2019](https://github.com/miyamot0/fxl/blob/main/man/figures/annotatedfigure2.svg?raw=true)

Concurrent Reversal Designs (Across Participants)
![Gilroy et al 2021](https://github.com/miyamot0/fxl/blob/main/man/figures/concurrentfigure.svg?raw=true)

Multiple Baseline Designs
![Gilroy et al 2015](https://github.com/miyamot0/fxl/blob/main/man/figures/multiplebaselinefigure.svg?raw=true)

Multi-element Designs/Functional Analyses 
![Gilroy et al 2019](https://github.com/miyamot0/fxl/blob/main/man/figures/fafigure.svg?raw=true)
