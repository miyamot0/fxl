
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

*Annotated, Hybrid Designs (MBD + Individual Reversals)*

The figure below is from a single-subject evaluation conducted by [Gilroy et al. (2019)](https://doi.org/10.1080/17518423.2019.1646342) and drawn in *fxl* (originally drawn in R, later rolled into the package). This figure was drawn using a combination of multiple baseline logic with an added reversal condition to demonstrate the effect of functional communication training (in an operant behavioral economic evaluation).

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/annotatedplot.R)

![Gilroy et al 2019](https://github.com/miyamot0/fxl/blob/main/man/figures/annotatedfigure2.svg?raw=true)

*Concurrent Reversal Designs (Across Participants)*

The figure below is from a single-subject evaluation conducted by [Gilroy et al. (2021)](https://doi.org/10.1002/jaba.826) and drawn in *fxl* (also drawn in R and later rolled into the package). This figure was drawn using a combination of individual reversals, visualized in a way similar to multiple baseline figures (with condition changes reflected across all participants) to evaluate how degrees of demand elasticity influenced rates of behavior.

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/concurrentplot.R)

![Gilroy et al 2021](https://github.com/miyamot0/fxl/blob/main/man/figures/concurrentfigure.svg?raw=true)

*Multiple Baseline Designs*

The respective figure is drawn from a single-subject evaluation conducted by [Gilroy et al. (2015)](https://doi.org/10.1016/j.rasd.2015.04.004). These data were previously illustrated using spreadsheet software and are now drawn in *fxl*. This figure was drawn using a multiple baseline design, whereby control is illustrated through a sequential introduction of independent variables, and changes in relational responding (i.e., deictics) were observed. 

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/multiplebaselineplot.R)

![Gilroy et al 2015](https://github.com/miyamot0/fxl/blob/main/man/figures/multiplebaselinefigure.svg?raw=true)

*Semi-logarithmic (Celeration) Charting with Annotations*

Although I've never used on in practice (behavior observed at rates ~1000x/day are pretty uncommon), _fxl_ supports plotting in log-linear space. This is often down when changes across orders of magnitude are more easily understood in terms of relative change. This is demonstrated with a toy simulation of data that occurs at low (0-2x/session) and high rates (~100-200x/session).

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/semilogplot.R)

![Example Data](https://github.com/miyamot0/fxl/blob/main/man/figures/semilogfigure.svg?raw=true)

*Concurrent Choice/Initial Link Selections*

The respective figure is drawn from a single-subject evaluation conducted by [Lozy et al. (2020)](https://doi.org/10.1002/jaba.677). These data were previously illustrated using spreadsheet software and are now drawn in *fxl*.

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/cumulativeplot.R)

![Lozy et al 2020](https://github.com/miyamot0/fxl/blob/main/man/figures/cumulativefigure.svg?raw=true)

*Multi-element Designs/Functional Analyses*

The last example is constructed from a functional analysis of severe behavior conducted in a study by [Gilroy et al. (2015)](https://doi.org/10.1016/j.rasd.2015.04.004). As with the other 2015 work, these data were previously illustrated using spreadsheet software and are now drawn in *fxl*. This figure was drawn using a multielement design, whereby the differential effects of contingencies are evaluated with respect to rates of behavior. 

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/faplot.R)

![Gilroy et al 2019](https://github.com/miyamot0/fxl/blob/main/man/figures/fafigure.svg?raw=true)

*Multi-element Designs/Functional Analyses with corresponding Integrity*

This is the same example as the one above, but with a corresponding bar series to illustrate integrity of sessions. This is often used in parent- or teacher-implemented FA's, when the fidelity of procedures is also a likely factor.

[Code to Draw Figure](https://github.com/miyamot0/fxl/blob/main/demo/faplotintegrity.R)

![Gilroy et al 2019](https://github.com/miyamot0/fxl/blob/main/man/figures/fafigureintegrity.svg?raw=true)

### Referenced Works (F/OSS software)

The fxl package uses a number of open source projects to work properly:

* TeachingDemos - Artistic 2.0 Licensed. [Site](https://cran.r-project.org/web/packages/TeachingDemos/index.html)

### Installation

The fxl package must be installed manually at this time, see below: 

library(devtools)

install_github("miyamot0/fxl")

### Development

Want to contribute? Great! Emails or PR's (worthwhile ones) are welcome.

### Todos

* Condensing methods

* Individually-keyed styles

### License

fxl - Copyright 2021, Shawn P. Gilroy. GPL-Version 2+
