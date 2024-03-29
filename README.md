
<!-- README.md is generated from README.Rmd. Please edit that file -->

# VPalm IDE<img src="www/logo.png" alt="logo" width="150" align="right" />

## Introduction

VPalm IDE is used to compute architectural parameters from field data,
call the VPalm automaton using those parameters and to build 3D plant
mock-ups arranged in a scene. The output files are in OPF (Open Plant
Format) and OPS (OPen Scene) format.

The data preparation, the models and all details are described in the
[PhD of R.
Perez](https://www.researchgate.net/publication/318351549_Analyzing_and_modelling_the_genetic_variability_of_aerial_architecture_and_light_interception_of_the_oil_palm_Elaeis_guineensis_Jacq?_sg=KZ3K7bz0sNnV3iRwaYehauoZ0rn6Lc0MDO3O3tyXR-j9QzT4ODe9zb6ySgPOAXzoJhHXloiAo7CA5SYE8TsD7dd3SldfET7k8Iy-CuKf.cKm7AfKFCz6-jBiO4jl27VWnrz_HvA_KI0RNwUQgIQfQzoL2Dj9HLol3pv95Qb9vzkq7AoICAb4IH77DV3rgAQ)
and the subsequent articles (Perez et al. 2016; Perez 2017; Perez,
Costes, et al. 2018; Perez, Dauzat, et al. 2018; Perez et al. 2017)

## Program details

VPalm IDE is an R Shiny application bundled in an
[Electron](https://electronjs.org/) application. This method was used
following the work of [Katie Sasso](https://github.com/ksasso) on this
[repository](https://github.com/ksasso/Electron_ShinyApp_Deployment),
and improved to add an installer for windows users.

## Install

### User

To install VPALM\_IDE, simply download this repository wherever you
want, and download the data folder. The data folder is available to
PT-SMART partners only, or upon request (need clearance from PT-SMART +
CIRAD). For partners, just download the “0-data” folder from Alfresco,
and put it at the root of this folder.

### Developer

To build the electron app, you need to download and install
[node.js](https://nodejs.org/en/download/), and to restart your
computer. Then, you need to download [R-portable]() so your application
does not depends on external ressources. You can also follow this
[link](https://sourceforge.net/projects/rportable/files/latest/download)
that points to the latest release of the project.

Then, clone this repository:

``` bash
# Clone this repository
git clone https://github.com/PalmStudio/VPALM_IDE
# Go into the repository
cd VPALM_IDE
```

Unzip the R-portable archive into the root of the project.

And now you can install the app and run it for the first time following
these steps:

``` bash
# Install dependencies
npm install
# Run the app
npm start
```

The installation adds a new folder called `node_modules` into your
project.

## Build

### Windows

If the app is running with no issue, then you can build the electron
application:

``` bash
npm run package-win
```

### Linux

``` bash
npm run package-linux
```

### Mac

``` bash
npm run package-mac
```

### All at once

``` bash
npm run package-all
```

NB: not tested yet.

### Issues

If it does not work, it is probably because the packager is not
installed. try to run this command, and re-run the previous one:

``` bash
npm install -g electron-packager
```

## run

A new folder called `builds` appeared on the project. The electron
application you just built is in there. To start the application, find
the corresponding execution file (*e.g.* the windows `.exe` file) and
start it. That’s it ! The folder were the executable lies is a
standalone Shiny application.

## Share

You can share the application just by copy/pasting it into a USB key or
by deploying it on the cloud. You might want to compress it beforehand
though.

## Build Windows installer

To build windows installer, we use the [windows-installer
app](https://github.com/electron/windows-installer). Simply follow these
instructions to build it:

Install the package if not already done:

``` bash
 npm install --save-dev electron-winstaller
 npm install --save electron-squirrel-startup
```

And build the installer using node:

``` bash
 node build.js
```

Remember to first build the app (`npm run package-win`) before building
the installer.

NB: Several interesting build options are available for the installer,
see [the project
repository](https://github.com/electron/windows-installer) for more
details. These options should be added to the `settings` variable in
`build.js`.

## Automatic updates

It is possible to add automatic updates to the app by using a remote
repository. To set it up, we can add the option `remoteReleases` (see
[here](https://github.com/electron/windows-installer) for more details)
and `remoteToken` if necessary. The link should point the releases
folders *i.e.* the `installer-x64` folder for our example. Then electron
will check for any updates automatically, dowload them and install them
if necessary. Further details are available on the [Squirrel.Windows
documentation](https://github.com/Squirrel/Squirrel.Windows/blob/master/docs/readme.md).
Also see
[this](https://stackoverflow.com/questions/42749972/how-to-make-a-simple-updater-for-electron-application)
and
[this](https://gist.github.com/Slauta/5b2bcf9fa1f6f6a9443aa6b447bcae05).

## Summary

To update the application and the installer, simply run these commands:

``` bash
npm install
npm run package-win
node build.js
```

## References

Perez, Raphaël. 2017. “Analyzing and Modelling the Genetic Variability
of Aerial Architecture and Light Interception of Oil Palm (Elaeis
Guineensis Jacq).” Thesis.

Perez, Raphaël, Evelyne Costes, Frédéric Théveny, Sébastien Griffon,
Jean-Pierre Caliman, and Jean Dauzat. 2018. “3D Plant Model Assessed by
Terrestrial Lidar and Hemispherical Photographs: A Useful Tool for
Comparing Light Interception Among Oil Palm Progenies.” *Agricultural
and Forest Meteorology* 249: 250–63.
[DOI](https://doi.org/https://doi.org/10.1016/j.agrformet.2017.11.008).

Perez, Raphaël, Jean Dauzat, Benoît Pallas, Julien Lamour, Philippe
Verley, Jean-Pierre Caliman, Evelyne Costes, and Robert Faivre. 2018.
“Designing Oil Palm Architectural Ideotypes for Optimal Light
Interception and Carbon Assimilation Through a Sensitivity Analysis of
Leaf Traits.” Journal Article. *Annals of Botany* 121 (5): 909–26. DOI:
[10.1093/aob/mcx161](https://doi.org/10.1093/aob/mcx161).

Perez, Raphaël, Benoît Pallas, Gilles Le Moguédec, Hervé Rey, Griffon
Sébastien, Jean-Pierre Caliman, Evelyne Costes, and Jean Dauzat. 2016.
“Integrating Mixed-Effect Models into an Architectural Plant Model to
Simulate Inter-and Intra-Progeny Variability: A Case Study on Oil Palm
(Elaeis Guineensis Jacq.).” *Journal of Experimental Botany* 67 (June):
erw203. DOI: [10.1093/jxb/erw203](https://doi.org/10.1093/jxb/erw203).

Perez, Raphaël, Benoît Pallas, Griffon Sébastien, Hervé Rey, J.P.
Caliman, G Le Moguédec, Jean Dauzat, and Evelyne Costes. 2017.
“Reconstructing Three-Dimensional Oil Palm Architecture from Allometric
Relationships.” *Acta Horticulturae* 1160 (May): 11–18.
[DOI](https://doi.org/https://doi.org/10.17660/ActaHortic.2017.1160.3).
