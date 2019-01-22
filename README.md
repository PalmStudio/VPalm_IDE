
<!-- README.md is generated from README.Rmd. Please edit that file -->
VPalm IDE
=========

Introduction
------------

VPalm IDE is used to compute architectural parameters from field data, call the VPalm automaton using those parameters and to build 3D plant mock-ups arranged in a scene. The output files are in OPF (Open Plant Format) and OPS (OPen Scene) format.

The data preparation, the models and all details are described in the [PhD of R. Perez](https://www.researchgate.net/publication/318351549_Analyzing_and_modelling_the_genetic_variability_of_aerial_architecture_and_light_interception_of_the_oil_palm_Elaeis_guineensis_Jacq?_sg=KZ3K7bz0sNnV3iRwaYehauoZ0rn6Lc0MDO3O3tyXR-j9QzT4ODe9zb6ySgPOAXzoJhHXloiAo7CA5SYE8TsD7dd3SldfET7k8Iy-CuKf.cKm7AfKFCz6-jBiO4jl27VWnrz_HvA_KI0RNwUQgIQfQzoL2Dj9HLol3pv95Qb9vzkq7AoICAb4IH77DV3rgAQ) and the subsequent articles (Perez et al. 2016; Perez 2017; Perez, Costes, et al. 2018; Perez, Dauzat, et al. 2018; Perez et al. 2017)

Program details
---------------

VPalm IDE is a R Shiny application bundled in an [Electron](https://electronjs.org/) application. This method was used following the work of [Katie Sasso](https://github.com/ksasso) on this [repository](https://github.com/ksasso/Electron_ShinyApp_Deployment).

Install
-------

To build the electron app, you need to download and install [noede.js](https://nodejs.org/en/download/), and to restart your computer. Then, clone this repository, install the app and run it for the first time following these steps:

``` bash
# Clone this repository
git clone https://github.com/VEZY/VPalm_app
# Go into the repository
cd VPalm_app
# Install dependencies
npm install
# Run the app
npm start
```

The installation adds a new folder called `node_modules` into your project.

Build
-----

If the app is running with no issue, then you can build the electron application:

``` bash
npm run package-win
```

if it does not work, it is probably because the packager is not installed. try to run this command, and re-run the previous one:

``` bash
npm install -g electron-packager
```

run
---

A new folder called `VPalm_IDE` appeared on the project. The electron application is in there. To start the application, find the `.exe` file and click on it. That's it ! This executable is a standalone Shiny application.

Share
-----

You can share the application just by copy/pasting it into a USB key or by deploying it on the cloud. You might want to compress it beforehand though.

Build Windows installer
-----------------------

To build windows installer, we use the [windows-installer app](https://github.com/electron/windows-installer). Simply follow these instructions to build it:

Install the package if not already done:

``` bash
 npm install --save-dev electron-winstaller
```

And build the installer using node:

``` bash
 node build.js
```

Remember to first build the app (`npm run package-win`) before building the installer.

NB: Several interesting build options are available for the installer, see [the project repository](https://github.com/electron/windows-installer) for more details. These options should be added to the `settings` variable in `build.js`.

References
----------

Perez, Raphaël. 2017. “Analyzing and Modelling the Genetic Variability of Aerial Architecture and Light Interception of Oil Palm (Elaeis Guineensis Jacq).” Thesis.

Perez, Raphaël, Evelyne Costes, Frédéric Théveny, Sébastien Griffon, Jean-Pierre Caliman, and Jean Dauzat. 2018. “3D Plant Model Assessed by Terrestrial Lidar and Hemispherical Photographs: A Useful Tool for Comparing Light Interception Among Oil Palm Progenies.” *Agricultural and Forest Meteorology* 249: 250–63. <doi:%5Bhttps://doi.org/10.1016/j.agrformet.2017.11.008%5D(https://doi.org/https://doi.org/10.1016/j.agrformet.2017.11.008)>.

Perez, Raphaël, Jean Dauzat, Benoît Pallas, Julien Lamour, Philippe Verley, Jean-Pierre Caliman, Evelyne Costes, and Robert Faivre. 2018. “Designing Oil Palm Architectural Ideotypes for Optimal Light Interception and Carbon Assimilation Through a Sensitivity Analysis of Leaf Traits.” Journal Article. *Annals of Botany* 121 (5): 909–26. <doi:%5B10.1093/aob/mcx161%5D(https://doi.org/10.1093/aob/mcx161)>.

Perez, Raphaël, Benoît Pallas, Gilles Le Moguédec, Hervé Rey, Griffon Sébastien, Jean-Pierre Caliman, Evelyne Costes, and Jean Dauzat. 2016. “Integrating Mixed-Effect Models into an Architectural Plant Model to Simulate Inter-and Intra-Progeny Variability: A Case Study on Oil Palm (Elaeis Guineensis Jacq.).” *Journal of Experimental Botany* 67 (June): erw203. <doi:%5B10.1093/jxb/erw203%5D(https://doi.org/10.1093/jxb/erw203)>.

Perez, Raphaël, Benoît Pallas, Griffon Sébastien, Hervé Rey, J.P. Caliman, G Le Moguédec, Jean Dauzat, and Evelyne Costes. 2017. “Reconstructing Three-Dimensional Oil Palm Architecture from Allometric Relationships.” *Acta Horticulturae* 1160 (May): 11–18. <doi:%5Bhttps://doi.org/10.17660/ActaHortic.2017.1160.3%5D(https://doi.org/https://doi.org/10.17660/ActaHortic.2017.1160.3)>.
