# *BlenderFDS* code repository

This is the open user interface for [NIST Fire Dynamics Simulator (FDS)](https://pages.nist.gov/fds-smv/), as an addon for [Blender](http://www.blender.org).

 * **Learn** how to use this tool on the [wiki pages](https://github.com/firetools/blenderfds/wiki).
 * **Discuss** about its usage on its [discussion group](https://groups.google.com/g/blenderfds).
 * File **issues** on the [issue tracker](https://github.com/firetools/blenderfds/issues). 

---

The development of *BlenderFDS* is funded by a grant from the [Italian Ministry of Foreign Affairs and International Cooperation](https://www.esteri.it/mae/en/).
By the research project *WUIFI-21* (High fidelity computational fluid dynamics modeling of forest fires for Wildland-Urban Interface communities resilience and protection) the participating organizations intend to extend the capabilities of FDS on the prediction of wildland-urban interface fires propagation.

![MAECI](https://github.com/firetools/qgis2fds/wiki/images/MAECI.png)


## How to install this development version

1. Download and install [Blender 2.9x](http://www.blender.org) on your computer;

2. Download the BlenderFDS addon from this [GitHub repository](https://github.com/firetools/blenderfds/archive/master.zip);

3. Rename both the downloaded .zip file and the folder within the .zip file to `blenderfds`;

4. Launch Blender and open the `Edit > Preferences` menu. Then install and enable the downloaded addon as described in [Blender Manual](https://docs.blender.org/manual/en/dev/editors/preferences/addons.html?highlight=addon#).

## How to stay on the edge of development

1. `git clone` this repository into the `blender/2.9x/scripts/addons/blenderfds` directory of your Blender install. No need to zip it.

2. Launch Blender and open the `Edit > Preferences` menu. Search for the `BlenderFDS` Addon and enable it.

3. To follow the development: `git update` your local repository, and relaunch Blender.
