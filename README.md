# BlenderFDS code repository

The open user interface for Fire Dynamics Simulator (FDS), as an addon for Blender 2.8x.

This addon is *experimental*. Do not use for production, the file format may still change.

For more information, and the original BlenderFDS code visit http://www.blenderfds.org

![BlenderFDS logo](https://github.com/firetools/blenderfds/raw/gh-pages/images/blenderfds_128.png)

The development of BlenderFDS is funded by a grant from the [Italian Ministry of Foreign Affairs and International Cooperation](https://www.esteri.it/mae/en/).
By the research project *WUIFI-21* (High fidelity computational fluid dynamics modeling of forest fires for Wildland-Urban Interface communities resilience and protection) the participating organizations intend to extend the capabilities of FDS on the prediction of wildland-urban interface fires propagation.

![MAECI logo](https://github.com/firetools/blenderfds/raw/gh-pages/images/MAECI.png)

## How to install this development version

1. Download and install [Blender 2.8x](http://www.blender.org) on your computer;

2. Download the BlenderFDS addon from this [GitHub repository](https://github.com/firetools/blenderfds280/archive/master.zip);

3. Rename both the downloaded .zip file and the folder within the .zip file to `blenderfds28x`;

4. Launch Blender 2.8x and open the `Edit > Preferences` menu. Then install and enable the downloaded addon as described in [Blender Manual](https://docs.blender.org/manual/en/dev/editors/preferences/addons.html?highlight=addon#).

## How to stay on the edge of development

1. `git clone` this repository into the `blender/2.80/scripts/addons/blenderfds280x` directory of your Blender 2.8x install. No need to zip it.

2. Launch Blender 2.8x and open the `Edit > Preferences` menu. Search for the `BlenderFDS` Addon and enable it.

3. To follow the development: `git update` your local repository, and relaunch Blender 2.8x.
