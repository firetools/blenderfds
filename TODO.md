# TODO

New BlenderFDS website with MAECI logo

user doc (WUIFI-21)

code snippets

## TODO Improvements


Import MULT

split MESH

progress_begin(min, max)

progress_update(value)

progress_end()

# DONE

Split extensions from lang (name?)


Explore app template
https://docs.blender.org/manual/en/latest/advanced/app_templates.html#app-templates


Verify: operators

Blender 2.9x

GEOM terrain new quality checks (WUIFI-21)

GEOM terrain toolbox (WUIFI-21)

GEOM bingeom path

Insert GEOREF in MISC and general file, set a checkbox to avoid exporting
North bearing of origin geolocation

unit testing (WUIFI-21): namelists, operators (by script)


Refactor to_fds in types and lang


Bug: if ob already present, renamed to ob.001 and bingeom is not found anymore
Does the same happen for MOVE?


GEOM MOVE import:
- Import MOVE namelist and set world matrix

active_object can be None, replace with object

automatic developer's doc with doxygen (WUIFI-21)

New free text when not existing

Set active object after deleting tmp obs

QUANTITY search in DEVC

Fix pixel geometry elevation

GEOM minimum face area default lower

Use 2.81 remesh tool

Replace "Quality" with "Sanity"

geometry caching, if ob not touched

file version management

default blender file

Search: MATL_ID in text, QUANTITY in text

Check FDS UG for namelists

Automatic path for external tools

Fixed logging

Improved FDS import parser

Unified formatting of parameters and namelist

voxelization fix for API change

show FDS code

show/hide geometry

tmp objects

fix center voxel/pixels

scale factor for units (length, time)

"global" to "world"

default SURFs

TRANSPARENCY

user preferences

simplification UI

refactor bf_other and UIList

op copy properties: bf_others difficulties

GEOM toolbox

GEOM geometry mod original object fix, tool panel?

logger

default appearance

from_fds, import

## DONE Geography

lon, lat, elevation of scene world origin and relative transformations

put cursor/object at coordinate (wgs84 lonlat, utm)

get coordinate at cursor/object (wgs84 lonlat, utm)
