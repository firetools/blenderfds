# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

import re, os.path, logging, math  # FIXME remove math with CYL

import bpy
from bpy.types import (
    bpy_struct,
    PropertyGroup,
    UIList,
    Operator,
    Object,
    Scene,
    Material,
    Collection,
)
from bpy.props import (
    BoolProperty,
    FloatProperty,
    IntProperty,
    IntVectorProperty,
    StringProperty,
    PointerProperty,
    EnumProperty,
    CollectionProperty,
)
from .types import (
    BFException,
    BFNotImported,
    BFParam,
    BFParamXB,
    BFParamXYZ,
    BFParamPB,
    BFParamStr,
    BFParamFYI,
    BFParamOther,
    BFNamelist,
    BFNamelistSc,
    BFNamelistOb,
    BFNamelistMa,
    FDSParam,
    FDSNamelist,
    FDSCase,
)
from . import geometry, gis, fds, io, config
from .utils import BFException, BFNotImported, is_iterable

log = logging.getLogger(__name__)

# General collections

bf_namelists = list()
bf_params = list()
bl_classes = list()  # list of all Blender classes that need registering
bf_classes = list()  # list of all BlenderFDS classes that need registering

bf_namelists_by_cls = dict()  # dict of all BFNamelist classes by cls name
bf_namelists_by_fds_label = dict()  # dict of all BFNamelist classes by fds_label


def subscribe(cls):
    """!
    Subscribe BFNamelist, BFParam, or bpy_struct class to related collections for registration.
    @param cls: class to be registered.
    """

    if issubclass(cls, BFNamelist):
        bf_namelists.append(cls)  # FIXME still used?
        bf_namelists_by_cls[cls.__name__] = cls
        if cls.fds_label:
            bf_namelists_by_fds_label[cls.fds_label] = cls
    elif issubclass(cls, BFParam):
        bf_params.append(cls)  # FIXME still used?
    elif issubclass(cls, bpy_struct):
        bl_classes.append(cls)
    else:
        bf_classes.append(cls)
    return cls


# Prepare for "Other" and "List of filepaths" parameters
# PropertyGroup and UIList
# The PG properties should always be: bf_export, name


@subscribe
class WM_PG_bf_other(PropertyGroup):
    """!
    Blender PropertyGroup for items of 'other' FDS parameters.
    """

    bf_export: BoolProperty(name="Export", default=True)
    name: StringProperty(name="Name")


@subscribe
class WM_UL_bf_other_items(UIList):
    """!
    Blender UIList for items of 'other' FDS parameters.
    """

    def draw_item(self, context, layout, data, item, icon, active_data):
        col = layout.column()
        col.active = item.bf_export
        col.prop(item, "name", text="", emboss=False, icon_value=icon)
        col = layout.column()
        col.prop(item, "bf_export", text="")


@subscribe
class WM_PG_bf_filepaths(PropertyGroup):
    """!
    Blender PropertyGroup for items of 'filepaths' FDS parameters.
    """

    bf_export: BoolProperty(name="Export", default=False)
    name: StringProperty(name="Name", subtype="FILE_PATH")


@subscribe
class WM_UL_bf_filepaths_items(UIList):
    """!
    Blender UIList for items of 'filepaths' FDS parameters.
    """

    def draw_item(self, context, layout, data, item, icon, active_data):
        col = layout.column()
        col.active = item.bf_export
        col.prop(item, "name", text="", emboss=False, icon_value=icon)
        col = layout.column()
        col.prop(item, "bf_export", text="")


# Case config


@subscribe
class SP_config_directory(BFParam):
    """!
    Blender representation for the destination directory of the exported case.
    """

    label = "Case Directory"
    description = "Destination directory for exported case"
    bpy_type = Scene
    bpy_idname = "bf_config_directory"
    bpy_prop = StringProperty
    bpy_other = {"subtype": "DIR_PATH", "maxlen": 1024}

    @property
    def exported(self):
        return bool(self.element.bf_config_directory)

    def check(self, context):
        if self.exported:
            value = self.element.bf_config_directory
            if not os.path.exists(bpy.path.abspath(value)):
                raise BFException(self, f"Case directory <{value}> not existing")


@subscribe
class SP_config_text(BFParam):
    """!
    Blender representation for the internal free text, included verbatim.
    """

    label = "Free Text"
    description = "Internal free text, included verbatim"
    bpy_type = Scene
    bpy_idname = "bf_config_text"
    bpy_prop = PointerProperty
    bpy_other = {"type": bpy.types.Text}

    def draw_operators(self, context, layout):
        layout.operator("scene.bf_show_text", text="", icon="GREASEPENCIL")


@subscribe
class SN_config(BFNamelistSc):
    """!
    Blender representation for the FDS Case Config.
    """

    label = "FDS Case Config"
    bf_params = (SP_config_directory, SP_config_text)


# Config sizes


@subscribe
class SP_config_min_edge_length_export(BFParam):
    """!
    Blender representation to use custom min allowed edge length.
    """

    label = "Use Custom Min Edge Length"
    description = "Use custom min allowed edge length for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_edge_length_export"
    bpy_prop = BoolProperty
    bpy_default = False


@subscribe
class SP_config_min_edge_length(BFParam):
    """!
    Blender representation for the min allowed edge length.
    """

    label = "Min Edge Length"
    description = "Min allowed edge length for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_edge_length"
    bpy_prop = FloatProperty
    bpy_default = 1e-05
    bpy_other = {"unit": "LENGTH"}
    bpy_export = "bf_config_min_edge_length_export"


@subscribe
class SP_config_min_face_area_export(BFParam):
    """!
    Blender representation to use custom min allowed face area.
    """

    label = "Use Custom Min Face Area"
    description = "Use custom min allowed face area for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_face_area_export"
    bpy_prop = BoolProperty
    bpy_default = False


@subscribe
class SP_config_min_face_area(BFParam):
    """!
    Blender representation for the min allowed face area.
    """

    label = "Min Face Area"
    description = "Min allowed face area for current case"
    bpy_type = Scene
    bpy_idname = "bf_config_min_face_area"
    bpy_prop = FloatProperty
    bpy_default = 1e-07
    bpy_other = {"unit": "AREA"}
    bpy_export = "bf_config_min_face_area_export"


@subscribe
class SP_config_default_voxel_size(BFParam):
    """!
    Blender representation for the default voxel/pixel resolution.
    """

    label = "Voxel/Pixel Size"
    description = "Default voxel/pixel resolution"
    bpy_type = Scene
    bpy_idname = "bf_default_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {"unit": "LENGTH", "step": 1.0, "precision": 3}


@subscribe
class SN_config_sizes(BFNamelistSc):
    """!
    Blender representation for the default Sizes and Thresholds.
    """

    label = "Default Sizes and Thresholds"
    bf_params = (
        SP_config_min_edge_length,
        SP_config_min_face_area,
        SP_config_default_voxel_size,
    )


# Config units


@subscribe
class SN_config_units(BFNamelistSc):
    """!
    Blender representation for the units.
    """

    label = "Units"

    def draw(self, context, layout):
        sc = self.element
        unit = sc.unit_settings
        col = layout.column()
        col.prop(unit, "system")
        col = col.column()
        col.enabled = unit.system != "NONE"
        col.prop(unit, "scale_length")
        col.prop(unit, "use_separate")
        col.prop(unit, "length_unit", text="Length")
        # col.prop(unit, "mass_unit", text="Mass")  # Unused
        # col.prop(unit, "time_unit", text="Time")  # Unused


# HEAD/TAIL


@subscribe
class SP_HEAD_CHID(BFParam):
    """!
    Blender representation of the CHID parameter, the case identificator, also used as case filename.
    """

    label = "CHID"
    description = "Case identificator, also used as case filename"
    fds_label = "CHID"
    bpy_type = Scene
    bpy_idname = "name"

    def copy_to(self, dest_element):
        pass


@subscribe
class SP_HEAD_TITLE(BFParamFYI):
    """!
    Blender representation of the TITLE parameter, the case description.
    """

    label = "TITLE"
    description = "Case description"
    fds_label = "TITLE"
    bpy_type = Scene
    bpy_idname = "bf_head_title"
    bpy_other = {"maxlen": 64}


@subscribe
class SN_HEAD(BFNamelistSc):
    """!
    Blender representation of the HEAD namelist.
    """

    label = "HEAD"
    description = "Case header"
    enum_id = 3001
    fds_label = "HEAD"
    bpy_export = "bf_head_export"
    bpy_export_default = True
    bf_params = SP_HEAD_CHID, SP_HEAD_TITLE


@subscribe
class SN_TAIL(BFNamelistSc):
    """!
    Blender representation of the TAIL namelist.
    For importing only.
    """

    label = "TAIL"
    description = "Case closing"
    enum_id = 3010
    fds_label = "TAIL"

    def to_fds_namelist(self, context):
        pass

    def from_fds(self, context, fds_namelist):
        pass


# TIME


@subscribe
class SP_TIME_setup_only(BFParam):
    """!
    Blender representation to set Smokeview to setup only geometry.
    """

    label = "Smokeview Geometry Setup"
    description = "Set Smokeview to setup only geometry"
    bpy_type = Scene
    bpy_idname = "bf_time_setup_only"
    bpy_prop = BoolProperty
    bpy_default = False

    def to_fds_param(self, context):
        if self.element.bf_time_setup_only:
            return FDSParam(
                fds_label="T_END", value=0.0, msg="Smokeview setup only", precision=1,
            )


@subscribe
class SP_TIME_T_BEGIN(BFParam):
    """!
    Blender representation of the T_BEGIN parameter, the simulation starting time.
    """

    label = "T_BEGIN [s]"
    description = "Simulation starting time"
    fds_label = "T_BEGIN"
    fds_default = 0.0
    bpy_type = Scene
    bpy_idname = "bf_time_t_begin"
    bpy_prop = FloatProperty
    bpy_other = {"step": 100.0, "precision": 1}  # "unit": "TIME", not working

    @property
    def exported(self):
        return super().exported and not self.element.bf_time_setup_only


@subscribe
class SP_TIME_T_END(BFParam):
    """!
    Blender representation of the T_END parameter, the simulation ending time.
    """

    label = "T_END [s]"
    description = "Simulation ending time"
    fds_label = "T_END"
    bpy_type = Scene
    bpy_idname = "bf_time_t_end"
    bpy_prop = FloatProperty
    bpy_default = 1.0
    bpy_other = {"step": 100.0, "precision": 1}  # "unit": "TIME", not working

    @property
    def exported(self):
        return super().exported and not self.element.bf_time_setup_only


@subscribe
class SP_TIME_other(BFParamOther):
    """!
    Blender representation of other parameters for the TIME namelist.
    """

    bpy_type = Scene
    bpy_idname = "bf_time_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class SN_TIME(BFNamelistSc):
    """!
    Blender representation of the TIME namelist.
    """

    label = "TIME"
    description = "Simulation time settings"
    enum_id = 3002
    fds_label = "TIME"
    bpy_export = "bf_time_export"
    bpy_export_default = True
    bf_params = SP_TIME_T_BEGIN, SP_TIME_T_END, SP_TIME_setup_only, SP_TIME_other


# MISC


@subscribe
class SP_MISC_FYI(BFParamFYI):
    """!
    Blender representation of the FYI parameter.
    """

    bpy_type = Scene
    bpy_idname = "bf_misc_fyi"


@subscribe
class SP_MISC_OVERWRITE(BFParam):
    """!
    Blender representation of the OVERWRITE parameter.
    """

    label = "OVERWRITE"
    description = "Do not check for the existence of CHID.out and overwrite files"
    fds_label = "OVERWRITE"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_misc_overwrite"


@subscribe
class SP_MISC_THICKEN_OBSTRUCTIONS(BFParam):
    """!
    Blender representation of the THICKEN_OBSTRUCTIONS parameter, to not allow thin sheet obstructions.
    """

    label = "THICKEN_OBSTRUCTIONS"
    description = "Do not allow thin sheet obstructions"
    fds_label = "THICKEN_OBSTRUCTIONS"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_misc_thicken_obstructions"


@subscribe
class SP_origin_geoname(BFParam):
    """!
    Blender representation for the origin location geographic name.
    """

    label = "Origin Geoname"
    description = "Origin location geographic name"
    bpy_type = Scene
    bpy_idname = "bf_origin_geoname"
    bpy_prop = StringProperty
    bpy_export = "bf_origin_export"
    bpy_export_default = False

    def draw(self, context, layout):
        sc = self.element
        url = gis.LonLat(lon=sc.bf_origin_lon, lat=sc.bf_origin_lat).to_url()
        active = self.element.bf_origin_export
        col = layout.column(align=True, heading="Origin Geolocation")
        row = col.row(align=True)
        row.active = active
        row.prop(sc, "bf_origin_export", text="")
        row.prop(sc, "bf_origin_geoname", text="")
        row.operator("wm.url_open", text="", icon="URL").url = url
        if active:
            col.prop(sc, "bf_origin_elevation", text="Elevation")
            col.prop(sc, "bf_origin_lon")
            col.prop(sc, "bf_origin_lat")
            col.prop(sc, "bf_origin_north_bearing")

    def to_fds_param(self, context):
        if self.element.bf_origin_export and self.element.bf_origin_geoname:
            return FDSParam(msg=f"Origin at: <{self.element.bf_origin_geoname}>")


@subscribe
class SP_origin_elevation(BFParam):
    """!
    Blender representation for the Elevation of world origin.
    """

    label = "Origin Elevation"  # ORIGIN_ELEVATION
    description = "Elevation of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_elevation"
    bpy_prop = FloatProperty
    bpy_export = "bf_origin_export"
    bpy_default = 0.0
    bpy_other = {"unit": "LENGTH", "precision": 4}

    def draw(self, context, layout):
        pass


def update_lonlat(self, context):
    """!
    Update the UTM of the context starting from longitude and latitude of the scene.
    @param context: the Blender context.
    """
    sc = context.scene
    utm = gis.LonLat(sc.bf_origin_lon, sc.bf_origin_lat).to_UTM()
    sc["bf_origin_utm_zn"] = utm.zn  # avoid triggering another update
    sc["bf_origin_utm_ne"] = utm.ne
    sc["bf_origin_utm_easting"] = utm.easting
    sc["bf_origin_utm_northing"] = utm.northing


@subscribe
class SP_ORIGIN_LON(BFParam):
    """!
    Blender representation for the longitude (WGS84, EPSG:4326) of world origin in decimal degrees.
    """

    label = "ORIGIN_LON"
    description = "Longitude (WGS84, EPSG:4326) of world origin in decimal degrees"
    fds_label = "ORIGIN_LON"
    bpy_type = Scene
    bpy_idname = "bf_origin_lon"
    bpy_prop = FloatProperty
    bpy_export = "bf_origin_export"
    bpy_default = 0.0
    bpy_other = {"min": -180.0, "max": 180.0, "precision": 6, "update": update_lonlat}

    def draw(self, context, layout):
        pass


@subscribe
class SP_ORIGIN_LAT(BFParam):
    """!
    Blender representation for the latitude (WGS84, EPSG:4326) of world origin in decimal degrees.
    """

    label = "ORIGIN_LAT"
    description = "Latitude (WGS84, EPSG:4326) of world origin in decimal degrees"
    fds_label = "ORIGIN_LAT"
    bpy_type = Scene
    bpy_idname = "bf_origin_lat"
    bpy_prop = FloatProperty
    bpy_export = "bf_origin_export"
    bpy_default = 0.0
    bpy_other = {"min": -80.0, "max": 84.0, "precision": 6, "update": update_lonlat}

    def draw(self, context, layout):
        pass


@subscribe
class SP_origin_utm_zn(BFParam):
    """!
    Blender representation for the UTM Zone Number (WGS84) of world origin.
    """

    label = "Origin UTM Zone Number"
    description = "UTM Zone Number (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_zn"
    bpy_prop = IntProperty
    bpy_default = 32
    bpy_other = {"min": 1, "max": 60}


@subscribe
class SP_origin_utm_ne(BFParam):
    """!
    Blender representation for the UTM northern emisphere (WGS84) of world origin.
    """

    label = "Origin UTM Northern Emisphere"
    description = "UTM northern emisphere (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_ne"
    bpy_prop = BoolProperty
    bpy_default = True


@subscribe
class SP_origin_utm_easting(BFParam):
    """!
    Blender representation for the UTM easting (WGS84) of world origin.
    """

    label = "Origin UTM Easting"
    description = "UTM easting (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_easting"
    bpy_prop = FloatProperty
    bpy_default = 500000.0
    bpy_other = {"unit": "LENGTH", "min": 0, "max": 1000000}


@subscribe
class SP_origin_utm_northing(BFParam):
    """!
    Blender representation for the UTM northing (WGS84) of world origin.
    """

    label = "Origin UTM Northing"
    description = "UTM northing (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_northing"
    bpy_prop = FloatProperty
    bpy_default = 5000000.0
    bpy_other = {"unit": "LENGTH", "min": 0, "max": 10000000}


@subscribe
class SP_ORIGIN_NORTH_BEARING(BFParam):
    """!
    Blender representation for the NORTH_BEARING parameter.
    """

    label = "NORTH_BEARING"
    description = "Angle between the geographical north and the world Y axis"
    fds_label = "NORTH_BEARING"
    bpy_type = Scene
    bpy_idname = "bf_origin_north_bearing"
    bpy_prop = FloatProperty
    bpy_export = "bf_origin_export"
    fds_default = 0.0
    bpy_other = {
        "min": -180.0,
        "max": 180.0,
        "precision": 1,
    }

    def draw(self, context, layout):
        pass


@subscribe
class SP_MISC_other(BFParamOther):
    """!
    Blender representation of other parameters for the MISC namelist.
    """

    bpy_type = Scene
    bpy_idname = "bf_misc_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class SN_MISC(BFNamelistSc):
    """!
    Blender representation of the MISC namelist.
    """

    label = "MISC"
    description = "Miscellaneous parameters"
    enum_id = 3003
    fds_label = "MISC"
    bpy_export = "bf_misc_export"
    bpy_export_default = False
    bf_params = (
        SP_MISC_FYI,
        SP_MISC_OVERWRITE,
        SP_MISC_THICKEN_OBSTRUCTIONS,
        SP_origin_geoname,
        SP_origin_elevation,
        SP_ORIGIN_LON,
        SP_ORIGIN_LAT,
        SP_ORIGIN_NORTH_BEARING,
        SP_MISC_other,
    )


# REAC


@subscribe
class SP_REAC_ID(BFParamStr):
    """!
    Blender representation of the ID string parameter, the identificator of the reaction.
    """

    label = "ID"
    description = "Identificator of the reaction"
    fds_label = "ID"
    bpy_type = Scene
    bpy_idname = "bf_reac_id"


@subscribe
class SP_REAC_FYI(BFParamFYI):
    """!
    Blender representation of the FYI parameter.
    """

    bpy_type = Scene
    bpy_idname = "bf_reac_fyi"


@subscribe
class SP_REAC_FUEL(BFParamStr):
    """!
    Blender representation of the FUEL string parameter, the identificator of fuel species.
    """

    label = "FUEL"
    description = "Identificator of fuel species"
    fds_label = "FUEL"
    bpy_type = Scene
    bpy_idname = "bf_reac_fuel"


@subscribe
class SP_REAC_FORMULA(BFParamStr):
    """!
    Blender representation of the FORMULA string parameter, the chemical formula of fuel species, it can only contain C, H, O, or N.
    """

    label = "FORMULA"
    description = "Chemical formula of fuel species, it can only contain C, H, O, or N"
    fds_label = "FORMULA"
    bpy_type = Scene
    bpy_idname = "bf_reac_formula"


@subscribe
class SP_REAC_CO_YIELD(BFParam):
    """!
    Blender representation of the CO_YIELD parameter, the fraction of fuel mass converted into carbon monoxide.
    """

    label = "CO_YIELD [kg/kg]"
    description = "Fraction of fuel mass converted into carbon monoxide"
    fds_label = "CO_YIELD"
    fds_default = 0.0
    bpy_type = Scene
    bpy_prop = FloatProperty
    bpy_idname = "bf_reac_co_yield"
    bpy_other = {"step": 1.0, "precision": 3, "min": 0.0, "max": 1.0}


@subscribe
class SP_REAC_SOOT_YIELD(SP_REAC_CO_YIELD):
    """!
    Blender representation of the SOOT_YIELD parameter, the fraction of fuel mass converted into smoke particulate.
    """

    label = "SOOT_YIELD [kg/kg]"
    description = "Fraction of fuel mass converted into smoke particulate"
    fds_label = "SOOT_YIELD"
    bpy_type = Scene
    bpy_idname = "bf_reac_soot_yield"


@subscribe
class SP_REAC_HEAT_OF_COMBUSTION(BFParam):
    """!
    Blender representation of the HEAT_OF_COMBUSTION parameter, the fuel heat of combustion.
    """

    label = "HEAT_OF_COMBUSTION [kJ/kg]"
    description = "Fuel heat of combustion"
    fds_label = "HEAT_OF_COMBUSTION"
    fds_default = 0.0
    bpy_type = Scene
    bpy_idname = "bf_reac_heat_of_combustion"
    bpy_prop = FloatProperty
    bpy_other = {"precision": 1, "min": 0.0}


@subscribe
class SP_REAC_IDEAL(BFParam):
    """!
    Blender representation of the IDEAL parameter to set ideal heat of combustion.
    """

    label = "IDEAL"
    description = "Set ideal heat of combustion"
    fds_label = "IDEAL"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_reac_ideal"


@subscribe
class SP_REAC_RADIATIVE_FRACTION(BFParam):
    """!
    Blender representation of the RADIATIVE_FRACTION parameter, the fraction of the total combustion energy that is released in the form of thermal radiation
    """

    label = "RADIATIVE_FRACTION"
    description = (
        "Fraction of the total combustion energy that is released "
        "in the form of thermal radiation"
    )
    fds_label = "RADIATIVE_FRACTION"
    fds_default = 0.35
    bpy_type = Scene
    bpy_idname = "bf_reac_radiative_fraction"
    bpy_prop = FloatProperty
    bpy_other = {"precision": 2, "min": 0.0, "max": 1.0}


@subscribe
class SP_REAC_other(BFParamOther):
    """!
    Blender representation of other parameters for the REAC namelist.
    """

    bpy_type = Scene
    bpy_idname = "bf_reac_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class SN_REAC(BFNamelistSc):
    """!
    Blender representation of the REAC namelist.
    """

    label = "REAC"
    description = "Reaction"
    enum_id = 3004
    fds_label = "REAC"
    bpy_export = "bf_reac_export"
    bpy_export_default = False
    bf_params = (
        SP_REAC_ID,
        SP_REAC_FUEL,
        SP_REAC_FYI,
        SP_REAC_FORMULA,
        SP_REAC_CO_YIELD,
        SP_REAC_SOOT_YIELD,
        SP_REAC_HEAT_OF_COMBUSTION,
        SP_REAC_IDEAL,
        SP_REAC_RADIATIVE_FRACTION,  # moved from RADI
        SP_REAC_other,
    )


# RADI


@subscribe
class SP_RADI_FYI(BFParamFYI):
    """!
    Blender representation of the FYI parameter.
    """

    bpy_type = Scene
    bpy_idname = "bf_radi_fyi"


@subscribe
class SP_RADI_RADIATION(BFParam):
    """!
    Blender representation of the RADIATION parameter to turn on/off the radiation solver.
    """

    label = "RADIATION"
    description = "Turn on/off the radiation solver"
    fds_label = "RADIATION"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_radi_radiation"


@subscribe
class SP_RADI_NUMBER_RADIATION_ANGLES(BFParam):
    """!
    Blender representation of the NUMBER_RADIATION_ANGLES parameter, the number of angles for spatial resolution of radiation solver.
    """

    label = "NUMBER_RADIATION_ANGLES"
    description = "Number of angles for spatial resolution of radiation solver"
    fds_label = "NUMBER_RADIATION_ANGLES"
    fds_default = 100
    bpy_type = Scene
    bpy_idname = "bf_radi_number_radiation_angles"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_TIME_STEP_INCREMENT(BFParam):
    """!
    Blender representation of the TIME_STEP_INCREMENT parameter, the frequency of calls to the radiation solver in time steps.
    """

    label = "TIME_STEP_INCREMENT"
    description = "Frequency of calls to the radiation solver in time steps"
    fds_label = "TIME_STEP_INCREMENT"
    fds_default = 3
    bpy_type = Scene
    bpy_idname = "bf_radi_time_step_increment"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_ANGLE_INCREMENT(BFParam):
    """!
    Blender representation of the ANGLE_INCREMENT parameter, the increment over which the angles are updated.
    """

    label = "ANGLE_INCREMENT"
    description = "Increment over which the angles are updated"
    fds_label = "ANGLE_INCREMENT"
    fds_default = 5
    bpy_type = Scene
    bpy_idname = "bf_radi_angle_increment"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_RADIATION_ITERATIONS(BFParam):
    """!
    Blender representation of the RADIATION_ITERATIONS parameter, the number of times the radiative intensity is updated in a time step.
    """

    label = "RADIATION_ITERATIONS"
    description = "Number of times the radiative intensity is updated in a time step"
    fds_label = "RADIATION_ITERATIONS"
    fds_default = 1
    bpy_type = Scene
    bpy_idname = "bf_radi_radiation_iterations"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_RADI_other(BFParamOther):
    """!
    Blender representation of other parameters for the RADI namelist.
    """

    bpy_type = Scene
    bpy_idname = "bf_radi_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class SN_RADI(BFNamelistSc):
    """!
    Blender representation of the RADI namelist.
    """

    label = "RADI"
    description = "Radiation parameters"
    enum_id = 3006
    fds_label = "RADI"
    bpy_export = "bf_radi_export"
    bpy_export_default = False
    bf_params = (
        SP_RADI_FYI,
        SP_RADI_RADIATION,
        SP_RADI_NUMBER_RADIATION_ANGLES,
        SP_RADI_TIME_STEP_INCREMENT,
        SP_RADI_ANGLE_INCREMENT,
        SP_RADI_RADIATION_ITERATIONS,
        SP_RADI_other,
    )


# DUMP


@subscribe
class SP_DUMP_FYI(BFParamFYI):
    """!
    Blender representation of the FYI parameter.
    """

    bpy_type = Scene
    bpy_idname = "bf_dump_fyi"


@subscribe
class SP_DUMP_render_file(BFParam):
    """!
    Blender representation of the RENDER_FILE parameter to export geometric description file GE1.
    """

    label = "Export Geometric Description File"
    description = "Export geometric description file GE1"
    fds_label = "RENDER_FILE"
    bpy_type = Scene
    bpy_idname = "bf_dump_render_file"
    bpy_prop = BoolProperty
    fds_default = False

    def _get_ge1_filepath(self, sc):
        return io.calc_path(
            bl_path=sc.bf_config_directory or "//", name=sc.name, extension=".ge1",
        )

    def to_fds_param(self, context):
        if self.exported:
            # Save .ge1 file
            filepath = self._get_ge1_filepath(sc=self.element)
            ge1_text = geometry.to_ge1.scene_to_ge1(context, self)
            io.write_txt_file(filepath, ge1_text)
            return FDSParam(fds_label="RENDER_FILE", value=f"{self.element.name}.ge1")

    def set_value(self, context, value=None):
        self.element.bf_dump_render_file = bool(value)


@subscribe
class SP_DUMP_STATUS_FILES(BFParam):
    """!
    Blender representation of the STATUS_FILES parameter, the export status file (*.notready), deleted when the simulation is completed successfully.
    """

    label = "STATUS_FILES"
    description = "Export status file (*.notready), deleted when the simulation is completed successfully"
    fds_label = "STATUS_FILES"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_dump_status_files"


@subscribe
class SP_DUMP_NFRAMES(BFParam):
    """!
    Blender representation of the NFRAMES parameter, the number of output dumps per calculation.
    """

    label = "NFRAMES"
    description = "Number of output dumps per calculation"
    fds_label = "NFRAMES"
    fds_default = 1000
    bpy_type = Scene
    bpy_idname = "bf_dump_nframes"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}

    @property
    def exported(self):
        return super().exported and not self.element.bf_dump_frames_freq_export


@subscribe
class SP_DUMP_frames_freq(BFParam):
    """!
    Blender representation of the dump output every 1 s.
    """

    label = "Dump Output Frequency [s]"
    description = "Dump output frequency in seconds"
    bpy_type = Scene
    bpy_idname = "bf_dump_frames_freq"
    bpy_prop = FloatProperty
    bpy_other = {"min": 0.01, "precision": 2}
    bpy_default = 1.0
    bpy_export = "bf_dump_frames_freq_export"
    bpy_export_default = False

    def to_fds_param(self, context):
        if self.exported:
            nframes = int(
                (self.element.bf_time_t_end - self.element.bf_time_t_begin)
                // self.element.bf_dump_frames_freq
            )
            if nframes < 1:
                nframes = 1
            return FDSParam(fds_label="NFRAMES", value=nframes)


@subscribe
class SP_DUMP_DT_RESTART(BFParam):
    """!
    Blender representation of the DT_RESTART parameter, the time interval between restart files are saved.
    """

    label = "DT_RESTART [s]"
    description = "Time interval between restart files are saved"
    fds_label = "DT_RESTART"
    fds_default = 600
    bpy_type = Scene
    bpy_idname = "bf_dump_dt_restart"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}


@subscribe
class SP_DUMP_other(BFParamOther):
    """!
    Blender representation of other parameters for the DUMP namelist.
    """

    bpy_type = Scene
    bpy_idname = "bf_dump_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class SN_DUMP(BFNamelistSc):
    """!
    Blender representation of the DUMP namelist.
    """

    label = "DUMP"
    description = "Output parameters"
    enum_id = 3005
    fds_label = "DUMP"
    bpy_export = "bf_dump_export"
    bpy_export_default = False
    bf_params = (
        SP_DUMP_FYI,
        SP_DUMP_render_file,
        SP_DUMP_STATUS_FILES,
        SP_DUMP_NFRAMES,
        SP_DUMP_frames_freq,
        SP_DUMP_DT_RESTART,
        SP_DUMP_other,
    )


# CATF


@subscribe
class SP_CATF_check_files(BFParam):
    """!
    Blender representation to check file existence while exporting filepaths.
    """

    label = "Check File Existance While Exporting"
    description = "Check file existence while exporting filepaths"
    bpy_type = Scene
    bpy_idname = "bf_catf_check_files"
    bpy_prop = BoolProperty
    bpy_default = False

    def to_fds_param(self, context):
        pass


@subscribe
class SP_CATF_files(BFParamOther):
    """!
    Blender representation to concatenated files (eg. PROP='/drive/test.catf').
    """

    label = "Concatenated File Paths"
    description = "Concatenated files (eg. PROP='/drive/test.catf')"
    fds_label = "OTHER_FILES"
    bpy_type = Scene
    bpy_idname = "bf_catf_files"
    bpy_pg = WM_PG_bf_filepaths
    bpy_ul = WM_UL_bf_filepaths_items

    def to_fds_param(self, context):
        el = self.element
        coll = getattr(self.element, self.bpy_idname)
        result = list()
        for p in coll:
            if p.bf_export and p.name:
                if el.bf_catf_check_files and not io.is_file(p.name):
                    raise BFException(self, f"File path <{p.name}> does not exist")
                result.append(tuple((FDSParam(fds_label="OTHER_FILES", value=p.name),)))
        return tuple(result)  # multi

    def from_fds(self, context, value):
        if not value:
            self.set_value(context, None)
        elif isinstance(value, str):
            self.set_value(context, value)
        else:  # tuple of str
            for v in value:
                self.set_value(context, v)


@subscribe
class SN_CATF(BFNamelistSc):
    """!
    Blender representation of the CATF namelist.
    """

    label = "CATF"
    description = "Concatenated file paths"
    fds_label = "CATF"
    bpy_export = "bf_catf_export"
    bpy_export_default = False
    bf_params = SP_CATF_check_files, SP_CATF_files


# Material


def update_MP_namelist_cls(self, context):
    self.bf_namelist.set_appearance(context)


@subscribe
class MP_namelist_cls(BFParam):
    """!
    Blender representation to the identification of FDS namelist.
    """

    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Material
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (("MN_SURF", "SURF", "Generic boundary condition", 2000),),
        "update": update_MP_namelist_cls,
    }
    bpy_default = "MN_SURF"

    @property
    def exported(self):
        if self.element.name in {"INERT", "HVAC", "MIRROR", "OPEN", "PERIODIC"}:
            return False
        return super().exported


@subscribe
class MP_ID(BFParamStr):
    """!
    Blender representation of the ID parameter, the material identification name.
    """

    label = "ID"
    description = "Material identification name"
    fds_label = "ID"
    bpy_type = Material
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"

    def copy_to(self, dest_element):
        pass


@subscribe
class MP_FYI(BFParamFYI):
    """!
    Blender representation of the FYI parameter for materials.
    """

    bpy_type = Material
    bpy_idname = "bf_fyi"


@subscribe
class MP_RGB(BFParam):
    """!
    Blender representation of the RGB parameter.
    Exports both RGB and TRANSPARENCY.
    """

    label = "RGB"
    description = "Red, green, blue components of color"
    fds_label = "RGB"
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def set_value(self, context, value):
        c = self.element.diffuse_color
        c[0], c[1], c[2] = value[0] / 255.0, value[1] / 255.0, value[2] / 255.0

    def to_fds_param(self, context):
        c = self.element.diffuse_color
        rgb = (int(c[0] * 255), int(c[1] * 255), int(c[2] * 255))
        if c[3] == 1.0:  # do not send TRANSPARENCY if it is 1
            return FDSParam(fds_label="RGB", value=rgb,)
        else:
            return (
                FDSParam(fds_label="RGB", value=rgb,),
                FDSParam(fds_label="TRANSPARENCY", value=c[3], precision=2),
            )


@subscribe
class MP_COLOR(BFParam):
    """!
    Blender representation of the COLOR parameter.
    For importing only.
    """

    label = "COLOR"
    description = "Color"
    fds_label = "COLOR"
    bpy_type = Material
    bpy_prop = None  # Do not register

    def set_value(self, context, value):
        c = self.element.diffuse_color
        rgb = config.fds_colors.get(value)
        if not rgb:
            raise BFException(self, f"Unknown color <{value}>")
        c[0], c[1], c[2] = rgb[0] / 255.0, rgb[1] / 255.0, rgb[2] / 255.0

    def to_fds_param(self, context):
        pass


@subscribe
class MP_TRANSPARENCY(BFParam):
    """!
    Blender representation of the TRANSPARENCY parameter.
    For importing only, exported by MP_RGB.
    """

    label = "TRANSPARENCY"
    description = "Red, green, blue components of color and transparency"
    fds_label = "TRANSPARENCY"
    bpy_type = Material
    bpy_prop = None  # Do not register

    def set_value(self, context, value):
        c = self.element.diffuse_color
        c[3] = value

    def to_fds_param(self, context):
        pass


@subscribe
class MP_THICKNESS(BFParam):
    """!
    Blender representation of the THICKNESS parameter, the surface thickness for heat transfer calculation.
    """

    label = "THICKNESS [m]"
    description = "Surface thickness for heat transfer calculation"
    fds_label = "THICKNESS"
    fds_default = 0.0
    bpy_type = Material
    bpy_idname = "bf_thickness"
    bpy_prop = FloatProperty
    bpy_other = {"step": 1.0, "precision": 6, "min": 0.000001}
    bpy_export = "bf_thickness_export"
    bpy_export_default = False

    def from_fds(self, context, value):
        if isinstance(value, float):
            return super().from_fds(context, value)
        else:
            raise BFNotImported(self, "Thickness list not handled")


@subscribe
class MP_HRRPUA(BFParam):
    """!
    Blender representation of the HRRPUA parameter, the heat release rate per unit area.
    """

    label = "HRRPUA [kW/m²]"
    description = "Heat release rate per unit area"
    fds_label = "HRRPUA"
    fds_default = 0.0
    bpy_type = Material
    bpy_idname = "bf_hrrpua"
    bpy_prop = FloatProperty
    bpy_other = {"precision": 3, "min": 0.0}


@subscribe
class MP_TAU_Q(BFParam):
    """!
    Blender representation of the TAU_Q parameter, the ramp time for heat release rate.
    """

    label = "TAU_Q [s]"
    description = "Ramp time for heat release rate"
    fds_label = "TAU_Q"
    fds_default = 1.0
    bpy_type = Material
    bpy_idname = "bf_tau_q"
    bpy_prop = FloatProperty
    bpy_other = {"step": 10.0, "precision": 1}


@subscribe
class MP_MATL_ID(BFParamStr):
    """!
    Blender representation of the MATL_ID parameter, the reference to a MATL (Material) line for self properties.
    """

    label = "MATL_ID"
    description = "Reference to a MATL (Material) line for self properties"
    fds_label = "MATL_ID"
    bpy_type = Material
    bpy_idname = "bf_matl_id"
    bpy_export = "bf_matl_id_export"
    bpy_export_default = False

    def draw_operators(self, context, layout):
        layout.operator("material.bf_choose_matl_id", icon="VIEWZOOM", text="")

    def from_fds(self, context, value):
        if isinstance(value, str):
            return super().from_fds(context, value)
        else:
            raise BFNotImported(self, "Material list not handled")


@subscribe
class MP_IGNITION_TEMPERATURE(BFParam):
    """!
    Blender representation of the IGNITION_TEMPERATURE parameter.
    """

    label = "IGNITION_TEMPERATURE [°C]"
    description = "Ignition temperature"
    fds_label = "IGNITION_TEMPERATURE"
    fds_default = 5000.0
    bpy_type = Material
    bpy_idname = "bf_ignition_temperature"
    bpy_prop = FloatProperty
    bpy_other = {"step": 100.0, "precision": 1, "min": -273.0}
    bpy_export = "bf_ignition_temperature_export"
    bpy_export_default = False


@subscribe
class MP_BACKING(BFParam):
    """!
    Blender representation of the BACKING parameter, the exposition of back side surface.
    """

    label = "BACKING"
    description = "Exposition of back side surface"
    fds_label = "BACKING"
    fds_default = "EXPOSED"
    bpy_type = Material
    bpy_idname = "bf_backing"
    bpy_prop = EnumProperty
    bpy_prop_export = "bf_backing_export"
    bpy_export_default = False
    bpy_other = {
        "items": (
            (
                "VOID",
                "VOID",
                "The wall is assumed to back up to the ambient temperature",
                100,
            ),
            (
                "INSULATED",
                "INSULATED",
                "The back side of the material is perfectly insulated",
                200,
            ),
            (
                "EXPOSED",
                "EXPOSED",
                "The heat transfer into the space behind the wall is calculated (only if wall is one cell thick)",
                300,
            ),
        )
    }


@subscribe
class MP_other(BFParamOther):
    """!
    Blender representation of other parameters for all namelists of Blender Material.
    """

    bpy_type = Material
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class MN_SURF(BFNamelistMa):
    """!
    Blender representation of the SURF namelist, the generic boundary condition.
    """

    label = "SURF"
    description = "Generic boundary condition"
    enum_id = 2000
    fds_label = "SURF"
    bpy_export = "bf_surf_export"
    bpy_export_default = True
    bf_params = (
        MP_ID,
        MP_FYI,
        MP_RGB,
        MP_COLOR,
        MP_TRANSPARENCY,
        MP_MATL_ID,
        MP_THICKNESS,
        MP_BACKING,
        MP_HRRPUA,
        MP_TAU_Q,
        MP_IGNITION_TEMPERATURE,
        MP_other,
    )

    @property
    def exported(self):
        return (
            self.element.bf_surf_export and self.element.name not in config.default_mas
        )


# Object


def update_OP_namelist_cls(ob, context):
    geometry.utils.rm_geometric_cache(ob=ob)
    geometry.utils.rm_tmp_objects()
    ob.bf_namelist.set_appearance(context)


@subscribe
class OP_namelist_cls(BFParam):
    """!
    Blender representation to the identification of FDS namelist.
    """

    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Object
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_default = "ON_OBST"
    bpy_other = {
        "items": (("ON_OBST", "OBST", "Obstruction", 1000),),
        "update": update_OP_namelist_cls,
    }


# OBST


@subscribe
class OP_ID(BFParamStr):
    """!
    Blender representation of the ID parameter, the object identification name.
    """

    label = "ID"
    description = "Object identification name"
    fds_label = "ID"
    bpy_type = Object
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"

    def copy_to(self, dest_element):
        pass


@subscribe
class OP_FYI(BFParamFYI):
    """!
    Blender representation of the FYI parameter for objects.
    """

    bpy_type = Object
    bpy_idname = "bf_fyi"


def update_bf_xb(ob, context):
    # Remove cache and tmp objects
    ob["ob_to_xbs_cache"] = None
    geometry.utils.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES") and ob.bf_xb_export:
        if ob.bf_xyz == "VERTICES":
            ob.bf_xyz_export = False
        if ob.bf_pb == "PLANES":
            ob.bf_pb_export = False
        return


@subscribe
class OP_XB_custom_voxel(BFParam):
    """!
    Blender representation to use custom voxel/pixel size for current Object.
    """

    label = "Use Custom Voxel/Pixel"
    description = "Use custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_custom_voxel"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}

    def draw(self, context, layout):
        return

    def to_fds_param(self, context):
        pass


@subscribe
class OP_XB_voxel_size(BFParam):
    """!
    Blender representation to use custom voxel/pixel size for current Object.
    """

    label = "Custom Voxel/Pixel Size"
    description = "Custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {
        "step": 1.0,
        "precision": 3,
        "min": 0.001,
        "max": 20.0,
        "unit": "LENGTH",
        "update": update_bf_xb,
    }
    bpy_export = "bf_xb_custom_voxel"

    def draw(self, context, layout):
        ob = self.element
        if ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS"):
            super().draw(context, layout)

    def to_fds_param(self, context):
        pass


@subscribe
class OP_XB_center_voxels(BFParam):
    """!
    Blender representation to center voxels/pixels to Object bounding box.
    """

    label = "Center Voxels/Pixels"
    description = "Center voxels/pixels to Object bounding box"
    bpy_type = Object
    bpy_idname = "bf_xb_center_voxels"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}

    def draw(self, context, layout):
        ob = self.element
        if ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS"):
            super().draw(context, layout)

    def to_fds_param(self, context):
        pass


@subscribe
class OP_XB_export(BFParam):
    """!
    Blender representation to set if XB shall be exported to FDS.
    """

    label = "Export XB"
    description = "Set if XB shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xb_export"
    bpy_prop = BoolProperty
    bpy_default = True
    bpy_other = {"update": update_bf_xb}


@subscribe
class OP_XB(BFParamXB):
    """!
    Blender representation to export as volumes/faces.
    """

    label = "XB"
    description = "Export as volumes/faces"
    fds_label = "XB"
    bpy_type = Object
    bpy_idname = "bf_xb"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_xb,
        "items": (
            ("BBOX", "BBox", "Export object bounding box", 100),
            ("VOXELS", "Voxels", "Export voxels from voxelized solid Object", 200),
            ("FACES", "Faces", "Export faces, one for each face", 300),
            ("PIXELS", "Pixels", "Export pixels from pixelized flat Object", 400),
            ("EDGES", "Edges", "Export segments, one for each edge", 500),
        ),
    }
    bpy_export = "bf_xb_export"
    bf_xb_from_fds = None  # auto

    def to_fds_param(self, context):
        ob = self.element
        if not ob.bf_xb_export:
            return
        # Compute
        xbs, msg = geometry.to_fds.ob_to_xbs(context, ob)
        # Single param
        if len(xbs) == 1:
            return FDSParam(fds_label="XB", value=xbs[0], precision=6)
        # Multi param, prepare new ID
        n = ob.name
        suffix = self.element.bf_id_suffix
        if suffix == "IDI":
            ids = (f"{n}_{i}" for i, _ in enumerate(xbs))
        elif suffix == "IDX":
            ids = (f"{n}_x{xb[0]:+.3f}" for xb in xbs)
        elif suffix == "IDY":
            ids = (f"{n}_y{xb[2]:+.3f}" for xb in xbs)
        elif suffix == "IDZ":
            ids = (f"{n}_z{xb[4]:+.3f}" for xb in xbs)
        elif suffix == "IDXY":
            ids = (f"{n}_x{xb[0]:+.3f}_y{xb[2]:+.3f}" for xb in xbs)
        elif suffix == "IDXZ":
            ids = (f"{n}_x{xb[0]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        elif suffix == "IDYZ":
            ids = (f"{n}_y{xb[2]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        elif suffix == "IDXYZ":
            ids = (f"{n}_x{xb[0]:+.3f}_y{xb[2]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        else:
            raise AssertionError(f"Unknown suffix <{suffix}>")
        result = tuple(
            (
                FDSParam(fds_label="ID", value=hid),
                FDSParam(fds_label="XB", value=xb, precision=6),
            )
            for hid, xb in zip(ids, xbs)
        )  # multi
        # Send message
        result[0][0].msg = msg
        return result

    def from_fds(self, context, value):
        bf_xb = geometry.from_fds.xbs_to_ob(
            xbs=(value,),
            context=context,
            ob=self.element,
            bf_xb=self.bf_xb_from_fds,  # auto or forced
        )
        self.element.bf_xb = bf_xb
        self.element.bf_xb_export = True


@subscribe
class OP_XB_BBOX(OP_XB):
    label = "XB as BBox"
    description = "Export as object bounding box"
    bpy_idname = None
    bpy_prop = None

    def to_fds_param(self, context):
        ob = self.element
        ob.bf_xb = "BBOX"
        if not ob.bf_xb_export:
            return
        # Compute
        xbs, _ = geometry.to_fds.ob_to_xbs(context, ob)
        return FDSParam(fds_label="XB", value=xbs[0], precision=6)

    def draw(self, context, layout):
        ob = self.element
        row = layout.row()
        row.active = ob.bf_xb_export
        row.prop(ob, "bf_xb_export", text="XB as BBox")


def update_bf_xyz(ob, context):
    # Remove cache and tmp objects
    ob["ob_to_xyzs_cache"] = None
    geometry.utils.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_xyz == "VERTICES" and ob.bf_xyz_export:
        if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES"):
            ob.bf_xb_export = False
        if ob.bf_pb == "PLANES":
            ob.bf_pb_export = False
        return


@subscribe
class OP_XYZ_export(BFParam):
    """!
    Blender representation to set if XYZ shall be exported to FDS.
    """

    label = "Export XYZ"
    description = "Set if XYZ shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xyz_export"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xyz}


@subscribe
class OP_XYZ(BFParamXYZ):
    """!
    Blender representation of the XYZ parameter to export as points.
    """

    label = "XYZ"
    description = "Export as points"
    fds_label = "XYZ"
    bpy_type = Object
    bpy_idname = "bf_xyz"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_xyz,
        "items": (
            ("CENTER", "Center", "Point, center point of this object", 100),
            ("VERTICES", "Vertices", "Points, one for each vertex of this object", 200),
        ),
    }
    bpy_export = "bf_xyz_export"

    def to_fds_param(self, context):
        ob = self.element
        if not ob.bf_xyz_export:
            return
        # Compute
        xyzs, msg = geometry.to_fds.ob_to_xyzs(context, ob)
        # Single param
        if len(xyzs) == 1:
            return FDSParam(fds_label="XYZ", value=xyzs[0])
        # Multi param, prepare new ID
        n = ob.name
        suffix = self.element.bf_id_suffix
        if suffix == "IDI":
            ids = (f"{n}_{i}" for i, _ in enumerate(xyzs))
        elif suffix == "IDX":
            ids = (f"{n}_x{xyz[0]:+.3f}" for xyz in xyzs)
        elif suffix == "IDY":
            ids = (f"{n}_y{xyz[1]:+.3f}" for xyz in xyzs)
        elif suffix == "IDZ":
            ids = (f"'{n}_z{xyz[2]:+.3f}" for xyz in xyzs)
        elif suffix == "IDXY":
            ids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}" for xyz in xyzs)
        elif suffix == "IDXZ":
            ids = (f"{n}_x{xyz[0]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        elif suffix == "IDYZ":
            ids = (f"{n}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        elif suffix == "IDXYZ":
            ids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        else:
            raise AssertionError(f"Unknown suffix <{suffix}>")
        result = tuple(
            (
                FDSParam(fds_label="ID", value=hid),
                FDSParam(fds_label="XYZ", value=xyz, precision=6),
            )
            for hid, xyz in zip(ids, xyzs)
        )  # multi
        # Send message
        result[0][0].msg = msg
        return result

    def from_fds(self, context, value):
        bf_xyz = geometry.from_fds.xyzs_to_ob(
            xyzs=(value,), context=context, ob=self.element,
        )
        self.element.bf_xyz = bf_xyz
        self.element.bf_xyz_export = True


@subscribe
class OP_XYZ_center(OP_XYZ):
    """!
    Blender representation of the XYZ parameter to export as points (center).
    """

    description = "Export as points (center)"
    bpy_prop = None  # do not redefine
    bf_xyz_idxs = (0,)  # CENTER, VERTICES


def update_bf_pb(ob, context):
    # Remove cache and tmp objects
    ob["ob_to_pbs_cache"] = None
    geometry.utils.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_pb == "PLANES" and ob.bf_pb_export:
        if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES"):
            ob.bf_xb_export = False
        if ob.bf_xyz == "VERTICES":
            ob.bf_xyz_export = False
        return


@subscribe
class OP_PB_export(BFParam):
    """!
    Blender representation to set if PBX, PBY, PBZ shall be exported to FDS.
    """

    label = "Export PBX, PBY, PBZ"
    description = "Set if PBX, PBY, PBZ shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_pb_export"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_pb}


@subscribe
class OP_PB(BFParamPB):
    """!
    Blender representation to export as planes.
    """

    label = "PBX, PBY, PBZ"
    description = "Export as planes"
    bpy_type = Object
    bpy_idname = "bf_pb"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_pb,
        "items": (
            ("PLANES", "Planes", "Planes, one for each face of this object", 100),
        ),
    }
    bpy_export = "bf_pb_export"

    axis = None  # axis for importing

    def to_fds_param(self, context):
        ob = self.element
        if not ob.bf_pb_export:
            return
        # Compute
        # pbs is: (0, 3.5), (0, 4.), (2, .5) ...
        # with 0, 1, 2 perpendicular axis
        pbs, msg = geometry.to_fds.ob_to_pbs(context, ob)
        # Prepare labels
        labels = tuple(
            f"PB{('X','Y','Z')[int(axis)]}" for axis, _ in pbs
        )  # int to protect from float sent by cache
        # Single param
        if len(pbs) == 1:
            return FDSParam(fds_label=labels[0], value=pbs[0][1], precision=6)
        # Multi param, prepare new ID
        n = ob.name
        suffix = self.element.bf_id_suffix
        if suffix == "IDI":
            ids = (f"{n}_{i}" for i, _ in enumerate(pbs))
        elif suffix == "IDXYZ":
            ids = (
                (f"{n}_x{pb:+.3f}", f"{n}_y{pb:+.3f}", f"{n}_z{pb:+.3f}")[
                    int(axis)
                ]  # the pbs cache sends floats
                for axis, pb in pbs
            )
        else:
            raise AssertionError(f"Unknown suffix <{suffix}>")
        result = tuple(
            (
                FDSParam(fds_label="ID", value=hid),
                FDSParam(fds_label=label, value=pb, precision=6),
            )
            for hid, label, (_, pb) in zip(ids, labels, pbs)
        )  # multi
        # Send message
        result[0][0].msg = msg
        return result

    def from_fds(self, context, value):
        bf_pb = geometry.from_fds.pbs_to_ob(
            pbs=((self.axis, value),), context=context, ob=self.element,
        )
        self.element.bf_pb = bf_pb
        self.element.bf_pb_export = True


@subscribe
class OP_PBX(OP_PB):
    """!
    Blender representation of the PBX parameter.
    For importing only.
    """

    fds_label = "PBX"
    bpy_prop = None  # already defined
    axis = 0  # axis for importing

    def draw(self, context, layout):
        return

    def to_fds_param(self, context):
        pass


@subscribe
class OP_PBY(OP_PBX):
    """!
    Blender representation of the PBY parameter.
    For importing only.
    """

    fds_label = "PBY"
    axis = 1  # axis for importing


@subscribe
class OP_PBZ(OP_PBX):
    """!
    Blender representation of the PBZ parameter.
    For importing only.
    """

    fds_label = "PBZ"
    axis = 2  # axis for importing


@subscribe
class OP_ID_suffix(BFParam):
    """!
    Blender representation to append suffix to multiple IDs.
    """

    label = "IDs Suffix"
    description = "Append suffix to multiple IDs"
    bpy_type = Object
    bpy_idname = "bf_id_suffix"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (
            ("IDI", "Index", "Append index number to multiple IDs", 100),
            ("IDX", "x", "Append x coordinate to multiple IDs", 200),
            ("IDY", "y", "Append y coordinate to multiple IDs", 300),
            ("IDZ", "z", "Append z coordinate to multiple IDs", 400),
            ("IDXY", "xy", "Append x,y coordinates to multiple IDs", 500),
            ("IDXZ", "xz", "Append x,z coordinates to multiple IDs", 600),
            ("IDYZ", "yz", "Append y,z coordinates to multiple IDs", 700),
            ("IDXYZ", "xyz", "Append x,y,z coordinates to multiple IDs", 800),
        )
    }

    def draw(self, context, layout):
        ob = self.element
        if (
            (ob.bf_xb_export and ob.bf_xb != "BBOX")
            or (ob.bf_xyz_export and ob.bf_xyz == "VERTICES")
            or ob.bf_pb_export
        ):
            layout.prop(ob, "bf_id_suffix")
        return layout


@subscribe
class OP_SURF_ID(BFParam):
    """!
    Blender representation of the SURF_ID parameter, the reference to a SURF boundary condition.
    """

    label = "SURF_ID"
    description = "Reference to SURF"
    fds_label = "SURF_ID"
    bpy_type = Object
    bpy_idname = "active_material"
    bpy_export = "bf_surf_id_export"
    bpy_export_default = True

    @property
    def value(self):
        if self.element.active_material:
            return self.element.active_material.name

    def set_value(self, context, value):
        if value is None:
            self.element.active_material = None
        else:
            try:  # FIXME use utils
                ma = bpy.data.materials.get(value)
            except IndexError:
                raise BFException(self, f"Blender Material <{value}> does not exists")
            else:
                self.element.active_material = ma

    @property
    def exported(self):
        ob = self.element
        return ob.bf_surf_id_export and ob.active_material

    def check(self, context):
        ob = self.element
        if (
            ob.bf_surf_id_export
            and ob.active_material
            and not ob.active_material.bf_surf_export
        ):
            raise BFException(
                self, f"Referred SURF <{ob.active_material.name}> is not exported"
            )


@subscribe
class OP_other(BFParamOther):
    """!
    Blender representation of other parameters for all namelists of Blender Object.
    """

    bpy_type = Object
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


@subscribe
class ON_OBST(BFNamelistOb):
    """!
    Blender representation of the OBST namelist.
    """

    label = "OBST"
    description = "Obstruction"
    enum_id = 1000
    fds_label = "OBST"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_XB,
        OP_XB_custom_voxel,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}


# Other


@subscribe
class OP_other_namelist(BFParam):
    """!
    Blender representation of the other namelist label, eg <ABCD>.
    """

    label = "Label"
    description = "Other namelist label, eg <ABCD>"
    bpy_type = Object
    bpy_prop = StringProperty
    bpy_idname = "bf_other_namelist"
    bpy_default = "ABCD"
    bpy_other = {"maxlen": 4}

    def check(self, context):
        if not re.match("^[A-Z0-9_]{4}$", self.element.bf_other_namelist):
            raise BFException(
                self,
                f"Malformed other namelist label <{self.element.bf_other_namelist}>",
            )


@subscribe
class ON_other(BFNamelistOb):
    """!
    Blender representation of any other namelists.
    """

    label = "Other"
    description = "Other namelist"
    enum_id = 1007
    bf_params = (
        OP_other_namelist,
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_XB,
        OP_XB_custom_voxel,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_XYZ,
        OP_PB,
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}

    @property
    def fds_label(self):
        return self.element.bf_other_namelist


# GEOM


@subscribe
class OP_GEOM_check_sanity(BFParam):
    """!
    Blender representation to check if closed orientable manifold, with no degenerate geometry while exporting.
    """

    label = "Check Sanity While Exporting"
    description = "Check if closed orientable manifold, with no degenerate geometry while exporting"
    bpy_type = Object
    bpy_idname = "bf_geom_check_sanity"
    bpy_prop = BoolProperty
    bpy_default = True

    def draw(self, context, layout):
        ob = self.element
        layout.prop(ob, "bf_geom_check_sanity")


@subscribe
class OP_GEOM_protect(BFParam):  # TODO put in operator? several ops depend on it
    """!
    Blender representation to protect original Object geometry while checking its sanity.
    """

    label = "Protect Original"
    description = "Protect original Object geometry while checking its sanity"
    bpy_type = Object
    bpy_idname = "bf_geom_protect"
    bpy_prop = BoolProperty
    bpy_default = True


@subscribe
class OP_MOVE_ID(BFParamStr):
    """!
    Blender representation for exporting a namelist with its MOVE namelist.
    """

    label = "MOVE_ID"
    description = "Export this namelist with a corresponding MOVE namelist"
    fds_label = "MOVE_ID"
    bpy_type = Object
    bpy_idname = "bf_move_id"
    bpy_export = "bf_move_id_export"
    bpy_export_default = False  # Export only when requested

    @property
    def exported(self):  # changed because inherits empty string is not exported
        return self.element.bf_move_id_export

    @property
    def value(self):  # never empty
        return self.element.bf_move_id or f"{self.element.name}_move"


@subscribe
class OP_GEOM_SURF_ID(BFParam):
    """!
    Blender representation for SURF_ID parameter.
    """

    label = "SURF_ID"
    fds_label = "SURF_ID"
    bpy_type = Object

    @property
    def value(self):
        ob = self.element
        value = list()
        for ms in ob.material_slots:
            ma = ms.material
            if not ma:
                raise BFException(self, f"Object <{ob.name}> has empty material slot")
            if not ma.bf_surf_export:
                raise BFException(ob, f"Referenced SURF <{ma.name}> is not exported")
            value.append(ma.name)
        return tuple(value)

    def set_value(self, context, value):
        # A single material is a string
        if isinstance(value, str):
            value = (value,)
        # Delete and refill material_slots
        ob = self.element
        while ob.material_slots:
            ob.active_material_index = 0  # select the top material
            bpy.ops.object.material_slot_remove()  # delete it
        for name in value:
            ma = bpy.data.materials.get(name)
            if not ma:
                raise BFException(self, f"Unknown SURF_ID <{name}>")
            ob.data.materials.append(ma)

    def draw(self, context, layout):
        pass


@subscribe
class OP_GEOM_SURF_IDS(OP_GEOM_SURF_ID):
    """!
    Blender representation for SURF_IDS parameter.
    """

    label = "SURF_IDS"
    fds_label = "SURF_IDS"
    bpy_type = Object

    def to_fds_param(self, context):
        pass


@subscribe
class OP_GEOM_SURF_ID6(OP_GEOM_SURF_ID):
    """!
    Blender representation for SURF_IDS parameter.
    """

    label = "SURF_ID6"
    fds_label = "SURF_ID6"
    bpy_type = Object

    def to_fds_param(self, context):
        pass


@subscribe
class OP_GEOM_XB(BFParam):
    """!
    Blender representation for SURF_IDS parameter.
    """

    label = "XB"
    fds_label = "XB"
    bpy_type = Object

    def set_value(self, context, value):
        xbs = (value,)
        if len(xbs[0]) != 6:
            raise BFNotImported(self, f"Unsupported XB value <{value}>")
        ob = self.element
        me = ob.data
        geometry.from_fds.xbs_bbox_to_mesh(
            xbs=xbs, context=context, me=me, surf_id=True,
        )

    def to_fds_param(self, context):
        pass


@subscribe
class OP_GEOM_binary_directory(BFParam):
    """!
    Blender representation for BINARY_FILE parameter directory.
    """

    # Used in conjuction with OP_GEOM_BINARY_FILE
    # Contains abs path or path relative to saved blend file
    label = "Binary Directory"
    description = "Destination directory for the binary bingeom file"
    bpy_type = Object
    bpy_idname = "bf_geom_binary_directory"
    bpy_prop = StringProperty
    bpy_other = {"subtype": "DIR_PATH", "maxlen": 1024}

    # Taken care at OP_GEOM_BINARY_FILE
    def draw(self, context, layout):
        pass


@subscribe
class OP_GEOM_BINARY_FILE(BFParam):
    """!
    Blender representation for BINARY_FILE parameter.
    """

    label = "BINARY_FILE"
    description = "Name of the binary bingeom file"
    fds_label = "BINARY_FILE"
    bpy_type = Object
    bpy_idname = "data"
    bpy_export = "bf_geom_binary_file_export"
    bpy_export_default = False

    def _get_bingeom_filepath(self, sc, ob):
        return io.calc_path(
            bl_path=ob.bf_geom_binary_directory or sc.bf_config_directory or "//",
            name=ob.data.name,
            extension=".bingeom",
        )

    def _get_fds_binary_file(self, sc, ob):
        return io.calc_path(
            bl_path=ob.bf_geom_binary_directory or sc.bf_config_directory or "//",
            name=ob.data.name,
            extension=".bingeom",
            bl_start=sc.bf_config_directory,
        )

    def _get_blend_name_and_directory(self, fds_binary_file, sc):
        fds_path = io.calc_path(bl_path=sc.bf_config_directory or "//",)
        return io.calc_bl_name_and_dir(filepath=fds_binary_file, start=fds_path,)

    def check(self, context):
        filepath = self._get_bingeom_filepath(sc=context.scene, ob=self.element)
        path = os.path.dirname(filepath)
        if self.element.bf_geom_binary_directory and not os.path.exists(path):
            raise BFException(self, f"Bingeom directory <{path}> not existing")

    def draw(self, context, layout):
        ob, mesh, space = context.object, context.mesh, context.space_data
        active = self.element.bf_geom_binary_file_export
        col = layout.column(align=True, heading=" ")
        col.active = active
        col.prop(ob, "bf_geom_binary_file_export", text="BINARY_FILE")
        if active:
            if ob:  # mesh name, from properties_data_mesh.py
                col.template_ID(ob, "data", text="Name")
            elif mesh:
                col.template_ID(space, "pin_id", text="Name")
            row = col.row()
            row_active = bool(self.element.bf_geom_binary_directory)
            row.active = row_active
            if row_active:
                try:
                    self.check(context)
                except BFException:
                    row.alert = True
            row.prop(ob, "bf_geom_binary_directory", text="Directory")

    def to_fds_param(self, context):
        if not self.exported:
            return
        self.check(context)
        # Calc geometry
        vs, fs, ss, _, msg = geometry.to_fds.ob_to_geom(
            context=context,
            ob=self.element,
            check=self.element.bf_geom_check_sanity,
            check_open=not self.element.bf_geom_is_terrain,
            world=not self.element.bf_move_id_export,
        )
        # Save .bingeom file
        filepath = self._get_bingeom_filepath(sc=context.scene, ob=self.element)
        io.write_bingeom_file(
            geom_type=self.element.bf_geom_is_terrain and 2 or 1,
            n_surf_id=len(self.element.data.materials),
            fds_verts=vs,
            fds_faces=fs,
            fds_surfs=ss,
            fds_volus=list(),
            filepath=filepath,
        )
        # Prepare fds_param
        value = self._get_fds_binary_file(sc=context.scene, ob=self.element)
        return FDSParam(fds_label="BINARY_FILE", value=value, msg=msg)

    def from_fds(self, context, value):
        if not value:
            return
        # Calc and assign value
        name, path = self._get_blend_name_and_directory(
            fds_binary_file=value, sc=context.scene
        )
        # Blender bug, assign twice to have it right  # TODO
        self.element.data.name = name
        self.element.data.name = name
        if self.element.data.name != name:
            raise Exception(f"Blender bug")
        self.element.bf_geom_binary_directory = path
        self.set_exported(context, value=True)
        # Load .bingeom file and import it
        filepath = self._get_bingeom_filepath(sc=context.scene, ob=self.element)
        me = self.element.data
        _, vs, fs, ss, _ = io.read_bingeom_file(filepath)
        geometry.from_fds.geom_to_mesh(
            fds_verts=vs, fds_faces=fs, fds_surfs=ss, context=context, me=me,
        )


@subscribe
class OP_GEOM_IS_TERRAIN(BFParam):
    """!
    Blender representation of the IS_TERRAIN parameter to set if it represents a terrain.
    """

    label = "IS_TERRAIN"
    description = "Set if it represents a terrain"
    fds_label = "IS_TERRAIN"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_is_terrain"


@subscribe
class OP_GEOM_EXTEND_TERRAIN(BFParam):
    """!
    Blender representation of the EXTEND_TERRAIN parameter to set if this terrain needs extension to fully cover the domain.
    """

    label = "EXTEND_TERRAIN"
    description = "Set if this terrain needs extension to fully cover the domain"
    fds_label = "EXTEND_TERRAIN"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_geom_extend_terrain"

    @property
    def exported(self):
        ob = self.element
        return ob.bf_geom_is_terrain


@subscribe
class ON_GEOM(BFNamelistOb):
    """!
    Blender representation of the GEOM namelist.
    """

    label = "GEOM"
    description = "Geometry"
    enum_id = 1021
    fds_label = "GEOM"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_GEOM_check_sanity,
        OP_GEOM_SURF_ID,
        OP_GEOM_SURF_IDS,
        OP_GEOM_SURF_ID6,
        OP_MOVE_ID,
        OP_GEOM_XB,
        OP_GEOM_binary_directory,
        OP_GEOM_BINARY_FILE,
        OP_GEOM_IS_TERRAIN,
        OP_GEOM_EXTEND_TERRAIN,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}

    def to_fds_namelist(self, context):
        # First populate the Object
        fds_namelist = super().to_fds_namelist(context)
        # Then treat VERTS and FACES
        if not self.element.bf_geom_binary_file_export:  # no bingeom?
            # Prepare geometry
            vs, _, _, fss, msg = geometry.to_fds.ob_to_geom(
                context=context,
                ob=self.element,
                check=self.element.bf_geom_check_sanity,
                check_open=not self.element.bf_geom_is_terrain,
                world=not self.element.bf_move_id_export,
            )
            # Send VERTS and FACES
            fds_namelist.fds_params.extend(
                (
                    FDSParam(fds_label="VERTS", value=vs, precision=6),
                    FDSParam(fds_label="FACES", value=fss),
                )
            )
            fds_namelist.msg = msg
        return fds_namelist

    def from_fds(self, context, fds_namelist):
        if (
            fds_namelist.get_by_label("CYLINDER_ORIGIN")  # FIXME remove after CYL
            or fds_namelist.get_by_label("ZVALS")
            or fds_namelist.get_by_label("POLY")
        ):
            raise BFNotImported(self, "Not implemented")
        # Get VERTS and FACES and remove them from auto treatment
        p_verts = fds_namelist.get_by_label("VERTS", remove=True)
        p_faces_surfs = fds_namelist.get_by_label("FACES", remove=True)
        # Get SPHERE  # FIXME check default and activation in FDS doc
        p_n_levels = fds_namelist.get_by_label("N_LEVELS", remove=True)
        p_sphere_origin = fds_namelist.get_by_label("SPHERE_ORIGIN", remove=True)
        p_sphere_radius = fds_namelist.get_by_label("SPHERE_RADIUS", remove=True)
        # Get CYLINDER  # FIXME check default and activation in FDS doc
        p_cyl_length = fds_namelist.get_by_label("CYLINDER_LENGTH", remove=True)
        p_cyl_radius = fds_namelist.get_by_label("CYLINDER_RADIUS", remove=True)
        p_cyl_origin = fds_namelist.get_by_label("CYLINDER_ORIGIN", remove=True)
        p_cyl_axis = fds_namelist.get_by_label("CYLINDER_AXIS", remove=True)
        p_cyl_nseg_axis = fds_namelist.get_by_label("CYLINDER_NSEG_AXIS", remove=True)
        p_cyl_nseg_theta = fds_namelist.get_by_label("CYLINDER_NSEG_THETA", remove=True)
        # Populate the Object
        super().from_fds(context, fds_namelist)
        if len(self.element.data.vertices):  # FIXME better way?
            # Mesh already present, no special treatment for bingeom
            return
        elif p_verts and p_faces_surfs:
            # Treat VERTS and FACES
            geometry.from_fds.geom_to_mesh(
                fds_verts=p_verts.value,
                fds_faces_surfs=p_faces_surfs.value,
                context=context,
                me=self.element.data,
            )
        elif p_sphere_origin:
            # Treat SPHERE # FIXME move to sphere_to_mesh
            bpy.ops.mesh.primitive_ico_sphere_add(
                subdivisions=p_n_levels and p_n_levels.value or 2,
                radius=p_sphere_radius and p_sphere_radius.value or 0.5,
                location=p_sphere_origin.value,
            )
            sphere_ob = context.object
            for ma in self.element.data.materials:
                sphere_ob.data.materials.append(ma)
            self.element.data = sphere_ob.data
            bpy.data.objects.remove(sphere_ob, do_unlink=True)
        # elif p_cyl_origin:
        #     # Treat CYLINDER # FIXME move to cyl_to_mesh, and defaults
        #     ax, ay, az = p_cyl_axis and p_cyl_axis.value or (1, 0, 0)
        #     rotation = (  # FIXME not working!
        #         math.degrees(
        #             (ay or az) and math.acos(ay / (ay ** 2 + az ** 2) ** 0.5) or 0
        #         ),  # alpha x
        #         math.degrees(
        #             (az or ax) and math.acos(az / (az ** 2 + ax ** 2) ** 0.5) or 0
        #         ),  # alpha x
        #         0.0,  # alpha z
        #     )
        #     print("rotation", rotation)
        #     bpy.ops.mesh.primitive_cylinder_add(
        #         vertices=p_cyl_nseg_theta and p_cyl_nseg_theta.value or 32,
        #         radius=p_cyl_radius and p_cyl_radius.value or 1.0,
        #         depth=p_cyl_length and p_cyl_length.value or 2.0,
        #         location=p_cyl_origin.value,
        #         rotation=rotation,
        #         # p_cyl_nseg_axis = fds_namelist.get_by_label("CYLINDER_NSEG_AXIS", remove=True)
        #         # p_cyl_nseg_theta = fds_namelist.get_by_label("CYLINDER_NSEG_THETA", remove=True)
        #     )
        #     cyl_ob = context.object
        #     for ma in self.element.data.materials:
        #         cyl_ob.data.materials.append(ma)
        #     self.element.data = cyl_ob.data
        #     bpy.data.objects.remove(cyl_ob, do_unlink=True)
        else:
            raise BFException(self, f"Unknown GEOM type <{fds_namelist}>")

    def draw_operators(self, context, layout):
        ob = context.object
        col = layout.column()
        col.prop(ob, "bf_geom_protect")
        col.operator("object.bf_geom_check_sanity")
        col.operator("object.bf_geom_check_intersections")


# HOLE


@subscribe
class ON_HOLE(BFNamelistOb):
    """!
    Blender representation of the HOLE namelist.
    """

    label = "HOLE"
    description = "Obstruction cutout"
    enum_id = 1009
    fds_label = "HOLE"
    bf_params = OP_ID, OP_FYI, OP_XB, OP_other
    bf_other = {"appearance": "DUMMY0"}


# VENT


@subscribe
class OP_VENT_OBST_ID(BFParam):
    """!
    Blender representation of the OBST_ID parameter to specify OBST on which projecting the condition.
    """

    label = "OBST_ID"
    description = "Specify OBST on which projecting the condition"
    fds_label = "OBST_ID"
    bpy_type = Object
    bpy_prop = PointerProperty
    bpy_idname = "bf_vent_obst_id"
    bpy_other = {"type": Object}

    @property
    def value(self):
        if self.element.bf_vent_obst_id:
            return self.element.bf_vent_obst_id.name

    def set_value(self, context, value):
        if value:
            ob = bpy.data.objects.get(value)
            if ob:
                self.element.bf_vent_obst_id = ob
            else:
                raise BFException(self, f"Object <{value}> not found")
        else:
            self.element.bf_vent_obst_id = None


@subscribe
class ON_VENT(BFNamelistOb):
    """!
    Blender representation of the VENT namelist.
    """

    label = "VENT"
    description = "Boundary condition patch"
    enum_id = 1010
    fds_label = "VENT"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_VENT_OBST_ID,
        OP_XB,
        OP_XYZ,
        OP_PB,
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}


# DEVC


@subscribe
class OP_DEVC_QUANTITY(BFParamStr):
    """!
    Blender representation of the QUANTITY parameter, the output quantity.
    """

    label = "QUANTITY"
    description = "Output quantity"
    fds_label = "QUANTITY"
    bpy_type = Object
    bpy_idname = "bf_quantity"

    def draw_operators(self, context, layout):
        layout.operator("object.bf_choose_devc_quantity", icon="VIEWZOOM", text="")


@subscribe
class OP_DEVC_SETPOINT(BFParam):
    """!
    Blender representation of the SETPOINT parameter, the value of the device at which its state changes.
    """

    label = "SETPOINT [~]"
    description = "Value of the device at which its state changes"
    fds_label = "SETPOINT"
    bpy_type = Object
    bpy_idname = "bf_devc_setpoint"
    bpy_prop = FloatProperty
    bpy_default = 0.0
    bpy_other = {"step": 10.0, "precision": 3}
    bpy_export = "bf_devc_setpoint_export"
    bpy_export_default = False


@subscribe
class OP_DEVC_INITIAL_STATE(BFParam):
    """!
    Blender representation of the INITIAL_STATE parameter to set device initial state.
    """

    label = "INITIAL_STATE"
    description = "Set device initial state"
    fds_label = "INITIAL_STATE"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_devc_initial_state"


@subscribe
class OP_DEVC_LATCH(BFParam):
    """!
    Blender representation of the LATCH parameter, the device only changes state once.
    """

    label = "LATCH"
    description = "Device only changes state once"
    fds_label = "LATCH"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_devc_latch"


@subscribe
class OP_DEVC_PROP_ID(BFParamStr):
    """!
    Blender representation of the PROP_ID parameter, the reference to a PROP (Property) line for self properties.
    """

    label = "PROP_ID"
    description = "Reference to a PROP (Property) line for self properties"
    fds_label = "PROP_ID"
    bpy_type = Object
    bpy_idname = "bf_devc_prop_id"

    def draw_operators(self, context, layout):
        layout.operator("object.bf_choose_devc_prop_id", icon="VIEWZOOM", text="")


@subscribe
class ON_DEVC(BFNamelistOb):
    """!
    Blender representation of the DEVC namelist.
    """

    label = "DEVC"
    description = "Device"
    enum_id = 1011
    fds_label = "DEVC"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_DEVC_SETPOINT,
        OP_DEVC_INITIAL_STATE,
        OP_DEVC_LATCH,
        OP_DEVC_PROP_ID,
        OP_XB,
        OP_XYZ,
        OP_PB,  # TODO used?
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "DUMMY1"}


# SLCF


@subscribe
class OP_SLCF_VECTOR(BFParam):
    """!
    Blender representation of the VECTOR parameter to create animated vectors.
    """

    label = "VECTOR"
    description = "Create animated vectors"
    fds_label = "VECTOR"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_slcf_vector"


@subscribe
class OP_SLCF_CELL_CENTERED(BFParam):
    """!
    Blender representation of the CELL_CENTERED parameter to output the actual cell-centered data with no averaging.
    """

    label = "CELL_CENTERED"
    description = "Output the actual cell-centered data with no averaging"
    fds_label = "CELL_CENTERED"
    fds_default = False
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_slcf_cell_centered"


@subscribe
class ON_SLCF(BFNamelistOb):
    """!
    Blender representation of the SLCF namelist.
    """

    label = "SLCF"
    description = "Slice file"
    enum_id = 1012
    fds_label = "SLCF"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_DEVC_QUANTITY,
        OP_SLCF_VECTOR,
        OP_SLCF_CELL_CENTERED,
        OP_XB,
        OP_PB,
        OP_PBX,
        OP_PBY,
        OP_PBZ,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "DUMMY1"}


# PROF


@subscribe
class ON_PROF(BFNamelistOb):
    """!
    Blender representation of the PROF namelist.
    """

    label = "PROF"
    description = "Wall profile output"
    enum_id = 1013
    fds_label = "PROF"
    bf_params = OP_ID, OP_FYI, OP_DEVC_QUANTITY, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "DUMMY1"}


# MESH


@subscribe
class OP_MESH_IJK(BFParam):
    """!
    Blender representation of the IJK parameter, the cell number in x, y, and z direction.
    """

    label = "IJK"
    description = "Cell number in x, y, and z direction"
    fds_label = "IJK"
    bpy_type = Object
    bpy_idname = "bf_mesh_ijk"
    bpy_prop = IntVectorProperty
    bpy_default = (10, 10, 10)
    bpy_other = {"size": 3, "min": 1}
    bpy_export = "bf_mesh_ijk_export"
    bpy_export_default = True

    def draw(self, context, layout):
        ob = context.object
        col = layout.column()
        xbs = geometry.utils.get_bbox_xbs(context=context, ob=ob, world=True)
        (
            has_good_ijk,
            cs,
            cell_count,
            cell_aspect_ratio,
        ) = fds.mesh_tools.calc_cell_infos(ijk=ob.bf_mesh_ijk, xbs=xbs)
        col.label(text=f"Cell Size: {cs[0]:.3f}m x {cs[1]:.3f}m x {cs[2]:.3f}m")
        col.label(
            text=f"Cell Qty: {cell_count} | Aspect: {cell_aspect_ratio:.1f} | Poisson: {has_good_ijk and 'Yes' or 'No'}"
        )
        super().draw(context, layout)

    def to_fds_param(self, context):
        ob = self.element
        if not ob.bf_mesh_ijk_export:
            return
        xbs = geometry.utils.get_bbox_xbs(context=context, ob=ob, world=True)
        (
            has_good_ijk,
            cs,
            cell_count,
            cell_aspect_ratio,
        ) = fds.mesh_tools.calc_cell_infos(ijk=ob.bf_mesh_ijk, xbs=xbs)
        msg = f"MESH Cell Size: {cs[0]:.3f} m, {cs[1]:.3f} m, {cs[2]:.3f} m | Qty: {cell_count} | Aspect: {cell_aspect_ratio:.1f} | Poisson: {has_good_ijk and 'Yes' or 'No'}"
        return FDSParam(fds_label="IJK", value=ob.bf_mesh_ijk, msg=msg)


@subscribe
class OP_MESH_MPI_PROCESS(BFParam):
    """!
    Blender representation of the MPI_PROCESS parameter, the assigned to given MPI process (Starting from 0.).
    """

    label = "MPI_PROCESS"
    description = "Assigned to given MPI process (Starting from 0.)"
    fds_label = "MPI_PROCESS"
    fds_default = 0
    bpy_type = Object
    bpy_idname = "bf_mesh_mpi_process"
    bpy_prop = IntProperty
    bpy_other = {"min": 0}
    bpy_export = "bf_mesh_mpi_process_export"
    bpy_export_default = False


@subscribe
class ON_MESH(BFNamelistOb):
    """!
    Blender representation of the MESH namelist.
    """

    label = "MESH"
    description = "Domain of simulation"
    enum_id = 1014
    fds_label = "MESH"
    bf_params = OP_ID, OP_FYI, OP_MESH_IJK, OP_MESH_MPI_PROCESS, OP_XB_BBOX, OP_other
    bf_other = {"appearance": "WIRE"}

    def draw_operators(self, context, layout):
        col = layout.column()
        col.operator("object.bf_set_mesh_cell_size")
        col.operator("object.bf_align_selected_meshes")


# INIT


@subscribe
class ON_INIT(BFNamelistOb):
    """!
    Blender representation of the INIT namelist.
    """

    label = "INIT"
    description = "Initial condition"
    enum_id = 1015
    fds_label = "INIT"
    bf_params = OP_ID, OP_FYI, OP_XB, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "DUMMY2"}


# ZONE


@subscribe
class ON_ZONE(BFNamelistOb):
    """!
    Blender representation of the ZONE namelist.
    """

    label = "ZONE"
    description = "Pressure zone"
    enum_id = 1016
    fds_label = "ZONE"
    bf_params = OP_ID, OP_FYI, OP_XB, OP_XYZ, OP_other
    bf_other = {"appearance": "DUMMY2"}


# HVAC


@subscribe
class ON_HVAC(BFNamelistOb):
    """!
    Blender representation of the HVAC namelist.
    """

    label = "HVAC"
    description = "HVAC system definition"
    enum_id = 1017
    fds_label = "HVAC"
    bf_params = OP_ID, OP_FYI, OP_XYZ, OP_ID_suffix, OP_other
    bf_other = {"appearance": "WIRE"}


# Closing

# Update OP_namelist items

items = [
    (cls.__name__, cls.label, cls.description, cls.enum_id)
    for _, cls in bf_namelists_by_cls.items()
    if cls.bpy_type == Object
]
items.sort(key=lambda k: k[1])
OP_namelist_cls.bpy_other["items"] = items

# Update MP_namelist items

items = [
    (cls.__name__, cls.label, cls.description, cls.enum_id)
    for _, cls in bf_namelists_by_cls.items()
    if cls.bpy_type == Material
]
items.sort(key=lambda k: k[1])
MP_namelist_cls.bpy_other["items"] = items


# Register


def register():
    """!
    Register Blender classes.
    """
    from bpy.utils import register_class

    # Blender classes
    for cls in bl_classes:
        log.debug(f"Registering Blender class <{cls.__name__}>")
        register_class(cls)
    # System parameters for tmp obs and file version
    # log.debug(f"Registering sys properties: bf_is_tmp, bf_has_tmp, ...")
    Object.bf_is_tmp = BoolProperty(
        name="Is Tmp", description="Set if this Object is tmp", default=False
    )
    Object.bf_has_tmp = BoolProperty(
        name="Has Tmp",
        description="Set if this Object has tmp companions",
        default=False,
    )
    Scene.bf_file_version = IntVectorProperty(name="BlenderFDS File Version", size=3)
    # params and namelists
    for cls in bf_params:
        cls.register()
    for cls in bf_namelists:
        cls.register()


def unregister():
    """!
    Unregister Blender classes.
    """
    from bpy.utils import unregister_class

    # params and namelists
    for cls in bf_namelists:
        cls.unregister()
    for cls in bf_params:
        cls.unregister()
    # System parameters for tmp obs and file version
    del Object.bf_is_tmp
    del Object.bf_has_tmp
    del Scene.bf_file_version
    # Blender classes
    for cls in bl_classes:
        log.debug(f"Unregistering Blender class <{cls.__name__}>")
        unregister_class(cls)
