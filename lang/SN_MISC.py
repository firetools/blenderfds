# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Scene
from bpy.props import BoolProperty, FloatProperty, StringProperty, IntProperty
from ..config import BEARING_P, LATLON_P
from ..types import BFParam, BFParamOther, BFParamFYI, BFNamelistSc, FDSParam, FDSList
from ..bl.ui_lists import (
    WM_PG_bf_other,
    WM_UL_bf_other_items,
)
from .. import utils

log = logging.getLogger(__name__)


class SP_MISC_FYI(BFParamFYI):
    bpy_type = Scene
    bpy_idname = "bf_misc_fyi"


class SP_MISC_OVERWRITE(BFParam):
    label = "OVERWRITE"
    description = "Do not check for the existence of CHID.out and overwrite files"
    fds_label = "OVERWRITE"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_misc_overwrite"


class SP_MISC_THICKEN_OBSTRUCTIONS(BFParam):
    label = "THICKEN_OBSTRUCTIONS"
    description = "Do not allow thin sheet obstructions"
    fds_label = "THICKEN_OBSTRUCTIONS"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_misc_thicken_obstructions"


def update_lonlat(self, context):
    sc = context.scene
    utm = utils.gis.LonLat(sc.bf_origin_lon, sc.bf_origin_lat).to_UTM()
    sc["bf_origin_utm_zn"] = utm.zn  # avoid triggering another update
    sc["bf_origin_utm_ne"] = utm.ne
    sc["bf_origin_utm_easting"] = utm.easting
    sc["bf_origin_utm_northing"] = utm.northing


class SP_origin_geoname_export(BFParam):
    label = "Export origin geoposition"
    description = "Set if origin geoposition shall be exported to FDS"
    bpy_type = Scene
    bpy_idname = "bf_origin_export"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_lonlat}


class SP_origin_geoname(BFParam):
    label = "Origin Geoname"
    description = "Origin location geographic name"
    bpy_type = Scene
    bpy_idname = "bf_origin_geoname"
    bpy_prop = StringProperty
    bpy_export = "bf_origin_export"

    def draw(self, context, layout):
        sc = self.element
        exported = sc.bf_origin_export  # TODO cleaner
        url = utils.gis.LonLat(lon=sc.bf_origin_lon, lat=sc.bf_origin_lat).to_url()
        col = layout.column(align=True, heading="Origin Geolocation")
        row = col.row(align=True)
        sub = row.row(align=True)
        sub.prop(sc, "bf_origin_export", text="")
        col2 = sub.column(align=True)
        col2.active = exported
        sub = col2.row(align=True)
        sub.prop(sc, "bf_origin_geoname", text="")
        sub.operator("wm.url_open", text="", icon="URL").url = url
        col2.prop(sc, "bf_origin_lon")
        col2.prop(sc, "bf_origin_lat")
        col2.prop(sc, "bf_origin_north_bearing")
        row = col.row()
        row.active = exported
        row.alignment = "RIGHT"
        text = (
            f"UTM {sc.bf_origin_utm_zn}{sc.bf_origin_utm_ne and 'N' or 'S'} "
            f"{int(sc.bf_origin_utm_easting)}m E {int(sc.bf_origin_utm_northing)}m N (WGS84)"
        )
        row.label(text=text)


class SP_ORIGIN_LON(BFParam):
    label = "ORIGIN_LON"
    description = "Longitude (WGS84, EPSG:4326) of world origin in decimal degrees"
    fds_label = "ORIGIN_LON"
    bpy_type = Scene
    bpy_idname = "bf_origin_lon"
    bpy_prop = FloatProperty
    bpy_export = "bf_origin_export"
    bpy_default = 9.16889  # Portofino mountain
    bpy_other = {
        "min": -180.0,
        "max": 180.0,
        "precision": LATLON_P,
        "update": update_lonlat,
    }

    def draw(self, context, layout):
        pass


class SP_ORIGIN_LAT(BFParam):
    label = "ORIGIN_LAT"
    description = "Latitude (WGS84, EPSG:4326) of world origin in decimal degrees"
    fds_label = "ORIGIN_LAT"
    bpy_type = Scene
    bpy_idname = "bf_origin_lat"
    bpy_prop = FloatProperty
    bpy_export = "bf_origin_export"
    bpy_default = 44.32676  # Portofino mountain
    bpy_other = {
        "min": -80.0,
        "max": 84.0,
        "precision": LATLON_P,
        "update": update_lonlat,
    }

    def draw(self, context, layout):
        pass


class SP_origin_utm_zn(BFParam):
    label = "Origin UTM Zone Number"
    description = "UTM Zone Number (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_zn"
    bpy_prop = IntProperty
    bpy_default = 32
    bpy_other = {"min": 1, "max": 60}

    def draw(self, context, layout):
        pass


class SP_origin_utm_ne(BFParam):
    label = "Origin UTM Northern Emisphere"
    description = "UTM northern emisphere (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_ne"
    bpy_prop = BoolProperty
    bpy_default = True

    def draw(self, context, layout):
        pass


class SP_origin_utm_easting(BFParam):
    label = "Origin UTM Easting"
    description = "UTM easting (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_easting"
    bpy_prop = FloatProperty
    bpy_default = 500000.0
    bpy_other = {"unit": "LENGTH", "min": 0, "max": 1000000}

    def draw(self, context, layout):
        pass


class SP_origin_utm_northing(BFParam):
    label = "Origin UTM Northing"
    description = "UTM northing (WGS84) of world origin"
    bpy_type = Scene
    bpy_idname = "bf_origin_utm_northing"
    bpy_prop = FloatProperty
    bpy_default = 5000000.0
    bpy_other = {"unit": "LENGTH", "min": 0, "max": 10000000}

    def draw(self, context, layout):
        pass


class SP_ORIGIN_NORTH_BEARING(BFParam):
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
        "precision": BEARING_P,
    }

    def draw(self, context, layout):
        pass


class SP_MISC_other(BFParamOther):
    bpy_type = Scene
    bpy_idname = "bf_misc_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class SN_MISC(BFNamelistSc):
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
        SP_ORIGIN_LON,
        SP_ORIGIN_LAT,
        SP_ORIGIN_NORTH_BEARING,
        SP_origin_utm_zn,
        SP_origin_utm_ne,
        SP_origin_utm_easting,
        SP_origin_utm_northing,
        SP_MISC_other,
    )
