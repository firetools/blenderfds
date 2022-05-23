# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators for geographic operations.
"""

import logging, bpy
from bpy.types import Operator
from bpy.props import BoolProperty, FloatProperty
from ...config import GEOLOC_PRECISION
from ... import utils

log = logging.getLogger(__name__)


class _bf_set_geoloc:
    """!
    Set geographic location (WGS84).
    """

    bl_label = "Set Geolocation"
    # bl_idname = "scene.bf_set_geoloc"
    bl_description = "Set geographic location (WGS84)"
    bl_options = {"REGISTER", "UNDO"}

    show: BoolProperty(name="Show Geolocation", default=False)

    bf_lon: FloatProperty(
        name="Longitude",
        description="Longitude (WGS84, EPSG:4326) in decimal degrees",
        min=-180,
        max=+180,
        precision=GEOLOC_PRECISION,
    )

    bf_lat: FloatProperty(
        name="Latitude",
        description="Latitude (WGS84, EPSG:4326) in decimal degrees",
        min=-80,
        max=+84,
        precision=GEOLOC_PRECISION,
    )

    def draw(self, context):
        """!
        Draw function for the operator.
        @param context: the Blender context.
        """
        col = self.layout.column(align=True)
        col.label(text="Error on horizontal geoposition < 1 m")
        col.prop(self, "bf_lon", text="Longitude")
        col.prop(self, "bf_lat", text="Latitude")

    def _get_loc(self, context):
        """!
        Placeholder method to get the XYZ location
        @param context: the Blender context.
        @return xyz location
        """
        return 0.0, 0.0, 0.0

    def _set_loc(self, context, xyz):
        """!
        Placeholder method to set the XYZ location
        @param context: the Blender context.
        @param xyz location
        """
        pass

    def execute(self, context):
        # Get loc, use only unmodified z
        _, _, z = self._get_loc(context)
        sc = context.scene
        utm = utils.gis.LonLat(
            lon=self.bf_lon,
            lat=self.bf_lat,
        ).to_UTM(force_zn=sc.bf_origin_utm_zn, force_ne=sc.bf_origin_utm_ne)
        # Compute loc relative to scene origin
        scale_length = sc.unit_settings.scale_length
        x = (utm.easting - sc.bf_origin_utm_easting) / scale_length
        y = (utm.northing - sc.bf_origin_utm_northing) / scale_length
        self._set_loc(context, (x, y, z))
        self.report({"INFO"}, "Geolocation set")
        return {"FINISHED"}

    def invoke(self, context, event):
        sc = context.scene
        # Check origin geolocation
        if not sc.bf_misc_export or not sc.bf_origin_export:
            self.report({"ERROR"}, "Undefined location, set origin geolocation in MISC")
            return {"FINISHED"}
        # Get loc, convert to set default
        x, y, _ = self._get_loc(context)
        scale_length = sc.unit_settings.scale_length
        utm = utils.gis.UTM(
            zn=sc.bf_origin_utm_zn,
            ne=sc.bf_origin_utm_ne,
            easting=sc.bf_origin_utm_easting + x * scale_length,
            northing=sc.bf_origin_utm_northing + y * scale_length,
        )
        lonlat = utm.to_LonLat()
        # Show
        if self.show:
            url = lonlat.to_url()
            bpy.ops.wm.url_open(url=url)
            self.report({"INFO"}, "Geolocation shown")
            return {"FINISHED"}
        # Set defaults
        self.bf_lon = lonlat.lon
        self.bf_lat = lonlat.lat
        # Call dialog
        wm = context.window_manager
        return wm.invoke_props_dialog(self)


class SCENE_OT_bf_set_cursor_geoloc(Operator, _bf_set_geoloc):
    """!
    Set geographic location (WGS84).
    """

    bl_idname = "scene.bf_set_cursor_geoloc"

    @classmethod
    def poll(cls, context):
        return context.scene.cursor

    def _get_loc(self, context):
        return context.scene.cursor.location

    def _set_loc(self, context, xyz):
        context.scene.cursor.location = xyz


class SCENE_OT_bf_set_ob_geoloc(Operator, _bf_set_geoloc):
    """!
    Set geographic location (WGS84).
    """

    bl_idname = "scene.bf_set_ob_geoloc"

    @classmethod
    def poll(cls, context):
        return context.object

    def _get_loc(self, context):
        return context.object.location

    def _set_loc(self, context, xyz):
        context.object.location = xyz


bl_classes = [
    SCENE_OT_bf_set_cursor_geoloc,
    SCENE_OT_bf_set_ob_geoloc,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
