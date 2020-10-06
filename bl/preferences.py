"""!
BlenderFDS, preferences panel.
"""

import bpy
import logging

from bpy.types import AddonPreferences
from bpy.props import (
    BoolProperty,
    StringProperty,
    FloatProperty,
    IntProperty,
    EnumProperty,
)

log = logging.getLogger(__name__)

# Get preference value like this:
# prefs = context.preferences.addons[__package__.split(".")[0]].preferences
# prefs.bf_pref_simplify_ui


# Preferences


class BFPreferences(AddonPreferences):
    """!
    BlenderFDS, preferences panel
    """

    bl_idname = __package__.split(".")[0]

    bf_pref_simplify_ui: BoolProperty(  # TODO remove bf_
        name="Simplify UI [Blender restart required]",
        description="Simplify BlenderFDS user interface, Blender restart required",
        default=True,
    )

    bf_pref_appearance: BoolProperty(  # TODO remove bf_
        name="Set Default Appearance",
        description="Automatically set default appearance to Blender Scenes, Objects, Materials,\ndepending on FDS namelist and parameters",
        default=True,
    )

    def update_loglevel(self, context):
        """!
        Update the BlenderFDS log level (DEBUG, INFO, WARNING, ERROR or CRITICAL).
        @param context: the Blender context.
        """
        log.setLevel(self.bf_loglevel)

    bf_loglevel: EnumProperty(  # TODO remove bf_
        name="Log Level",
        description="Select the log level",
        items=[
            ("DEBUG", "Debug", ""),
            ("INFO", "Info", ""),
            ("WARNING", "Warning", ""),
            ("ERROR", "Error", ""),
            ("CRITICAL", "Critical", ""),
        ],
        update=update_loglevel,
        default="INFO",
    )

    min_edge_length: FloatProperty(
        name="Min Edge Length",
        description="Min allowed edge length",
        default=1e-05,
        unit="LENGTH",
    )

    min_face_area: FloatProperty(
        name="Min Face Area",
        description="Min allowed face area",
        default=1e-07,
        unit="AREA",
    )

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the Blender context.
        @return Blender layout.
        """
        paths = context.preferences.filepaths
        layout = self.layout
        box = layout.box()
        box.label(text="User Interface")
        box.operator("wm.bf_load_blenderfds_settings")
        box.prop(self, "bf_pref_simplify_ui")
        box.prop(self, "bf_pref_appearance")
        box.prop(paths, "use_load_ui")
        box.prop(paths, "use_relative_paths")
        box.prop(self, "bf_loglevel")
        box = layout.box()
        box.label(text="Default Sizes and Thresholds")
        box.prop(self, "min_edge_length")
        box.prop(self, "min_face_area")
        return layout


# Register


def register():
    """!
    Load the Python classes and functions to blender.
    """
    bpy.utils.register_class(BFPreferences)


def unregister():
    """!
    Unload the Python classes and functions from blender.
    """
    bpy.utils.unregister_class(BFPreferences)
