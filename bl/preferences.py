"""!
BlenderFDS, preferences panel.
"""

import bpy
import logging

from bpy.types import AddonPreferences
from bpy.props import BoolProperty, FloatProperty
from . import ui

log = logging.getLogger(__name__)

# Get preference value like this:
# prefs = context.preferences.addons[__package__.split(".")[0]].preferences
# prefs.bf_pref_simplify_ui


def update_bf_pref_simplify_ui(prefs, context):
    ui.toggle_simple_ui(prefs, context)


class BFPreferences(AddonPreferences):
    """!
    BlenderFDS, preferences panel
    """

    bl_idname = __package__.split(".")[0]

    bf_pref_simplify_ui: BoolProperty(  # TODO remove bf_
        name="Simplify UI",
        description="Simplify BlenderFDS user interface",
        default=True,
        update=update_bf_pref_simplify_ui,
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
        box.prop(paths, "use_load_ui")
        box.prop(paths, "use_relative_paths")
        return layout


def register():
    log.debug("Register preferences...")
    bpy.utils.register_class(BFPreferences)


def unregister():
    log.debug("Unregister preferences...")
    bpy.utils.unregister_class(BFPreferences)
