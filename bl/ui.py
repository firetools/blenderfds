"""!
BlenderFDS, ui classes.
"""

import logging
from bpy.types import (  # this is the original class for navigation
    PROPERTIES_PT_navigation_bar as PROPERTIES_PT_navigation_bar_original,
)
from bpy.types import Panel
from bpy.utils import register_class, unregister_class

log = logging.getLogger(__name__)

# Simplifying Blender UI, by rewiring existing classes
# 3.0/scripts/startup/bl_ui/space_properties.py


class PROPERTIES_PT_navigation_bar(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "NAVIGATION_BAR"
    bl_label = "Navigation Bar"
    bl_options = {"HIDE_HEADER"}

    def draw(self, context):
        layout = self.layout
        view = context.space_data
        layout.scale_x = 1.4
        layout.scale_y = 1.4

        col = layout.column(align=True)
        col.prop_enum(view, "context", "TOOL", text="", icon="TOOL_SETTINGS")

        col = layout.column(align=True)
        col.prop_enum(view, "context", "SCENE", text="", icon="SCENE_DATA")
        col.prop_enum(
            view, "context", "COLLECTION", text="", icon="OUTLINER_COLLECTION"
        )

        ob = context.active_object
        if not ob:
            return
        col = layout.column(align=True)
        col.prop_enum(view, "context", "OBJECT", text="", icon="OBJECT_DATA")

        if ob.type != "MESH":
            return
        col.prop_enum(view, "context", "MODIFIER", text="", icon="MODIFIER_DATA")
        col.prop_enum(view, "context", "DATA", text="", icon="MESH_DATA")
        col.prop_enum(view, "context", "MATERIAL", text="", icon="MATERIAL_DATA")


# Register


def register():
    register_class(PROPERTIES_PT_navigation_bar)


def unregister():
    unregister_class(PROPERTIES_PT_navigation_bar)
    register_class(PROPERTIES_PT_navigation_bar_original)
