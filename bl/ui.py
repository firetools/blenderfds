"""!
BlenderFDS, ui classes
"""

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

import bpy, logging
from bpy.types import Panel, Menu

log = logging.getLogger(__name__)

# Collections

classes = list()


def subscribe(cls):
    """!
    Subscribe class to related collection.
    @param cls: the class to subscribe.
    @return the class subscribed.
    """
    classes.append(cls)
    return cls


classes_rm = (
    "PROPERTIES_PT_navigation_bar",
    "TOPBAR_MT_editor_menus",
    "SCENE_PT_scene",
    "SCENE_PT_unit",
    "SCENE_PT_keyframing_settings",
    "SCENE_PT_keying_set_paths",
    "SCENE_PT_keying_sets",
    "SCENE_PT_audio",
    "SCENE_PT_physics",
    "SCENE_PT_rigid_body_world",
    "SCENE_PT_custom_props",
    "OBJECT_PT_motion_paths",
    "OBJECT_PT_motion_paths_display",
    "OBJECT_PT_custom_props",
    "MATERIAL_PT_preview",
    "EEVEE_MATERIAL_PT_surface",
    "EEVEE_MATERIAL_PT_volume",
    "EEVEE_MATERIAL_PT_settings",
    "MATERIAL_PT_viewport",
    "MATERIAL_PT_custom_props",
)

# Simplifying Blender UI, by rewiring existing classes
# 2.80/scripts/startup/bl_ui/space_topbar.py


@subscribe
class PROPERTIES_PT_navigation_bar(Panel):
    """!
    Navigation Bar
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "NAVIGATION_BAR"
    bl_label = "Navigation Bar"
    bl_options = {"HIDE_HEADER"}

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        layout = self.layout
        view = context.space_data
        layout.scale_x = 1.4
        layout.scale_y = 1.4
        # layout.prop_tabs_enum(view, "context", icon_only=True)  # original
        layout.prop_enum(view, "context", "TOOL", text="", icon="TOOL_SETTINGS")
        layout.prop_enum(view, "context", "SCENE", text="", icon="SCENE_DATA")
        col = layout.column(align=True)
        ob = context.active_object
        if ob:
            col.prop_enum(view, "context", "OBJECT", text="", icon="OBJECT_DATA")
            if ob.type == "MESH":
                col.prop_enum(
                    view, "context", "MODIFIER", text="", icon="MODIFIER_DATA"
                )
                col.prop_enum(view, "context", "DATA", text="", icon="MESH_DATA")
                col.prop_enum(
                    view, "context", "MATERIAL", text="", icon="MATERIAL_DATA"
                )


@subscribe
class TOPBAR_MT_editor_menus(Menu):
    """!
    Editor menu
    """

    bl_idname = "TOPBAR_MT_editor_menus"
    bl_label = ""

    def draw(self, _context):
        """!
        Draw UI elements into the menu UI layout.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        """
        layout = self.layout
        layout.menu("TOPBAR_MT_app", text="", icon="BLENDER")
        layout.menu("TOPBAR_MT_file")
        layout.menu("TOPBAR_MT_edit")
        layout.menu("TOPBAR_MT_window")
        layout.menu("TOPBAR_MT_help")


# Register


def register():
    """!
    Register classes and functions to bpy.
    """
    from bpy.utils import register_class, unregister_class

    for cls in classes_rm:
        try:
            unregister_class(getattr(bpy.types, cls))
        except AttributeError:
            log.warning(f"Cannot rm <{cls}>")
        else:
            log.debug(f"rm <{cls}>")

    for cls in classes:
        register_class(cls)


def unregister():
    """!
    Unregister classes and functions to bpy.
    """
    # reload Blender
    pass
