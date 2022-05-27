# SPDX-License-Identifier: GPL-3.0-or-later

from bpy.types import Menu
from bpy.utils import register_class, unregister_class


class TOPBAR_MT_editor_menus(Menu):
    bl_idname = "TOPBAR_MT_editor_menus"
    bl_label = ""

    def draw(self, context):
        layout = self.layout

        # Allow calling this menu directly (this might not be a header area).
        if getattr(context.area, "show_menus", False):
            layout.menu("TOPBAR_MT_blender", text="", icon="BLENDER")
        else:
            layout.menu("TOPBAR_MT_blender", text="Blender")

        layout.menu("TOPBAR_MT_file")
        layout.menu("TOPBAR_MT_edit")

        layout.menu("TOPBAR_MT_window")
        layout.menu("TOPBAR_MT_help")


# Register/Unregister

bl_classes = [
    TOPBAR_MT_editor_menus,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
