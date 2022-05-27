# SPDX-License-Identifier: GPL-3.0-or-later

from bpy.types import Panel, Scene, Object, Collection
from bpy.utils import register_class, unregister_class


class PROPERTIES_PT_navigation_bar(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "NAVIGATION_BAR"
    bl_label = "Navigation Bar"
    bl_options = {"HIDE_HEADER"}

    def draw(self, context):
        space = context.space_data
        pin_id = space.pin_id

        layout = self.layout
        layout.scale_x = 1.4
        layout.scale_y = 1.4

        # Check space.context for strange situations
        if space.context not in (
            "OBJECT",
            "DATA",
            "MATERIAL",
            "SCENE",
            "COLLECTION",
            "MODIFIER",
        ):
            space.context = "SCENE"

        # Set UI
        if not pin_id:
            sc = context.scene
            ob = context.active_object
            co = context.collection

            col = layout.column(align=True)
            col.prop_enum(space, "context", "SCENE", text="", icon="SCENE_DATA")

            if co != sc.collection:
                col.prop_enum(
                    space, "context", "COLLECTION", text="", icon="OUTLINER_COLLECTION"
                )
            if ob:
                col = layout.column(align=True)
                col.prop_enum(space, "context", "OBJECT", text="", icon="OBJECT_DATA")
                if ob.type == "MESH":
                    col.prop_enum(
                        space, "context", "MODIFIER", text="", icon="MODIFIER_DATA"
                    )
                if ob.type == "MESH" or ob.type == "EMPTY":
                    col.prop_enum(space, "context", "DATA", text="", icon="MESH_DATA")
                if ob.type == "MESH":
                    col.prop_enum(
                        space, "context", "MATERIAL", text="", icon="MATERIAL_DATA"
                    )

        else:
            if isinstance(pin_id, Scene):
                sc = pin_id
                col = layout.column(align=True)
                col.prop_enum(space, "context", "SCENE", text="", icon="SCENE_DATA")
            elif isinstance(pin_id, Collection):
                co = pin_id
                col = layout.column(align=True)
                col.prop_enum(
                    space, "context", "COLLECTION", text="", icon="OUTLINER_COLLECTION"
                )
            elif isinstance(pin_id, Object):
                ob = pin_id
                col = layout.column(align=True)
                col.prop_enum(space, "context", "OBJECT", text="", icon="OBJECT_DATA")
                if ob.type == "MESH":
                    col.prop_enum(
                        space, "context", "MODIFIER", text="", icon="MODIFIER_DATA"
                    )
                if ob.type == "MESH" or ob.type == "EMPTY":
                    col.prop_enum(space, "context", "DATA", text="", icon="MESH_DATA")
                if ob.type == "MESH":
                    col.prop_enum(
                        space, "context", "MATERIAL", text="", icon="MATERIAL_DATA"
                    )


# Register/Unregister

bl_classes = [
    PROPERTIES_PT_navigation_bar,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
