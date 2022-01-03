"""!
BlenderFDS, ui classes.
"""

import logging
from bpy.types import Panel

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


# Register


def register():
    """!
    Register classes and functions to bpy.
    """
    from bpy.utils import register_class, unregister_class

    # for cls in classes_rm:
    #     try:
    #         unregister_class(getattr(bpy.types, cls))
    #     except AttributeError:
    #         log.warning(f"Cannot rm <{cls}>")
    #     else:
    #         log.debug(f"rm <{cls}>")

    for cls in classes:
        register_class(cls)


def unregister():
    """!
    Unregister classes and functions to bpy.
    """
    # reload Blender
    pass
