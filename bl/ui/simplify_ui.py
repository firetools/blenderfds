# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, ui classes.
"""

import logging, bpy

from bpy.types import Menu, Panel, Scene, Object, Collection
from bpy.utils import register_class, unregister_class

log = logging.getLogger(__name__)


# Simplifying Blender UI, by rewiring existing classes

original_classes = list()
original_class_names = [
    ## from: 3.0/scripts/startup/bl_ui/properties_workspace.py
    # "WORKSPACE_PT_main",
    "WORKSPACE_PT_addons",
    "WORKSPACE_PT_custom_props",
    ## from: 3.0/scripts/startup/bl_ui/properties_scene.py
    # "SCENE_UL_keying_set_paths",
    "SCENE_PT_scene",
    # "SCENE_PT_unit",  # for BlenderBIM addon compat
    "SCENE_PT_physics",
    "SCENE_PT_keying_sets",
    "SCENE_PT_keying_set_paths",
    "SCENE_PT_keyframing_settings",
    "SCENE_PT_audio",
    "SCENE_PT_rigid_body_world",
    "SCENE_PT_rigid_body_world_settings",
    "SCENE_PT_rigid_body_cache",
    "SCENE_PT_rigid_body_field_weights",
    "SCENE_PT_custom_props",
    ## from: 3.0/scripts/startup/bl_ui/properties_collection.py
    # "COLLECTION_PT_collection_flags",
    # "COLLECTION_PT_instancing",
    "COLLECTION_PT_lineart_collection",
    ## from: 3.0/scripts/startup/bl_ui/properties_object.py
    # "OBJECT_PT_context_object",
    # "OBJECT_PT_transform",  # for BlenderBIM addon compat
    # "OBJECT_PT_delta_transform",
    # "OBJECT_PT_relations",
    # "COLLECTION_MT_context_menu",
    # "OBJECT_PT_collections",
    "OBJECT_PT_instancing",
    "OBJECT_PT_instancing_size",
    "OBJECT_PT_motion_paths",
    "OBJECT_PT_motion_paths_display",
    # "OBJECT_PT_display",
    # "OBJECT_PT_visibility",
    "OBJECT_PT_lineart",
    "OBJECT_PT_custom_props",
    ## from: 3.0/scripts/startup/bl_ui/properties_data_mesh.py
    # "MESH_MT_vertex_group_context_menu",
    # "MESH_MT_shape_key_context_menu",
    # "MESH_UL_vgroups",
    # "MESH_UL_fmaps",
    # "MESH_UL_shape_keys",
    # "MESH_UL_uvmaps",
    # "MESH_UL_vcols",
    # "MESH_UL_attributes",
    # "DATA_PT_context_mesh",
    "DATA_PT_vertex_groups",
    "DATA_PT_shape_keys",
    "DATA_PT_uv_texture",
    "DATA_PT_vertex_colors",
    "DATA_PT_sculpt_vertex_colors",
    "DATA_PT_face_maps",
    "DATA_PT_mesh_attributes",
    "DATA_PT_normals",
    "DATA_PT_texture_space",
    # "DATA_PT_remesh",
    "DATA_PT_customdata",
    "DATA_PT_custom_props_mesh",
    ## from: 3.0/scripts/startup/bl_ui/properties_material.py
    # "MATERIAL_MT_context_menu",
    # "MATERIAL_UL_matslots",
    # "MATERIAL_PT_preview",
    # "EEVEE_MATERIAL_PT_context_material",
    "EEVEE_MATERIAL_PT_surface",
    "EEVEE_MATERIAL_PT_volume",
    "EEVEE_MATERIAL_PT_settings",
    "MATERIAL_PT_lineart",
    "MATERIAL_PT_viewport",
    "EEVEE_MATERIAL_PT_viewport_settings",
    "MATERIAL_PT_custom_props",
    ## from: 3.0/scripts/startup/bl_ui/space_properties.py
    "PROPERTIES_PT_navigation_bar",
    "TOPBAR_MT_editor_menus",
    "VIEW3D_MT_view",
]


class TOPBAR_MT_editor_menus_bf(Menu):
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


class VIEW3D_MT_view_bf(Menu):
    bl_idname = "VIEW3D_MT_view"
    bl_label = "View"

    def draw(self, context):
        layout = self.layout
        view = context.space_data

        layout.prop(view, "show_region_toolbar")
        layout.prop(view, "show_region_ui")
        layout.prop(view, "show_region_tool_header")
        layout.prop(view, "show_region_hud")

        layout.separator()

        layout.operator(
            "view3d.view_selected", text="Frame Selected"
        ).use_all_regions = False
        if view.region_quadviews:
            layout.operator(
                "view3d.view_selected", text="Frame Selected (Quad View)"
            ).use_all_regions = True

        layout.operator("view3d.view_all").center = False
        layout.operator("view3d.view_persportho", text="Perspective/Orthographic")
        layout.menu("VIEW3D_MT_view_local")

        layout.separator()
        layout.menu("VIEW3D_MT_view_viewpoint")
        layout.menu("VIEW3D_MT_view_navigation")
        layout.menu("VIEW3D_MT_view_align")

        layout.separator()

        layout.menu("INFO_MT_area")


class PROPERTIES_PT_navigation_bar_bf(Panel):
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
            "TOOL",
        ):
            space.context = "SCENE"

        # Set UI
        col = layout.column(align=True)
        col.prop_enum(space, "context", "TOOL", text="", icon="TOOL_SETTINGS")

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


replacement_classes = (
    TOPBAR_MT_editor_menus_bf,
    PROPERTIES_PT_navigation_bar_bf,
    VIEW3D_MT_view_bf,
)


def _load_original_classes():
    log.debug(f"Load original ui classes...")
    for cls_name in original_class_names:
        module_name = f"bpy.types.{cls_name}"
        try:
            cls = eval(module_name)
        except:
            log.debug(f"Load: Unknown original class <{module_name}>...")
        else:
            original_classes.append(cls)


def _set_simple_ui():
    # Check already simple
    if hasattr(bpy.types, replacement_classes[0].__name__):
        return
    # Set
    for cls in original_classes:
        try:
            unregister_class(cls)
        except:
            log.debug(f"Unregister: unknown original class <{cls}>...")
    for cls in replacement_classes:
        register_class(cls)


def _set_normal_ui():
    # Check already normal
    if not hasattr(bpy.types, replacement_classes[0].__name__):
        return
    # Set
    for cls in replacement_classes:
        unregister_class(cls)
    for cls in original_classes:
        try:
            register_class(cls)
        except:
            log.debug(f"Register: unknown original class <{cls}>...")


def toggle_simple_ui(prefs=None, context=None, force_normal=False):
    log.info("Toggle simple ui...")
    if not original_classes:
        _load_original_classes()
    if force_normal:
        _set_normal_ui()
        return
    if context is None:
        context = bpy.context
    if prefs is None:
        prefs = context.preferences.addons[__package__.split(".")[0]].preferences
    if prefs.bf_pref_simplify_ui:
        _set_simple_ui()
    else:
        _set_normal_ui()


def register():
    toggle_simple_ui()  # at start, check


def unregister():
    toggle_simple_ui(force_normal=True)  # at end, restore
