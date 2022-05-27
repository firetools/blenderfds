# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, simplify Blender ui classes.
"""

import logging, bpy
from bpy.utils import register_class, unregister_class
from . import bf_ui

log = logging.getLogger(__name__)

## name of original Blender ui class to be checked for existance
bl_check_cls_name = "WORKSPACE_PT_addons"

## names of original Blender ui classes to be unregistered
bl_cls_names = list()

# from: 3.0/scripts/startup/bl_ui/properties_workspace.py
bl_cls_names.extend(
    (
        # "WORKSPACE_PT_main",
        "WORKSPACE_PT_addons",
        "WORKSPACE_PT_custom_props",
    )
)

# from: 3.0/scripts/startup/bl_ui/properties_scene.py
bl_cls_names.extend(
    (
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
    )
)

# from: 3.0/scripts/startup/bl_ui/properties_collection.py
bl_cls_names.extend(
    (
        # "COLLECTION_PT_collection_flags",
        # "COLLECTION_PT_instancing",
        "COLLECTION_PT_lineart_collection",
    )
)

# from: 3.0/scripts/startup/bl_ui/properties_object.py
bl_cls_names.extend(
    (
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
    )
)

# from: 3.0/scripts/startup/bl_ui/properties_data_mesh.py
bl_cls_names.extend(
    (
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
    )
)

# from: 3.0/scripts/startup/bl_ui/properties_material.py
bl_cls_names.extend(
    (
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
    )
)

# Replaced classes
# from: 3.0/scripts/startup/bl_ui/space_properties.py
bl_cls_names.extend(
    (
        "PROPERTIES_PT_navigation_bar",
        "TOPBAR_MT_editor_menus",
        "VIEW3D_MT_view",
        "VIEW3D_MT_add",
        "VIEW3D_MT_object",
    )
)

## references to original Blender ui classes to unregister
bl_cls_refs = list()


def _load_bl_cls_refs():
    log.debug(f"Load original Blender ui classes...")
    for n in bl_cls_names:
        module_name = f"bpy.types.{n}"
        # Check existance
        try:
            cls = eval(module_name)
            log.debug(f"Load: Found original ui class <{module_name}>...")
        except:
            log.debug(f"Load: Unknown original ui class <{module_name}>...")
        else:
            bl_cls_refs.append(cls)


def _set_simple_ui():
    log.debug("Set simple ui...")

    # Check already simple
    if not hasattr(bpy.types, bl_check_cls_name):
        log.debug("Already simple, do not touch.")
        return

    # Unregister original
    for cls in bl_cls_refs:
        try:
            unregister_class(cls)
        except:
            log.debug(f"Unregister: unknown original class <{cls}>")

    # Register new ui
    bf_ui.register()


def _set_normal_ui():
    log.debug("Set normal ui...")

    # Check already normal
    if hasattr(bpy.types, bl_check_cls_name):
        return

    # Unregister new ui
    bf_ui.unregister()

    # Register original
    for cls in bl_cls_refs:
        try:
            register_class(cls)
        except:
            log.debug(f"Register: unknown original class <{cls}>")


def toggle_simple_ui(prefs=None, context=None, force_normal=False):
    log.info("Toggle simple ui...")

    # First start, load original references
    if not bl_cls_refs:
        _load_bl_cls_refs()

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


# Register/Unregister


def register():
    toggle_simple_ui()  # at start, check


def unregister():
    toggle_simple_ui(force_normal=True)  # at end, restore
