"""!
BlenderFDS, ui classes.
"""

import logging, bpy
from bpy.types import Panel
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
]


class PROPERTIES_PT_navigation_bar_bf(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "NAVIGATION_BAR"
    bl_label = "Navigation Bar"
    bl_options = {"HIDE_HEADER"}

    def draw(self, context):
        ob = context.active_object

        layout = self.layout
        view = context.space_data
        layout.scale_x = 1.4
        layout.scale_y = 1.4

        col = layout.column(align=True)
        col.prop_enum(view, "context", "TOOL", text="", icon="TOOL_SETTINGS")

        col = layout.column(align=True)
        col.prop_enum(view, "context", "SCENE", text="", icon="SCENE_DATA")
        # FIXME bug, when there is no collection, only scene collection
        # col.prop_enum(
        #    view, "context", "COLLECTION", text="", icon="OUTLINER_COLLECTION"
        # )

        col = layout.column(align=True)
        if ob:
            col.prop_enum(view, "context", "OBJECT", text="", icon="OBJECT_DATA")

        if ob and ob.type == "MESH":
            col.prop_enum(view, "context", "MODIFIER", text="", icon="MODIFIER_DATA")

        if ob and (ob.type == "MESH" or ob.type == "EMPTY"):
            col.prop_enum(view, "context", "DATA", text="", icon="MESH_DATA")

        if ob and ob.type == "MESH":
            col.prop_enum(view, "context", "MATERIAL", text="", icon="MATERIAL_DATA")


replacement_classes = (PROPERTIES_PT_navigation_bar_bf,)


def _load_original_classes():
    log.debug(f"Load original ui classes...")
    for cls_name in original_class_names:
        module_name = f"bpy.types.{cls_name}"
        try:
            cls = eval(module_name)
        except:
            log.debug(f"Unknown original class <{module_name}>...")
        original_classes.append(cls)


def _set_simple_ui():
    # Original
    for cls in original_classes:
        # log.debug(f"Unregister original class <{cls}>...")
        unregister_class(cls)
    # Replacement
    for cls in replacement_classes:
        # log.debug(f"Register replacement class <{cls}>...")
        register_class(cls)


def _set_normal_ui():
    # Replacement
    for cls in replacement_classes:
        # log.debug(f"Unregister replacement class <{cls}>...")
        unregister_class(cls)
    # Original
    for cls in original_classes:
        # log.debug(f"Register original class <{cls}>...")
        register_class(cls)


def toggle_simple_ui(prefs=None, context=None, force_normal=False):
    log.debug("Toggle simple ui...")
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
