# SPDX-License-Identifier: GPL-3.0-or-later

import bpy
from bpy.types import Menu
from bpy.app.translations import contexts as i18n_contexts
from bpy.utils import register_class, unregister_class


class VIEW3D_MT_view(Menu):
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


class VIEW3D_MT_add(Menu):
    bl_label = "Add"
    bl_translation_context = i18n_contexts.operator_default

    def draw(self, context):
        layout = self.layout
        layout.operator_context = "EXEC_REGION_WIN"
        layout.menu("VIEW3D_MT_mesh_add", icon="OUTLINER_OB_MESH")
        layout.menu("VIEW3D_MT_curve_add", icon="OUTLINER_OB_CURVE")
        layout.menu("VIEW3D_MT_surface_add", icon="OUTLINER_OB_SURFACE")
        layout.operator_menu_enum(
            "object.gpencil_add",
            "type",
            text="Grease Pencil",
            icon="OUTLINER_OB_GREASEPENCIL",
        )

        layout.separator()
        layout.operator_menu_enum(
            "object.empty_add", "type", text="Empty", icon="OUTLINER_OB_EMPTY"
        )
        layout.menu("VIEW3D_MT_image_add", text="Image", icon="OUTLINER_OB_IMAGE")

        # FIXME Manage Collection instances
        # layout.separator()
        # has_collections = bool(bpy.data.collections)
        # col = layout.column()
        # col.enabled = has_collections

        # if not has_collections or len(bpy.data.collections) > 10:
        #     col.operator_context = "INVOKE_REGION_WIN"
        #     col.operator(
        #         "object.collection_instance_add",
        #         text="Collection Instance..."
        #         if has_collections
        #         else "No Collections to Instance",
        #         icon="OUTLINER_OB_GROUP_INSTANCE",
        #     )
        # else:
        #     col.operator_menu_enum(
        #         "object.collection_instance_add",
        #         "collection",
        #         text="Collection Instance",
        #         icon="OUTLINER_OB_GROUP_INSTANCE",
        #     )


class VIEW3D_MT_object(Menu):
    bl_context = "objectmode"
    bl_label = "Object"

    def draw(self, _context):
        layout = self.layout

        layout.menu("VIEW3D_MT_transform_object")
        layout.operator_menu_enum(
            "object.origin_set", text="Set Origin", property="type"
        )
        layout.menu("VIEW3D_MT_mirror")
        layout.menu("VIEW3D_MT_object_clear")
        layout.menu("VIEW3D_MT_object_apply")
        layout.menu("VIEW3D_MT_snap")

        layout.separator()

        layout.operator("object.duplicate_move")
        layout.operator("object.duplicate_move_linked")
        layout.operator("object.join")

        layout.separator()

        layout.operator("view3d.copybuffer", text="Copy Objects", icon="COPYDOWN")
        layout.operator("view3d.pastebuffer", text="Paste Objects", icon="PASTEDOWN")

        layout.separator()

        layout.menu("VIEW3D_MT_object_asset")
        # layout.menu("VIEW3D_MT_object_parent")
        layout.menu("VIEW3D_MT_object_collection")
        layout.menu("VIEW3D_MT_object_relations")
        # layout.menu("VIEW3D_MT_object_constraints")
        # layout.menu("VIEW3D_MT_object_track")
        layout.menu("VIEW3D_MT_make_links")

        layout.separator()

        layout.operator("object.shade_smooth")
        layout.operator("object.shade_flat")

        layout.separator()

        # layout.menu("VIEW3D_MT_object_animation")
        # layout.menu("VIEW3D_MT_object_rigid_body")

        # layout.separator()

        # layout.menu("VIEW3D_MT_object_quick_effects")

        # layout.separator()

        layout.menu("VIEW3D_MT_object_convert")

        layout.separator()

        layout.menu("VIEW3D_MT_object_showhide")
        layout.menu("VIEW3D_MT_object_cleanup")

        layout.separator()

        layout.operator_context = "EXEC_REGION_WIN"
        layout.operator("object.delete", text="Delete").use_global = False
        layout.operator("object.delete", text="Delete Global").use_global = True


# Register/Unregister

bl_classes = [VIEW3D_MT_view, VIEW3D_MT_add, VIEW3D_MT_object]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
