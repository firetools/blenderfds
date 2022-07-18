# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, panel class extensions.
"""

from bpy.types import Panel
from ..lang.SN_config import SN_config
from ..lang.SN_HEAD import SN_HEAD
from ..lang.SN_TIME import SN_TIME
from ..lang.SN_MISC import SN_MISC
from ..lang.SN_REAC import SN_REAC
from ..lang.SN_RADI import SN_RADI
from ..lang.SN_PRES import SN_PRES
from ..lang.SN_DUMP import SN_DUMP
from ..lang.ON_MULT import ON_MULT, OP_other_MULT_ID

# Property panels


class _SCENE_PT_bf_namelist:
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_label = "FDS Generic Scene Panel"
    bl_context = "scene"

    bf_namelist = SN_HEAD  # example
    layout = None  # example

    @classmethod
    def poll(cls, context):
        return context.scene

    def draw_header(self, context):
        sc = context.scene
        self.bf_namelist(sc).draw_header(
            context=context, layout=self.layout, panel=self
        )

    def draw(self, context):
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        self.bf_namelist(sc).draw(context, layout)


class SCENE_PT_bf_case(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_config
    bl_label = "FDS Case Config"

    def draw(self, context):
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        self.bf_namelist(sc).draw(context, layout)


class SCENE_PT_bf_namelist_HEAD(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_HEAD
    bl_label = "FDS HEAD"


class SCENE_PT_bf_namelist_TIME(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_TIME
    bl_label = "FDS TIME"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_MISC(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_MISC
    bl_label = "FDS MISC"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_REAC(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_REAC
    bl_label = "FDS REAC"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_RADI(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_RADI
    bl_label = "FDS RADI"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_PRES(Panel, _SCENE_PT_bf_namelist):

    bf_namelist = SN_PRES
    bl_label = "FDS PRES"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_DUMP(Panel, _SCENE_PT_bf_namelist):
    bf_namelist = SN_DUMP
    bl_label = "FDS DUMP"
    bl_options = {"DEFAULT_CLOSED"}


class COLLECTION_PT_bf_config(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "collection"
    bl_label = "FDS Collection Config"

    def draw_header(self, context):
        self.layout.prop(
            context.collection,
            "hide_render",
            icon_only=True,
            toggle=False,
            invert_checkbox=True,
        )

    def draw(self, context):
        co = context.collection
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        layout.operator("collection.bf_show_fds_code", icon="HIDE_OFF")
        layout.prop(co, "name")


class OBJECT_PT_bf_namelist(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "FDS Geometric Namelist"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH"  # TODO other types?

    def draw_header(self, context):
        ob = context.object
        ob.bf_namelist.draw_header(context=context, layout=self.layout, panel=self)

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        ob.bf_namelist.draw(context, layout)  # draw namelist


class OBJECT_PT_MULT(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "FDS MULT"
    bl_parent_id = "OBJECT_PT_bf_namelist"
    bl_options = {"DEFAULT_CLOSED"}

    bf_namelist = ON_MULT

    @classmethod
    def poll(cls, context):
        ob = context.object
        return (
            ob
            and ob.type == "MESH"
            and not ob.bf_is_tmp
            and ob.bf_namelist.has_bf_param(OP_other_MULT_ID)
        )

    def draw_header(self, context):
        self.bf_namelist(context.object).draw_header(
            context=context, layout=self.layout, panel=self
        )

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = False  # special
        layout.use_property_decorate = False  # no animation
        self.bf_namelist(context.object).draw(context, layout)  # draw namelist


class MATERIAL_PT_bf_namelist(Panel):
    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "FDS Boundary Condition"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.active_material

    def draw_header(self, context):
        ma = context.object.active_material
        ma.bf_namelist.draw_header(context=context, layout=self.layout, panel=self)

    def draw(self, context):
        ma = context.object.active_material
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        ma.bf_namelist.draw(context, layout)  # draw namelist


# Toolbar panels


class VIEW3D_PT_bf_sc_utils(Panel):
    bl_idname = "VIEW3D_PT_bf_sc_utils"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "FDS Case Utils"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        return context.scene

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False

        # Draw FDS and Smokeview run
        col = layout.column()
        row = col.row(align=True)
        row.operator("scene.bf_run_fds")
        row.operator("scene.bf_eta_fds", icon="SORTTIME", text="")
        col.operator("scene.bf_run_smv")
        col.separator()

        # Draw import snippet
        col = layout.column(align=True)
        col.operator("import_to_current_scene.fds")
        col.separator()


class VIEW3D_PT_bf_ob_utils(Panel):
    bl_idname = "VIEW3D_PT_bf_ob_utils"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = " "  # FDS Namelist Utils"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH" and not ob.bf_is_tmp

    def draw_header(self, context):
        self.layout.label(text=f"FDS {context.object.bf_namelist_cls[3:7]} Utils")

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False

        # Draw Object operators
        ob.bf_namelist.draw_operators(context, layout)


# From properties_da_mesh-py, class DATA_PT_remesh()
class VIEW3D_PT_bf_ob_remesh(Panel):
    bl_idname = "VIEW3D_PT_bf_ob_remesh"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "Remesh"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        row = layout.row()

        mesh = context.object.data  # fix
        row.prop(mesh, "remesh_mode", text="Mode", expand=True)
        col = layout.column()
        if mesh.remesh_mode == "VOXEL":
            col.prop(mesh, "remesh_voxel_size")
            col.prop(mesh, "remesh_voxel_adaptivity")
            col.prop(mesh, "use_remesh_fix_poles")
            # Removed props here
            col.operator("object.voxel_remesh", text="Voxel Remesh")
        else:
            col.operator("object.quadriflow_remesh", text="QuadriFlow Remesh")


class VIEW3D_PT_bf_mesh_clean_up(Panel):
    bl_idname = "VIEW3D_PT_bf_mesh_clean_up"
    bl_context = "mesh_edit"
    bl_category = "FDS"
    bl_label = "Clean Up"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_options = {"DEFAULT_CLOSED"}

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH"

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the Blender context.
        """
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        col = layout.column()
        row = col.row(align=True)
        row.template_header_3D_mode()
        row.menu("VIEW3D_MT_edit_mesh_select_by_trait")
        col.menu("VIEW3D_MT_edit_mesh_clean")


class VIEW3D_PT_bf_geolocation(Panel):
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_category = "FDS"
    bl_label = "Geolocation"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        # Object geolocation
        ob = context.object
        if ob and not ob.bf_is_tmp:
            ob = context.object
            col = layout.column(align=True)
            col.prop(ob, "location", text="Object")
            row = col.row(align=True)
            row.operator("scene.bf_set_ob_geoloc").show = False
            row.operator("scene.bf_set_ob_geoloc", text="", icon="URL").show = True
        # 3D Cursor geolocation
        cursor = context.scene.cursor
        col = layout.column(align=True)
        col.prop(cursor, "location", text="3D Cursor")
        row = col.row(align=True)
        row.operator("scene.bf_set_cursor_geoloc").show = False
        row.operator("scene.bf_set_cursor_geoloc", text="", icon="URL").show = True
        # Online converter
        col = layout.column()
        url = "https://epsg.io/transform#s_srs=4326&t_srs=4326"
        col.operator("wm.url_open", text="Transform Coordinates").url = url


bl_classes = [
    SCENE_PT_bf_case,
    SCENE_PT_bf_namelist_HEAD,
    SCENE_PT_bf_namelist_TIME,
    SCENE_PT_bf_namelist_MISC,
    SCENE_PT_bf_namelist_REAC,
    SCENE_PT_bf_namelist_RADI,
    SCENE_PT_bf_namelist_PRES,
    SCENE_PT_bf_namelist_DUMP,
    COLLECTION_PT_bf_config,
    OBJECT_PT_bf_namelist,
    OBJECT_PT_MULT,
    MATERIAL_PT_bf_namelist,
    VIEW3D_PT_bf_sc_utils,
    VIEW3D_PT_bf_ob_utils,
    VIEW3D_PT_bf_ob_remesh,
    VIEW3D_PT_bf_mesh_clean_up,
    VIEW3D_PT_bf_geolocation,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
