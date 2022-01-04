"""!
BlenderFDS, panel class extensions.
"""

import bpy
from bpy.types import Panel, bpy_struct
from ..types import BFNamelist

from .. import lang, config

bl_classes = list()
bf_classes = list()


def subscribe(cls):
    """!
    Subscribe class to related collection.
    @param cls: the class to subscribe.
    @return the class subscribed.
    """
    if issubclass(cls, bpy_struct):
        bl_classes.append(cls)
    else:
        bf_classes.append(cls)
    return cls


# Property panels


class SCENE_PT_bf_namelist:
    """!
    FDS Panel
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_label = "FDS Panel"
    bl_context = "scene"

    bf_namelist_cls = "SN_HEAD"  # example
    layout = None  # example

    @classmethod
    def poll(cls, context):
        return context.scene

    def draw_header(self, context):
        """!
        Draw UI elements into the panel’s header UI layout.
        @param context: the Blender context.
        """
        sc = context.scene
        # Manage Scene
        bf_namelist = BFNamelist.subclasses_by_cls_name[self.bf_namelist_cls]
        if bf_namelist.bpy_export:
            self.layout.prop(sc, bf_namelist.bpy_export, icon_only=True)
        if bf_namelist.description:
            self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        else:
            self.bl_label = bf_namelist.label

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the Blender context.
        """
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        bf_namelist = BFNamelist.subclasses_by_cls_name[self.bf_namelist_cls]
        bf_namelist(sc).draw(context, flow)


@subscribe
class SCENE_PT_bf_case(Panel, SCENE_PT_bf_namelist):
    """!
    FDS Case Config
    """

    bf_namelist_cls = "SN_config"
    bl_label = "FDS Case"

    def draw(self, context):
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        row = layout.row(align=True)  # general operators
        row.operator("scene.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("scene.bf_props_to_scene", icon="COPYDOWN")
        bf_namelist = BFNamelist.subclasses_by_cls_name[self.bf_namelist_cls]
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        bf_namelist(sc).draw(context, flow)


@subscribe
class SCENE_PT_bf_config_sizes(Panel, SCENE_PT_bf_namelist):
    """!
    Default Sizes and Thresholds
    """

    bf_namelist_cls = "SN_config_sizes"
    bl_label = "Default Sizes and Thresholds"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_config_units(Panel, SCENE_PT_bf_namelist):
    """!
    Units configuration
    """

    bf_namelist_cls = "SN_config_units"
    bl_label = "Units"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_HEAD(Panel, SCENE_PT_bf_namelist):
    """!
    FDS HEAD
    """

    bf_namelist_cls = "SN_HEAD"
    bl_label = "FDS HEAD"
    bl_parent_id = "SCENE_PT_bf_case"


@subscribe
class SCENE_PT_bf_namelist_TIME(Panel, SCENE_PT_bf_namelist):
    """!
    FDS TIME
    """

    bf_namelist_cls = "SN_TIME"
    bl_label = "FDS TIME"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_MISC(Panel, SCENE_PT_bf_namelist):
    """!
    FDS MISC
    """

    bf_namelist_cls = "SN_MISC"
    bl_label = "FDS MISC"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_REAC(Panel, SCENE_PT_bf_namelist):
    """!
    FDS REAC
    """

    bf_namelist_cls = "SN_REAC"
    bl_label = "FDS REAC"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_RADI(Panel, SCENE_PT_bf_namelist):
    """!
    FDS RADI
    """

    bf_namelist_cls = "SN_RADI"
    bl_label = "FDS RADI"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_PRES(Panel, SCENE_PT_bf_namelist):
    """!
    FDS PRES
    """

    bf_namelist_cls = "SN_PRES"
    bl_label = "FDS PRES"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_DUMP(Panel, SCENE_PT_bf_namelist):
    """!
    FDS DUMP
    """

    bf_namelist_cls = "SN_DUMP"
    bl_label = "FDS DUMP"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class SCENE_PT_bf_namelist_CATF(Panel, SCENE_PT_bf_namelist):
    """!
    FDS CATF
    """

    bf_namelist_cls = "SN_CATF"
    bl_label = "FDS CATF"
    bl_parent_id = "SCENE_PT_bf_case"
    bl_options = {"DEFAULT_CLOSED"}


@subscribe
class OBJECT_PT_bf_namelist(Panel):
    """!
    FDS geometric namelist
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "object"
    bl_label = "FDS Namelist"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH"  # TODO other types?

    def draw_header(self, context):
        ob = context.object
        if ob.bf_is_tmp:
            self.bl_label = f"FDS Temp Geometry"
            return
        bf_namelist = ob.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ob, "hide_render", emboss=False, icon_only=True)

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        row = layout.row(align=True)  # general operators
        if ob.bf_is_tmp:
            row.operator("object.bf_show_fds_geometry", icon="HIDE_ON")
            return
        if ob.bf_has_tmp:
            row.operator("object.bf_show_fds_geometry", icon="HIDE_ON")
        else:
            row.operator("object.bf_show_fds_geometry", icon="HIDE_OFF")
        row.operator("object.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("object.bf_props_to_sel_obs", icon="COPYDOWN")
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        flow.prop(ob, "bf_namelist_cls")  # draw namelist choice
        ob.bf_namelist.draw(context, flow)  # draw namelist


@subscribe
class MATERIAL_PT_bf_namelist(Panel):
    """!
    FDS SURF
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_context = "material"
    bl_label = "FDS SURF"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.active_material

    def draw_header(self, context):
        ma = context.object.active_material
        # Manage predefined Material
        if ma.name in config.default_mas:
            self.bl_label = f"FDS Predefined {ma.name}"
            return
        # Manage Material
        bf_namelist = ma.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"
        self.layout.prop(ma, "bf_surf_export", icon_only=True)

    def draw(self, context):
        ma = context.object.active_material
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        row = layout.row(align=True)  # general operators
        if ma.name in config.default_mas:
            row.prop(ma, "diffuse_color")
            return
        row.operator("material.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("material.bf_surf_to_sel_obs", icon="COPYDOWN")
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        flow.prop(ma, "bf_namelist_cls")  # draw namelist choice
        ma.bf_namelist.draw(context, flow)  # draw namelist


# Toolbar panels


@subscribe
class VIEW3D_PT_bf_ob_namelist_tools(Panel):
    """!
    Object namelist tools
    """

    bl_idname = "VIEW3D_PT_bf_ob_namelist_tools"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "FDS Namelist Tools"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH"  # TODO other types?

    def draw_header(self, context):
        ob = context.object
        if ob.bf_is_tmp:
            self.bl_label = f"FDS Temp Geometry"
            return
        bf_namelist = ob.bf_namelist
        self.bl_label = f"FDS {bf_namelist.label}"

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        if ob.bf_is_tmp or ob.bf_has_tmp:
            layout.operator("object.bf_show_fds_geometry", icon="HIDE_ON")
            return
        ob.bf_namelist.draw_operators(context, layout)


@subscribe
class VIEW3D_PT_bf_remesh(Panel):
    """!
    Object remesh panel
    """

    bl_idname = "VIEW3D_PT_bf_remesh"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "Remesh"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_options = {"DEFAULT_CLOSED"}

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the Blender context.
        @return current object
        """
        ob = context.active_object
        return ob and ob.type == "MESH"

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the Blender context.
        """
        # See: properties_data_mesh.py, class DATA_PT_remesh
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        row = layout.row()
        mesh = context.active_object.data  # FIXME ?
        row.prop(mesh, "remesh_mode", text="Mode", expand=True)
        col = layout.column()
        if mesh.remesh_mode == "VOXEL":
            col.prop(mesh, "remesh_voxel_size")
            col.prop(mesh, "remesh_voxel_adaptivity")
            col.prop(mesh, "use_remesh_fix_poles")

            col = layout.column(heading="Preserve")
            col.prop(mesh, "use_remesh_preserve_volume", text="Volume")
            col.prop(mesh, "use_remesh_preserve_paint_mask", text="Paint Mask")
            col.prop(mesh, "use_remesh_preserve_sculpt_face_sets", text="Face Sets")
            if context.preferences.experimental.use_sculpt_vertex_colors:
                col.prop(
                    mesh, "use_remesh_preserve_vertex_colors", text="Vertex Colors"
                )
            col.operator("object.voxel_remesh", text="Voxel Remesh")
        else:
            col.operator("object.quadriflow_remesh", text="QuadriFlow Remesh")


@subscribe
class VIEW3D_PT_bf_mesh_clean_up(Panel):
    """!
    Mesh clean up panel
    """

    bl_idname = "VIEW3D_PT_bf_mesh_clean_up"
    bl_context = "mesh_edit"
    bl_category = "FDS"
    bl_label = "Clean Up"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_options = {"DEFAULT_CLOSED"}

    @classmethod
    def poll(cls, context):
        """!
        If this method returns a non-null output, then the panel can be drawn
        @param context: the Blender context.
        @return current object
        """
        ob = context.active_object
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
        col.label(text=context.screen.statusbar_info().split(" | Tris:", 1)[0])
        row = col.row(align=True)
        row.template_header_3D_mode()
        row.menu("VIEW3D_MT_edit_mesh_select_by_trait")
        col.menu("VIEW3D_MT_edit_mesh_clean")


@subscribe
class VIEW3D_PT_bf_geolocation(Panel):
    """!
    Geolocation panel
    """

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


@subscribe
class VIEW3D_PT_bf_view3d_properties(Panel):  # FIXME set view clipping automatically
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_category = "FDS"
    bl_label = "View"
    bl_options = {"DEFAULT_CLOSED"}

    def draw(self, context):
        layout = self.layout
        view = context.space_data
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        col = layout.column()

        subcol = col.column(align=True)
        subcol.prop(view, "clip_start", text="Clip Start")
        subcol.prop(view, "clip_end", text="End")


# Register


def register():
    """!
    Load the Python classes and functions to blender.
    """
    for cls in bl_classes:
        bpy.utils.register_class(cls)


def unregister():
    """!
    Unload the Python classes and functions from blender.
    """
    for cls in bl_classes:
        bpy.utils.unregister_class(cls)
