"""!
BlenderFDS, panel class extensions.
"""

from bpy.types import Panel
from ..types import BFNamelist
from .. import config

# Property panels


class _SCENE_PT_bf_namelist:
    """!
    FDS Panel
    """

    bl_space_type = "PROPERTIES"
    bl_region_type = "WINDOW"
    bl_label = "FDS Generic Scene Panel"
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
        bf_namelist = BFNamelist.get_subclass(cls_name=self.bf_namelist_cls)
        if bf_namelist.bpy_export:  # add export toggle
            self.layout.prop(sc, bf_namelist.bpy_export, icon_only=True)
        if bf_namelist.description and bf_namelist.label:
            self.bl_label = f"FDS {bf_namelist.label} ({bf_namelist.description})"

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
        bf_namelist = BFNamelist.get_subclass(cls_name=self.bf_namelist_cls)
        bf_namelist(sc).draw(context, flow)


class SCENE_PT_bf_case(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS Case Config
    """

    bf_namelist_cls = "SN_config"
    bl_label = "FDS Case Config"

    def draw(self, context):
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        row = layout.row(align=True)  # general operators
        row.operator("scene.bf_show_fds_code", icon="HIDE_OFF")
        row.operator("scene.bf_props_to_scene", icon="COPYDOWN")
        bf_namelist = BFNamelist.get_subclass(cls_name=self.bf_namelist_cls)
        flow = layout.grid_flow(row_major=True, columns=1, even_columns=True)
        bf_namelist(sc).draw(context, flow)


class SCENE_PT_bf_namelist_HEAD(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS HEAD
    """

    bf_namelist_cls = "SN_HEAD"
    bl_label = "FDS HEAD"


class SCENE_PT_bf_namelist_TIME(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS TIME
    """

    bf_namelist_cls = "SN_TIME"
    bl_label = "FDS TIME"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_MISC(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS MISC
    """

    bf_namelist_cls = "SN_MISC"
    bl_label = "FDS MISC"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_REAC(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS REAC
    """

    bf_namelist_cls = "SN_REAC"
    bl_label = "FDS REAC"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_RADI(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS RADI
    """

    bf_namelist_cls = "SN_RADI"
    bl_label = "FDS RADI"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_PRES(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS PRES
    """

    bf_namelist_cls = "SN_PRES"
    bl_label = "FDS PRES"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_DUMP(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS DUMP
    """

    bf_namelist_cls = "SN_DUMP"
    bl_label = "FDS DUMP"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_CATF(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS CATF
    """

    bf_namelist_cls = "SN_CATF"
    bl_label = "FDS CATF"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_config_sizes(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS Default Sizes and Thresholds
    """

    bf_namelist_cls = "SN_config_sizes"
    bl_label = "FDS Default Sizes and Thresholds"
    bl_options = {"DEFAULT_CLOSED"}


class OBJECT_PT_bf_namelist(Panel):
    """!
    FDS geometric namelist
    """

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


class MATERIAL_PT_bf_namelist(Panel):
    """!
    FDS SURF
    """

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
        self.bl_label = f"FDS {bf_namelist.label} Tools"

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        if ob.bf_is_tmp or ob.bf_has_tmp:
            layout.operator("object.bf_show_fds_geometry", icon="HIDE_ON")
            return
        ob.bf_namelist.draw_operators(context, layout)


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


bl_classes = [
    SCENE_PT_bf_case,
    SCENE_PT_bf_namelist_HEAD,
    SCENE_PT_bf_namelist_TIME,
    SCENE_PT_bf_namelist_MISC,
    SCENE_PT_bf_namelist_REAC,
    SCENE_PT_bf_namelist_RADI,
    SCENE_PT_bf_namelist_PRES,
    SCENE_PT_bf_namelist_DUMP,
    SCENE_PT_bf_namelist_CATF,
    SCENE_PT_bf_config_sizes,
    OBJECT_PT_bf_namelist,
    MATERIAL_PT_bf_namelist,
    VIEW3D_PT_bf_ob_namelist_tools,
    VIEW3D_PT_bf_remesh,
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
