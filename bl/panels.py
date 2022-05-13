"""!
BlenderFDS, panel class extensions.
"""

from bpy.types import Panel
from ..lang.SN_config import SN_config, SN_config_sizes
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
    """!
    FDS Panel
    """

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
        """!
        Draw UI elements into the panel’s header UI layout.
        @param context: the Blender context.
        """
        sc = context.scene
        self.bf_namelist(sc).draw_header(
            context=context, layout=self.layout, panel=self
        )

    def draw(self, context):
        """!
        Draw UI elements into the panel UI layout.
        @param context: the Blender context.
        """
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        self.bf_namelist(sc).draw(context, layout)


class SCENE_PT_bf_case(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS Case Config
    """

    bf_namelist = SN_config
    bl_label = "FDS Case Config"

    def draw(self, context):
        sc = context.scene
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # No animation.
        self.bf_namelist(sc).draw(context, layout)


class SCENE_PT_bf_namelist_HEAD(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS HEAD
    """

    bf_namelist = SN_HEAD
    bl_label = "FDS HEAD"


class SCENE_PT_bf_namelist_TIME(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS TIME
    """

    bf_namelist = SN_TIME
    bl_label = "FDS TIME"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_MISC(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS MISC
    """

    bf_namelist = SN_MISC
    bl_label = "FDS MISC"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_REAC(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS REAC
    """

    bf_namelist = SN_REAC
    bl_label = "FDS REAC"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_RADI(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS RADI
    """

    bf_namelist = SN_RADI
    bl_label = "FDS RADI"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_PRES(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS PRES
    """

    bf_namelist = SN_PRES
    bl_label = "FDS PRES"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_namelist_DUMP(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS DUMP
    """

    bf_namelist = SN_DUMP
    bl_label = "FDS DUMP"
    bl_options = {"DEFAULT_CLOSED"}


class SCENE_PT_bf_config_sizes(Panel, _SCENE_PT_bf_namelist):
    """!
    FDS Default Sizes and Thresholds
    """

    bf_namelist = SN_config_sizes
    bl_label = "FDS Default Sizes and Thresholds"
    bl_options = {"DEFAULT_CLOSED"}


class COLLECTION_PT_bf_config(Panel):
    """!
    FDS Collection Config
    """

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
        ob.bf_namelist.draw_header(context=context, layout=self.layout, panel=self)

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        ob.bf_namelist.draw(context, layout)  # draw namelist


class OBJECT_PT_MULT(Panel):
    """!
    FDS MULT namelist
    """

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
        ma.bf_namelist.draw_header(context=context, layout=self.layout, panel=self)

    def draw(self, context):
        ma = context.object.active_material
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False  # no animation
        ma.bf_namelist.draw(context, layout)  # draw namelist


# Toolbar panels


# class VIEW3D_PT_bf_sc_config_tools(Panel):
#     """!
#     Object case tools
#     """

#     bl_idname = "VIEW3D_PT_bf_sc_config_tools"
#     bl_context = "objectmode"
#     bl_category = "FDS"
#     bl_label = "FDS Case Config Tools"
#     bl_space_type = "VIEW_3D"
#     bl_region_type = "UI"

#     @classmethod
#     def poll(cls, context):
#         return context.scene

#     def draw(self, context):
#         layout = self.layout
#         layout.use_property_split = True
#         layout.use_property_decorate = False
#         SN_config(context.scene).draw_operators(context, layout)


class VIEW3D_PT_bf_ob_tools(Panel):
    """!
    Object tools
    """

    bl_idname = "VIEW3D_PT_bf_ob_tools"
    bl_context = "objectmode"
    bl_category = "FDS"
    bl_label = "FDS Tools"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"

    @classmethod
    def poll(cls, context):
        ob = context.object
        return ob and ob.type == "MESH" and not ob.bf_is_tmp  # TODO other types?

    def draw(self, context):
        ob = context.object
        layout = self.layout
        layout.use_property_split = True
        layout.use_property_decorate = False
        ob.bf_namelist.draw_operators(context, layout)


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
    SCENE_PT_bf_config_sizes,
    COLLECTION_PT_bf_config,
    OBJECT_PT_bf_namelist,
    OBJECT_PT_MULT,
    MATERIAL_PT_bf_namelist,
    VIEW3D_PT_bf_mesh_clean_up,
    VIEW3D_PT_bf_ob_tools,
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
