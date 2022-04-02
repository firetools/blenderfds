import logging
from bpy.types import Material
from bpy.props import EnumProperty
from ..types import (
    BFParam,
    BFParamOther,
    BFParamFYI,
    BFNamelist,
    BFException,
    FDSParam,
    FDSList,
)
from ..bl.ui_lists import WM_PG_bf_other, WM_UL_bf_other_items
from .. import config

log = logging.getLogger(__name__)


class BFMaterial:
    """!
    Extension of Blender Material.
    """

    @property
    def bf_namelist(self):
        """!
        Return related bf_namelist, instance of BFNamelist.
        """
        return BFNamelist.get_subclass(cls_name=self.bf_namelist_cls)(element=self)

    def to_fds_list(self, context) -> FDSList:
        """!
        Return the FDSList instance from self, never None.
        """
        return self.bf_namelist.to_fds_list(context)

    def from_fds(self, context, fds_namelist):
        """!
        Set self.bf_namelist from FDSNamelist, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: FDSNamelist.
        """
        # Get subclass from offered fds_label, set bf_namelist_cls
        bf_namelist = BFNamelist.get_subclass(fds_label=fds_namelist.fds_label)
        self.bf_namelist_cls = bf_namelist.__name__
        # Import
        self.bf_namelist.from_fds(context, fds_namelist=fds_namelist)
        self.use_fake_user = True  # eg. used by CTRL

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Material.bf_namelist = cls.bf_namelist
        Material.to_fds_list = cls.to_fds_list
        Material.from_fds = cls.from_fds

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Material.from_fds
        del Material.to_fds_list
        del Material.bf_namelist


# Before updating the items, the lang classes should be imported
# Only after updating the items, the MP_namelist_cls can be registered
def update_MP_namelist_cls_items():
    items = [
        (cls.__name__, cls.label, cls.description, cls.enum_id)
        for cls in BFNamelist.subclasses
        if cls.bpy_type == Material and cls.enum_id
    ]
    items.sort(key=lambda k: k[1])
    MP_namelist_cls.bpy_other["items"] = items
    # log.debug(f"Updated MP_namelist_cls items (before registration): {items}")


def update_MP_namelist_cls(self, context):
    self.bf_namelist.set_appearance(context)


class MP_namelist_cls(BFParam):
    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Material
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (("MN_SURF", "SURF", "Generic boundary condition", 2000),),
        "update": update_MP_namelist_cls,
    }
    bpy_default = "MN_SURF"

    def get_exported(self, context):
        return False

    def draw_operators(self, context, layout):
        layout.operator("material.bf_surf_to_sel_obs", icon="COPYDOWN", text="")


class MP_ID(BFParam):
    label = "ID"
    description = "Material identification name"
    fds_label = "ID"
    bpy_type = Material
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"

    def copy_to(self, context, dest_element):
        pass

    def draw(self, context, layout):
        row = layout.row(align=True)
        row.prop(self.element, "name", text="ID")
        row.prop(self.element, "use_fake_user", text="")  # force export


class MP_FYI(BFParamFYI):
    bpy_type = Material
    bpy_idname = "bf_fyi"


class MP_RGB(BFParam):
    label = "RGB"
    description = "Red, green, blue components of color"
    fds_label = "RGB"
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def set_value(self, context, value):
        c = self.element.diffuse_color
        c[0], c[1], c[2] = value[0] / 255.0, value[1] / 255.0, value[2] / 255.0

    def to_fds_list(self, context) -> FDSList:
        c = self.element.diffuse_color
        rgb = (int(c[0] * 255), int(c[1] * 255), int(c[2] * 255))
        if c[3] == 1.0:  # do not send TRANSPARENCY if it is 1
            return FDSParam(
                fds_label="RGB",
                value=rgb,
            )
        else:
            return FDSList(
                (
                    FDSParam(fds_label="RGB", value=rgb),
                    FDSParam(fds_label="TRANSPARENCY", value=c[3], precision=2),
                )
            )


class MP_COLOR(BFParam):  # only import
    label = "COLOR"
    description = "Color"
    fds_label = "COLOR"
    bpy_type = Material
    bpy_prop = None  # Do not register

    def set_value(self, context, value):
        c = self.element.diffuse_color
        rgb = config.fds_colors.get(value)
        if not rgb:
            raise BFException(self, f"Unknown color <{value}>")
        c[0], c[1], c[2] = rgb[0] / 255.0, rgb[1] / 255.0, rgb[2] / 255.0

    def get_exported(self, context):
        return False


class MP_TRANSPARENCY(BFParam):  # only import
    label = "TRANSPARENCY"
    description = "Red, green, blue components of color and transparency"
    fds_label = "TRANSPARENCY"
    bpy_type = Material
    bpy_prop = None  # Do not register

    def set_value(self, context, value):
        c = self.element.diffuse_color
        c[3] = value

    def get_exported(self, context):
        return False


class MP_other(BFParamOther):
    bpy_type = Material
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items
