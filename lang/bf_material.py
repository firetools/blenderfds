# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Material
from bpy.props import EnumProperty
from ..types import (
    BFParam,
    BFParamOther,
    BFParamFYI,
    BFNamelist,
    BFException,
    BFNotImported,
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

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Material.bf_namelist = cls.bf_namelist
        Material.to_fds_list = cls.to_fds_list

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
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

    def draw_operators(self, context, layout):
        layout.operator("material.bf_props_to_ma", icon="COPYDOWN", text="")


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
    label = "RGB, TRANSPARENCY"
    description = "Set color and transparency of the boundary condition"
    fds_label = "RGB"
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def get_value(self, context):
        c = getattr(self.element, self.bpy_idname)
        return (int(c[0] * 255), int(c[1] * 255), int(c[2] * 255))

    def set_value(self, context, value):
        c = getattr(self.element, self.bpy_idname)
        c[0], c[1], c[2] = value[0] / 255.0, value[1] / 255.0, value[2] / 255.0


class MP_COLOR(BFParam):  # only import
    label = "COLOR"
    description = "Color"
    fds_label = "COLOR"
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def get_exported(self, context):
        return False

    def set_value(self, context, value):
        c = getattr(self.element, self.bpy_idname)
        rgb = config.FDS_COLORS.get(value)
        if not rgb:
            raise BFException(self, f"Unknown color <{value}>")
        c[0], c[1], c[2] = rgb[0] / 255.0, rgb[1] / 255.0, rgb[2] / 255.0

    def draw(self, context, layout):  # see MP_RGB
        pass


class MP_TRANSPARENCY(BFParam):  # no draw
    label = "TRANSPARENCY"
    description = "Transparency"
    fds_label = "TRANSPARENCY"
    fds_default = 1.0
    bpy_type = Material
    bpy_prop = None  # Do not register
    bpy_idname = "diffuse_color"

    def get_value(self, context):
        c = getattr(self.element, self.bpy_idname)
        return c[3]

    def set_value(self, context, value):
        c = getattr(self.element, self.bpy_idname)
        c[3] = value

    def draw(self, context, layout):  # see MP_RGB
        pass


# class MP_MATL_ID(BFParam):
#     label = "MATL_ID"
#     description = "Reference to a MATL (Material) line for self properties"
#     fds_label = "MATL_ID"
#     bpy_type = Material
#     bpy_prop = StringProperty
#     bpy_idname = "bf_matl_id"

#     def draw_operators(self, context, layout):
#         layout.operator("material.bf_choose_matl_id", icon="VIEWZOOM", text="")

#     def set_value(self, context, value):
#         if isinstance(value, str):
#             return super().set_value(context, value)
#         else:
#             raise BFNotImported(self, "Material list not handled")

# When this Material is default, export DEFAULT=T
class MP_DEFAULT(BFParam):  # no label
    label = "DEFAULT"
    description = "Set default SURF"
    fds_label = "DEFAULT"
    fds_default = False
    bpy_type = Material

    def get_value(self, context):
        return context.scene.bf_default_surf == self.element

    def set_value(self, context, value=None):
        context.scene.bf_default_surf = self.element


class MP_other(BFParamOther):
    bpy_type = Material
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items
