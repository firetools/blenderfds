# SPDX-License-Identifier: GPL-3.0-or-later

import logging
import bpy
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty
from .bf_material import MP_RGB, MP_COLOR, MP_TRANSPARENCY
from ..types import BFParam, BFParamOther, BFParamFYI, BFNamelist, FDSList
from .. import utils
from ..bl.ui_lists import WM_PG_bf_other, WM_UL_bf_other_items

log = logging.getLogger(__name__)


class BFObject:
    """!
    Extension of Blender Object.
    """

    @property
    def bf_namelist(self):
        """!
        Related bf_namelist, instance of BFNamelist.
        """
        return BFNamelist.get_subclass(cls_name=self.bf_namelist_cls)(element=self)

    # TODO is there a faster method?
    def get_layer_collection(self, context, _layer_collection=None):
        """!
        Return related layer_collection in current context.
        @param context: the Blender context.
        @param _layer_collection: internal use for recursivity.
        @return layer_collection related to self in current context.
        """
        if not _layer_collection:
            _layer_collection = context.view_layer.layer_collection
        found = None
        if self.name in _layer_collection.collection.objects:
            return _layer_collection
        for c in _layer_collection.children:
            found = self.get_layer_collection(context, _layer_collection=c)
            if found:
                return found

    def to_fds_list(self, context) -> FDSList:
        """!
        Return the FDSList instance from self, never None.
        """
        if self.hide_render or self.bf_is_tmp or self.type != "MESH":
            return FDSList()
        if self.mode == "EDIT":  # only in interactive
            bpy.ops.object.mode_set(mode="OBJECT")
        return self.bf_namelist.to_fds_list(context)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Object.bf_namelist = cls.bf_namelist
        Object.to_fds_list = cls.to_fds_list
        Object.get_layer_collection = cls.get_layer_collection

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Object.get_layer_collection
        del Object.to_fds_list
        del Object.bf_namelist


def update_OP_namelist_cls(ob, context):
    # Reset selections
    ob.bf_mult_export = False
    # Rm cache and tmp
    utils.geometry.rm_geometric_cache(ob=ob)
    if ob.bf_has_tmp:
        utils.geometry.rm_tmp_objects()
    # Reset new appearance
    ob.bf_namelist.set_appearance(context)


# Before updating the items, the lang classes should be imported
# Only after updating the items, the OP_namelist_cls can be registered
def update_OP_namelist_cls_items():
    items = [
        (cls.__name__, cls.label, cls.description, cls.enum_id)
        for cls in BFNamelist.subclasses
        if cls.bpy_type == Object and cls.enum_id
    ]
    items.sort(key=lambda k: k[1])
    OP_namelist_cls.bpy_other["items"] = items
    # log.debug(f"Updated OP_namelist_cls items (before registration): {items}")


# This are service parameters for managing tmp geometry
class OP_is_tmp(BFParam):
    label = "Temporary Object"
    description = "Set if it is a temporary Object"
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_is_tmp"
    bpy_default = False


class OP_has_tmp(BFParam):
    label = "Has Temporary Object"
    description = "Set if it has related temporary Object instances"
    bpy_type = Object
    bpy_prop = BoolProperty
    bpy_idname = "bf_has_tmp"
    bpy_default = False


class OP_namelist_cls(BFParam):
    label = "Namelist"
    description = "Identification of FDS namelist"
    bpy_type = Object
    bpy_idname = "bf_namelist_cls"
    bpy_prop = EnumProperty
    bpy_default = "ON_OBST"
    bpy_other = {
        "items": (("ON_OBST", "OBST", "Obstruction", 1000),),
        "update": update_OP_namelist_cls,
    }

    def draw_operators(self, context, layout):
        layout.operator("object.bf_props_to_sel_obs", icon="COPYDOWN", text="")


class OP_ID(BFParam):
    label = "ID"
    description = "Object identification name"
    fds_label = "ID"
    bpy_type = Object
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"

    def copy_to(self, context, dest_element):
        pass

    def set_value(self, context, value=None):
        # Change the name of tmp objects
        if self.element.bf_is_tmp:
            value += "_tmp"
        super().set_value(context, value)


class OP_ID_suffix(BFParam):
    label = "IDs Suffix"
    description = "Suffix for IDs in case of multiple geometry (eg. voxels, faces, ...)"
    bpy_type = Object
    bpy_idname = "bf_id_suffix"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (
            ("IDI", "Index", "Append an index", 100),
            ("IDX", "x", "Append the x coordinate", 200),
            ("IDY", "y", "Append the y coordinate", 300),
            ("IDZ", "z", "Append the z coordinate", 400),
            ("IDXY", "xy", "Append the x,y coordinates", 500),
            ("IDXZ", "xz", "Append the x,z coordinates", 600),
            ("IDYZ", "yz", "Append the y,z coordinates", 700),
            ("IDXYZ", "xyz", "Append the x,y,z coordinates", 800),
        )
    }

    def get_active(self, context):
        ob = self.element
        return (
            (ob.bf_xb_export and ob.bf_xb != "BBOX")
            or (ob.bf_xyz_export and ob.bf_xyz == "VERTICES")
            or (ob.bf_pb_export and ob.bf_pb == "PLANES")
        )


class OP_FYI(BFParamFYI):
    bpy_type = Object
    bpy_idname = "bf_fyi"


class OP_other(BFParamOther):
    bpy_type = Object
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


# Taken from Material
# Used by ON_MESH, ON_OBST, ON_HOLE


class OP_RGB_override(MP_RGB):
    label = "Override RGB"
    description = "Override color and transparency from boundary conditions"
    bpy_type = Object
    bpy_idname = "color"
    bpy_export = "bf_rgb_export"
    bpy_export_default = False


class OP_COLOR_override(MP_COLOR):  # only import
    bpy_type = Object
    bpy_idname = "color"


class OP_TRANSPARENCY_override(MP_TRANSPARENCY):  # no draw
    bpy_type = Object
    bpy_idname = "color"
    bpy_export = "bf_rgb_export"  # already existing
