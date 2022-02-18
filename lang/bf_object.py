import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty
from ..types import (
    BFParam,
    BFParamOther,
    BFParamFYI,
    BFNamelist,
)
from .. import utils
from ..bl.ui_lists import (
    WM_PG_bf_other,
    WM_UL_bf_other_items,
)

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

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        if self.hide_render or self.bf_is_tmp or not self.type == "MESH":
            return
        return self.bf_namelist.to_fds(context)

    def from_fds(self, context, fds_namelist):
        """!
        Set self.bf_namelist from FDSNamelist, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: FDSNamelist.
        """
        # Get subclass from offered fds_label, set bf_namelist_cls
        bf_namelist = BFNamelist.get_subclass(fds_label=fds_namelist.fds_label)
        self.bf_namelist_cls = bf_namelist.__name__
        # Prevent default geometry (eg. XB=BBOX)
        self.bf_xb_export, self.bf_xyz_export, self.bf_pb_export = (False, False, False)
        # Import
        self.bf_namelist.from_fds(context, fds_namelist=fds_namelist)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Object.bf_namelist = cls.bf_namelist
        Object.to_fds = cls.to_fds
        Object.from_fds = cls.from_fds
        Object.bf_is_tmp = BoolProperty(
            name="Is Tmp", description="Set if this Object is tmp", default=False
        )
        Object.bf_has_tmp = BoolProperty(
            name="Has Tmp",
            description="Set if this Object has tmp companions",
            default=False,
        )

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Object.bf_has_tmp
        del Object.bf_is_tmp
        del Object.from_fds
        del Object.to_fds
        del Object.bf_namelist


def update_OP_namelist_cls(ob, context):
    if not ob.bf_is_tmp:
        utils.geometry.rm_geometric_cache(ob=ob)
        utils.geometry.rm_tmp_objects()
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


class OP_ID(BFParam):
    label = "ID"
    description = "Object identification name"
    fds_label = "ID"
    bpy_type = Object
    bpy_prop = None  # to avoid creation
    bpy_idname = "name"

    def copy_to(self, context, dest_element):
        pass


class OP_ID_suffix(BFParam):
    label = "IDs Suffix"
    description = "Append suffix to multiple IDs"
    bpy_type = Object
    bpy_idname = "bf_id_suffix"
    bpy_prop = EnumProperty
    bpy_other = {
        "items": (
            ("IDI", "Index", "Append index number to multiple IDs", 100),
            ("IDX", "x", "Append x coordinate to multiple IDs", 200),
            ("IDY", "y", "Append y coordinate to multiple IDs", 300),
            ("IDZ", "z", "Append z coordinate to multiple IDs", 400),
            ("IDXY", "xy", "Append x,y coordinates to multiple IDs", 500),
            ("IDXZ", "xz", "Append x,z coordinates to multiple IDs", 600),
            ("IDYZ", "yz", "Append y,z coordinates to multiple IDs", 700),
            ("IDXYZ", "xyz", "Append x,y,z coordinates to multiple IDs", 800),
        )
    }

    def draw(self, context, layout):
        ob = self.element
        if (
            (ob.bf_xb_export and ob.bf_xb != "BBOX")
            or (ob.bf_xyz_export and ob.bf_xyz == "VERTICES")
            or ob.bf_pb_export
        ):
            layout.prop(ob, "bf_id_suffix")
        return layout


class OP_FYI(BFParamFYI):
    bpy_type = Object
    bpy_idname = "bf_fyi"


class OP_other(BFParamOther):
    bpy_type = Object
    bpy_idname = "bf_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items
