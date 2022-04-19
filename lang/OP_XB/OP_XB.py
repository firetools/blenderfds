import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty, FloatProperty
from ...config import LENGTH_PRECISION
from ...types import BFParam, FDSParam, FDSList, FDSMulti
from ... import utils
from ..ON_MULT import OP_other_MULT_ID, multiply_xbs
from .ob_to_xbs import ob_to_xbs
from .xbs_to_ob import xbs_to_ob

log = logging.getLogger(__name__)


def update_bf_xb(ob, context):
    utils.geometry.rm_geometric_cache(ob=ob)
    if ob.bf_has_tmp:
        utils.geometry.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES") and ob.bf_xb_export:
        if ob.bf_xyz == "VERTICES":
            ob.bf_xyz_export = False
        if ob.bf_pb == "PLANES":
            ob.bf_pb_export = False
        return


class OP_XB_custom_voxel(BFParam):
    label = "Use Custom Voxel/Pixel"
    description = "Use custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_custom_voxel"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}

    def draw(self, context, layout):
        pass

    def get_exported(self, context):
        return False


class OP_XB_voxel_size(BFParam):
    label = "Custom Voxel/Pixel Size"
    description = "Custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {
        "step": 1.0,
        "precision": LENGTH_PRECISION,
        "min": 0.001,
        "max": 20.0,
        "unit": "LENGTH",
        "update": update_bf_xb,
    }
    bpy_export = "bf_xb_custom_voxel"

    def draw(self, context, layout):
        ob = self.element
        if ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS"):
            super().draw(context, layout)

    def get_exported(self, context):
        return False


class OP_XB_center_voxels(BFParam):
    label = "Center Voxels/Pixels"
    description = "Center voxels/pixels to Object bounding box"
    bpy_type = Object
    bpy_idname = "bf_xb_center_voxels"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}

    def draw(self, context, layout):
        ob = self.element
        if ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS"):
            super().draw(context, layout)

    def get_exported(self, context):
        return False


class OP_XB_export(BFParam):
    label = "Export XB"
    description = "Set if XB shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xb_export"
    bpy_prop = BoolProperty
    bpy_default = True
    bpy_other = {"update": update_bf_xb}


class OP_XB(BFParam):
    label = "XB"
    description = "Export as volumes/faces"
    fds_label = "XB"
    bpy_type = Object
    bpy_idname = "bf_xb"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_xb,
        "items": (
            ("BBOX", "Bounding Box", "Export object bounding box", 100),
            ("VOXELS", "Voxels", "Export voxels from voxelized solid Object", 200),
            ("FACES", "Faces", "Export faces, one for each face", 300),
            ("PIXELS", "Pixels", "Export pixels from pixelized flat Object", 400),
            ("EDGES", "Edges", "Export segments, one for each edge", 500),
        ),
    }
    bpy_export = "bf_xb_export"

    def to_fds_list(self, context) -> FDSList:
        # Get geometry
        ob, hids, xbs, msgs, lp = self.element, tuple(), tuple(), tuple(), LENGTH_PRECISION
        if ob.bf_xb_export:
            hids, xbs, msgs = ob_to_xbs(context=context, ob=ob, bf_xb=ob.bf_xb)
        # Single
        match len(xbs):
            case 0:
                return FDSList()
            case 1:
                return FDSParam(fds_label="XB", value=xbs[0], precision=lp)
        # Multi
        return FDSMulti(
            iterable=(
                (FDSParam(fds_label="ID", value=hid) for hid in hids),
                (FDSParam(fds_label="XB", value=xb, precision=lp) for xb in xbs),
            ),
            msgs=msgs,
        )

    def from_fds(self, context, value):
        bf_xb = xbs_to_ob(
            context=context,
            ob=self.element,
            xbs=(value,),
            set_origin=True,
            add=True,
        )
        self.element.bf_xb = bf_xb
        self.element.bf_xb_export = True


class OP_XB_BBOX(OP_XB):
    label = "XB"
    description = "Export as object bounding box (BBOX)"
    fds_label = "XB"
    bpy_type = Object
    bpy_idname = None
    bpy_export = None

    def _get_geometry(self, context):
        ob, = self.element
        hids, xbs, msgs = ob_to_xbs(context=context, ob=ob, bf_xb=ob.bf_xb)
        return ob, hids, xbs, msgs

    def from_fds(self, context, value):
        xbs_to_ob(
            context=context,
            ob=self.element,
            xbs=(value,),
            set_origin=True,
            add=True,
        )

    def draw(self, context, layout):  # draw label only
        row = layout.split(factor=0.4)
        row.alignment = "RIGHT"
        row.label(text=f"XB")
        row.alignment = "EXPAND"
        row.label(text=f"Bounding Box")
