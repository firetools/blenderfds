# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty, FloatProperty
from ...config import LP
from ...types import BFParam, FDSParam, FDSList, FDSMulti
from ... import utils
from .ob_to_xbs import ob_to_xbs
from .xbs_to_ob import xbs_to_ob

log = logging.getLogger(__name__)


def update_bf_xb(ob, context):
    # Rm tmp geometry and cache
    utils.geometry.rm_geometric_cache(ob=ob)
    if ob.bf_has_tmp:
        utils.geometry.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES") and ob.bf_xb_export:
        if ob.bf_xyz == "VERTICES":
            ob["bf_xyz_export"] = False  # prevent multiple update
        if ob.bf_pb == "PLANES":
            ob["bf_pb_export"] = False
        return


class OP_XB_voxel_size(BFParam):
    label = "Voxel/Pixel Size"
    description = "Voxel/pixel size for the current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {
        "step": 1.0,
        "precision": LP,
        "min": 0.001,
        "max": 20.0,
        "unit": "LENGTH",
        "update": update_bf_xb,
    }
    bpy_export = "bf_xb_custom_voxel"
    bpy_export_default = False

    def get_active(self, context):
        ob = self.element
        return ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS")


class OP_XB_center_voxels(BFParam):
    label = "Center Voxels/Pixels"
    description = "Center voxels/pixels to Object bounding box"
    bpy_type = Object
    bpy_idname = "bf_xb_center_voxels"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xb}

    def get_active(self, context):
        ob = self.element
        return ob.bf_xb_export and ob.bf_xb in ("VOXELS", "PIXELS")


class OP_XB(BFParam):
    label = "XB"
    description = "Export as volumes/faces/edges"
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
    bpy_export_default = False

    def to_fds_list(self, context) -> FDSList:
        if not self.get_exported(context):
            return FDSList()
        ob = self.element
        hids, xbs, msgs = ob_to_xbs(context=context, ob=ob, bf_xb=ob.bf_xb)
        match len(xbs):
            case 0:
                return FDSList()
            case 1:
                return FDSParam(fds_label="XB", value=xbs[0], precision=LP)
            case _:
                iterable=(
                    (FDSParam(fds_label="ID", value=hid) for hid in hids),
                    (FDSParam(fds_label="XB", value=xb, precision=LP) for xb in xbs),
                )
                return FDSMulti(iterable=iterable, msgs=msgs)

    def set_value(self, context, value=None):
        ob = self.element
        bf_xb = xbs_to_ob(
            context=context,
            ob=ob,
            xbs=(value,),
            set_origin=True,
            add=True
        )
        ob.bf_xb = bf_xb
        ob.bf_xb_export = True


class OP_XB_BBOX(OP_XB):  # should always work, even without bf_xb_export and bf_xb
    label = "XB"
    description = "Export as object bounding box (BBOX)"
    fds_label = "XB"
    bpy_type = Object
    bpy_idname = None
    bpy_export = None

    def to_fds_list(self, context) -> FDSList:
        ob = self.element
        xb = utils.geometry.get_bbox_xb(context=context, ob=ob, world=True)
        return FDSParam(fds_label="XB", value=xb, precision=LP)

    def draw(self, context, layout):  # draw label only
        row = layout.split(factor=0.4)
        row.alignment = "RIGHT"
        row.label(text=f"XB")
        row.alignment = "EXPAND"
        row.label(text=f"Bounding Box")
