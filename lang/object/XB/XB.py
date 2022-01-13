import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty, FloatProperty
from ....types import BFParam, BFParamXB, FDSParam
from .... import utils
from .ob_to_xbs import ob_to_xbs
from .xbs_to_ob import xbs_to_ob

log = logging.getLogger(__name__)


def update_bf_xb(ob, context):
    # Remove cache and tmp objects
    ob["ob_to_xbs_cache"] = None
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
        return

    def to_fds_param(self, context):
        pass


class OP_XB_voxel_size(BFParam):
    label = "Custom Voxel/Pixel Size"
    description = "Custom voxel/pixel size for current Object"
    bpy_type = Object
    bpy_idname = "bf_xb_voxel_size"
    bpy_prop = FloatProperty
    bpy_default = 0.1
    bpy_other = {
        "step": 1.0,
        "precision": 3,
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

    def to_fds_param(self, context):
        pass


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

    def to_fds_param(self, context):
        pass


class OP_XB_export(BFParam):
    label = "Export XB"
    description = "Set if XB shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xb_export"
    bpy_prop = BoolProperty
    bpy_default = True
    bpy_other = {"update": update_bf_xb}


class OP_XB(BFParamXB):
    label = "XB"
    description = "Export as volumes/faces"
    fds_label = "XB"
    bpy_type = Object
    bpy_idname = "bf_xb"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_xb,
        "items": (
            ("BBOX", "BBox", "Export object bounding box", 100),
            ("VOXELS", "Voxels", "Export voxels from voxelized solid Object", 200),
            ("FACES", "Faces", "Export faces, one for each face", 300),
            ("PIXELS", "Pixels", "Export pixels from pixelized flat Object", 400),
            ("EDGES", "Edges", "Export segments, one for each edge", 500),
        ),
    }
    bpy_export = "bf_xb_export"

    def to_fds_param(self, context):
        ob = self.element
        if not ob.bf_xb_export:
            return
        # Compute
        xbs, msg = ob_to_xbs(context, ob, ob.bf_xb)
        # Single param
        if len(xbs) == 1:
            return FDSParam(fds_label="XB", value=xbs[0], precision=6)
        # Multi param, prepare new ID
        n = ob.name
        suffix = self.element.bf_id_suffix
        if suffix == "IDI":
            ids = (f"{n}_{i}" for i, _ in enumerate(xbs))
        elif suffix == "IDX":
            ids = (f"{n}_x{xb[0]:+.3f}" for xb in xbs)
        elif suffix == "IDY":
            ids = (f"{n}_y{xb[2]:+.3f}" for xb in xbs)
        elif suffix == "IDZ":
            ids = (f"{n}_z{xb[4]:+.3f}" for xb in xbs)
        elif suffix == "IDXY":
            ids = (f"{n}_x{xb[0]:+.3f}_y{xb[2]:+.3f}" for xb in xbs)
        elif suffix == "IDXZ":
            ids = (f"{n}_x{xb[0]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        elif suffix == "IDYZ":
            ids = (f"{n}_y{xb[2]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        elif suffix == "IDXYZ":
            ids = (f"{n}_x{xb[0]:+.3f}_y{xb[2]:+.3f}_z{xb[4]:+.3f}" for xb in xbs)
        else:
            raise AssertionError(f"Unknown suffix <{suffix}>")
        result = tuple(
            (
                FDSParam(fds_label="ID", value=hid),
                FDSParam(fds_label="XB", value=xb, precision=6),
            )
            for hid, xb in zip(ids, xbs)
        )  # multi
        # Send message
        result[0][0].msg = msg
        return result

    def from_fds(self, context, value):
        bf_xb = xbs_to_ob(
            context=context,
            ob=self.element,
            xbs=(value,),
        )
        self.element.bf_xb = bf_xb
        self.element.bf_xb_export = True


class OP_XB_BBOX(BFParamXB):  # independent from OP_XB
    label = "XB"
    description = "Export as object bounding box (BBOX)"
    fds_label = "XB"
    bpy_type = Object

    def to_fds_param(self, context):
        ob = self.element
        xbs, _ = ob_to_xbs(context, ob, "BBOX")
        return FDSParam(fds_label="XB", value=xbs[0], precision=6)

    def from_fds(self, context, value):
        xbs_to_ob(
            context=context,
            ob=self.element,
            xbs=(value,),
        )

    def draw(self, context, layout):
        layout.label(text=f"XB as object bounding box (BBOX)")
