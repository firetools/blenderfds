import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty, FloatProperty
from ...types import BFParam, BFParamXYZ, FDSParam
from ... import geometry

log = logging.getLogger(__name__)


def update_bf_xyz(ob, context):
    # Remove tmp objects
    geometry.utils.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_xyz == "VERTICES" and ob.bf_xyz_export:
        if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES"):
            ob.bf_xb_export = False
        if ob.bf_pb == "PLANES":
            ob.bf_pb_export = False
        return


class OP_XYZ_export(BFParam):
    label = "Export XYZ"
    description = "Set if XYZ shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_xyz_export"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_xyz}


class OP_XYZ(BFParamXYZ):
    label = "XYZ"
    description = "Export as points"
    fds_label = "XYZ"
    bpy_type = Object
    bpy_idname = "bf_xyz"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_xyz,
        "items": (
            ("CENTER", "Center", "Point, center point of this object", 100),
            ("VERTICES", "Vertices", "Points, one for each vertex of this object", 200),
        ),
    }
    bpy_export = "bf_xyz_export"

    def to_fds_param(self, context):
        ob = self.element
        if not ob.bf_xyz_export:
            return
        # Compute
        xyzs, msg = geometry.to_fds.ob_to_xyzs(context, ob, ob.bf_xyz)
        # Single param
        if len(xyzs) == 1:
            return FDSParam(fds_label="XYZ", value=xyzs[0])
        # Multi param, prepare new ID
        n = ob.name
        suffix = self.element.bf_id_suffix
        if suffix == "IDI":
            ids = (f"{n}_{i}" for i, _ in enumerate(xyzs))
        elif suffix == "IDX":
            ids = (f"{n}_x{xyz[0]:+.3f}" for xyz in xyzs)
        elif suffix == "IDY":
            ids = (f"{n}_y{xyz[1]:+.3f}" for xyz in xyzs)
        elif suffix == "IDZ":
            ids = (f"{n}_z{xyz[2]:+.3f}" for xyz in xyzs)
        elif suffix == "IDXY":
            ids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}" for xyz in xyzs)
        elif suffix == "IDXZ":
            ids = (f"{n}_x{xyz[0]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        elif suffix == "IDYZ":
            ids = (f"{n}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        elif suffix == "IDXYZ":
            ids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
        else:
            raise AssertionError(f"Unknown suffix <{suffix}>")
        result = tuple(
            (
                FDSParam(fds_label="ID", value=hid),
                FDSParam(fds_label="XYZ", value=xyz, precision=6),
            )
            for hid, xyz in zip(ids, xyzs)
        )  # multi
        # Send message
        result[0][0].msg = msg
        return result

    def from_fds(self, context, value):
        bf_xyz = geometry.from_fds.xyzs_to_ob(
            context=context,
            ob=self.element,
            xyzs=(value,),
        )
        self.element.bf_xyz = bf_xyz
        self.element.bf_xyz_export = True


class OP_XYZ_center(OP_XYZ):
    description = "Export as points (center)"
    bpy_prop = None  # do not redefine
    bf_xyz_idxs = (0,)  # CENTER, VERTICES
