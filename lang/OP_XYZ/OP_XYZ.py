import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty, FloatProperty
from ...config import LENGTH_PRECISION
from ...types import BFParam, FDSParam, FDSList, FDSMulti
from ... import utils
from .ob_to_xyzs import ob_to_xyzs
from .xyzs_to_ob import xyzs_to_ob

log = logging.getLogger(__name__)


def update_bf_xyz(ob, context):
    # Remove tmp objects
    utils.geometry.rm_tmp_objects()
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


class OP_XYZ(BFParam):
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

    def _get_geometry(self, context):
        ob, xyzs, msgs = self.element, list(), list()
        if ob.bf_xyz_export:
            xyzs, msgs = ob_to_xyzs(context=context, ob=ob, bf_xyz=ob.bf_xyz)
        return ob, xyzs, msgs

    def to_fds_list(self, context) -> FDSList:
        ob, xyzs, msgs = self._get_geometry(context)
        match len(xyzs):
            case 0:
                return FDSList()
            case 1:
                return FDSParam(fds_label="XYZ", value=xyzs[0], precision=LENGTH_PRECISION)
        # Multi
        n = ob.name
        match self.element.bf_id_suffix:
            case "IDI":
                hids = (f"{n}_{i}" for i, _ in enumerate(xyzs))
            case "IDX":
                hids = (f"{n}_x{xyz[0]:+.3f}" for xyz in xyzs)
            case "IDY":
                hids = (f"{n}_y{xyz[1]:+.3f}" for xyz in xyzs)
            case "IDZ":
                hids = (f"{n}_z{xyz[2]:+.3f}" for xyz in xyzs)
            case "IDXY":
                hids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}" for xyz in xyzs)
            case "IDXZ":
                hids = (f"{n}_x{xyz[0]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
            case "IDYZ":
                hids = (f"{n}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
            case "IDXYZ":
                hids = (f"{n}_x{xyz[0]:+.3f}_y{xyz[1]:+.3f}_z{xyz[2]:+.3f}" for xyz in xyzs)
            case _:
                raise AssertionError(f"Unknown suffix <{self.element.bf_id_suffix}>")
        return FDSMulti(
            iterable=(
                FDSList(
                    iterable=(
                        FDSParam(fds_label="ID", value=hid),
                        FDSParam(fds_label="XYZ", value=xyz, precision=LENGTH_PRECISION),
                    )
                )
                for hid, xyz in zip(hids, xyzs)
            ),
            msgs=msgs,
        )

    def show_fds_geometry(self, context, ob_tmp):
        ob, xyzs, _ = self._get_geometry(context)
        xyzs_to_ob(context=context, ob=ob_tmp, xyzs=xyzs, add=True)
        ob_tmp.active_material = ob.active_material

    def from_fds(self, context, value):
        bf_xyz = xyzs_to_ob(
            context=context,
            ob=self.element,
            xyzs=(value,),
            set_origin=True,
            add=True,
        )
        self.element.bf_xyz = bf_xyz
        self.element.bf_xyz_export = True


class OP_XYZ_center(OP_XYZ):
    description = "Export as points (center)"
    bpy_prop = None  # do not redefine
    bf_xyz_idxs = (0,)  # CENTER, VERTICES
