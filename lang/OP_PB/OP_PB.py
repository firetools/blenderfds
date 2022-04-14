import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty
from ...config import LENGTH_PRECISION
from ...types import BFParam, FDSParam, FDSList, FDSMulti
from ... import utils
from .ob_to_pbs import ob_to_pbs
from .pbs_to_ob import pbs_to_ob

log = logging.getLogger(__name__)


def update_bf_pb(ob, context):
    # Remove tmp objects
    utils.geometry.rm_tmp_objects()
    # Prevent double multiparam
    if ob.bf_pb == "PLANES" and ob.bf_pb_export:
        if ob.bf_xb in ("VOXELS", "FACES", "PIXELS", "EDGES"):
            ob.bf_xb_export = False
        if ob.bf_xyz == "VERTICES":
            ob.bf_xyz_export = False
        return


class OP_PB_export(BFParam):
    label = "Export PBX, PBY, PBZ"
    description = "Set if PBX, PBY, PBZ shall be exported to FDS"
    bpy_type = Object
    bpy_idname = "bf_pb_export"
    bpy_prop = BoolProperty
    bpy_default = False
    bpy_other = {"update": update_bf_pb}


class OP_PB(BFParam):
    label = "PBX, PBY, PBZ"
    description = "Export as planes"
    fds_label = None  # used for importing
    bpy_type = Object
    bpy_idname = "bf_pb"
    bpy_prop = EnumProperty
    bpy_other = {
        "update": update_bf_pb,
        "items": (
            ("PLANES", "Planes", "Planes, one for each face of this object", 100),
        ),
    }
    bpy_export = "bf_pb_export"

    def _get_geometry(self, context):
        ob, hids, pbs, msgs = self.element, tuple(), tuple(), tuple()
        if self.get_exported(context):  # for OP_PBX, OP_PBY, OP_PBZ
            hids, pbs, msgs = ob_to_pbs(context, ob, bf_pb=ob.bf_pb)
        return ob, hids, pbs, msgs

    def to_fds_list(self, context) -> FDSList:
        _, hids, pbs, msgs = self._get_geometry(context)
        # Single
        match len(pbs):
            case 0:
                return FDSList()
            case 1:
                return FDSParam(fds_label=pbs[0][0], value=pbs[0][1], precision=LENGTH_PRECISION)
        # Multi
        return FDSMulti(
            iterable=(
                (FDSParam(fds_label="ID", value=hid) for hid in hids),
                (
                    FDSParam(fds_label=axis, value=pb, precision=LENGTH_PRECISION)
                    for axis, pb in pbs
                ),
            ),
            msgs=msgs,
        )

    def show_fds_geometry(self, context, ob_tmp):
        ob, pbs, _ = self._get_geometry(context)
        pbs_to_ob(context=context, ob=ob_tmp, pbs=pbs, add=True)
        ob_tmp.active_material = ob.active_material

    def from_fds(self, context, value):
        bf_pb = pbs_to_ob(
            context=context,
            ob=self.element,
            pbs=((self.fds_label, value),),
            set_origin=True,
            add=True,
        )
        self.element.bf_pb = bf_pb
        self.element.bf_pb_export = True


class OP_PBX(OP_PB):  # only for importing
    fds_label = "PBX"
    bpy_prop = None  # already defined

    def draw(self, context, layout):
        pass

    def get_exported(self, context):
        return False


class OP_PBY(OP_PBX):  # only for importing
    fds_label = "PBY"


class OP_PBZ(OP_PBX):  # only for importing
    fds_label = "PBZ"
