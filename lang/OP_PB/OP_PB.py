import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty
from ...types import BFParam, FDSParam, FDSMany, FDSMulti
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

    axis = None  # axis for importing

    def _get_geometry(self, context):
        ob, pbs, msgs = self.element, list(), list()
        if ob.bf_pb_export:
            # Compute
            # pbs is: (0, 3.5), (0, 4.), (2, .5) ...
            # with 0, 1, 2 perpendicular axis
            pbs, msgs = ob_to_pbs(context, ob, bf_pb=ob.bf_pb)
        return ob, pbs, msgs

    def to_fds_param(self, context):
        ob, pbs, msgs = self._get_geometry(context)
        # Prepare labels
        labels = tuple(
            f"PB{('X','Y','Z')[int(axis)]}" for axis, _ in pbs
        )  # int to protect from float sent by cache
        # Single param
        if len(pbs) == 1:
            return FDSParam(fds_label=labels[0], value=pbs[0][1], precision=6)
        # Multi param, prepare new ID
        n = ob.name
        suffix = self.element.bf_id_suffix
        if suffix == "IDI":
            ids = (f"{n}_{i}" for i, _ in enumerate(pbs))
        else:
            ids = (
                (f"{n}_x{pb:+.3f}", f"{n}_y{pb:+.3f}", f"{n}_z{pb:+.3f}")[axis]
                for axis, pb in pbs
            )
        return FDSMulti(
            (
                FDSMany(
                    (
                        FDSParam(fds_label="ID", value=hid),
                        FDSParam(fds_label=label, value=pb, precision=6),
                    )
                )
                for hid, label, (_, pb) in zip(ids, labels, pbs)
            ),
            msgs=msgs,
        )

    def show_fds_geometry(self, context, ob_tmp):
        ob, pbs, _ = self._get_geometry(context)
        pbs_to_ob(context=context, ob=ob_tmp, pbs=pbs)
        ob_tmp.active_material = ob.active_material

    def from_fds(self, context, value):
        bf_pb = pbs_to_ob(context=context, ob=self.element, pbs=((self.axis, value),))
        self.element.bf_pb = bf_pb
        self.element.bf_pb_export = True


class OP_PBX(OP_PB):
    fds_label = "PBX"
    bpy_prop = None  # already defined
    axis = 0  # axis for importing

    def draw(self, context, layout):
        return

    def to_fds_param(self, context):
        pass


class OP_PBY(OP_PBX):
    fds_label = "PBY"
    axis = 1  # axis for importing


class OP_PBZ(OP_PBX):
    fds_label = "PBZ"
    axis = 2  # axis for importing
