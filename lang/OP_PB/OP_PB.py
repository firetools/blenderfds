import logging
from bpy.types import Object
from bpy.props import EnumProperty, BoolProperty
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
        if self.get_exported(context):  # for OP_PBX, OP_PBY, OP_PBZ
            # pbs is: (0, 3.5), (0, 4.), (2, .5) ...
            # with 0, 1, 2 perpendicular axis
            pbs, msgs = ob_to_pbs(context, ob, bf_pb=ob.bf_pb)
        return ob, pbs, msgs

    def to_fds_list(self, context) -> FDSList:
        ob, pbs, msgs = self._get_geometry(context)
        labels = tuple(f"PB{('X','Y','Z')[axis]}" for axis, _ in pbs)
        match len(pbs):
            case 0:
                return FDSList()
            case 1:
                return FDSParam(fds_label=labels[0], value=pbs[0][1], precision=6)
        # Multi
        n = ob.name
        match ob.bf_id_suffix:
            case "IDI":
                hids = (f"{n}_{i}" for i, _ in enumerate(pbs))
            case _:
                hids = (
                    (f"{n}_x{pb:+.3f}", f"{n}_y{pb:+.3f}", f"{n}_z{pb:+.3f}")[axis]
                    for axis, pb in pbs
                )
        iterable = FDSList(
            FDSList(
                (
                    FDSParam(fds_label="ID", value=hid),
                    FDSParam(fds_label=label, value=pb, precision=6),
                )
                for hid, label, (_, pb) in zip(hids, labels, pbs)
            )
        )
        return FDSMulti(iterable=FDSList(iterable), msgs=msgs)

    def show_fds_geometry(self, context, ob_tmp):
        ob, pbs, _ = self._get_geometry(context)
        pbs_to_ob(context=context, ob=ob_tmp, pbs=pbs, add=True)
        ob_tmp.active_material = ob.active_material

    def from_fds(self, context, value):
        bf_pb = pbs_to_ob(context=context, ob=self.element, pbs=((self.axis, value),), set_origin=True, add=True,)
        self.element.bf_pb = bf_pb
        self.element.bf_pb_export = True


class OP_PBX(OP_PB):  # only for importing
    fds_label = "PBX"
    bpy_prop = None  # already defined
    axis = 0  # axis for importing

    def draw(self, context, layout):
        pass

    def get_exported(self, context):
        return False


class OP_PBY(OP_PBX):  # only for importing
    fds_label = "PBY"
    axis = 1  # axis for importing


class OP_PBZ(OP_PBX):  # only for importing
    fds_label = "PBZ"
    axis = 2  # axis for importing
