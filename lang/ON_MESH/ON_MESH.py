"""!
MESH namelist definition
"""

import logging
from bpy.types import Object
from bpy.props import IntVectorProperty, IntProperty
from ...config import LP
from ...types import BFParam, BFNamelistOb, FDSParam, FDSMulti, FDSList, BFException
from ... import utils
from ..bf_object import (
    OP_namelist_cls,
    OP_ID,
    OP_FYI,
    OP_other,
    OP_RGB_override,
    OP_COLOR_override,
)
from ..OP_XB import OP_XB_BBOX
from ..ON_MULT import OP_other_MULT_ID
from .calc_meshes import get_mesh_geometry

log = logging.getLogger(__name__)


def update_bf_mesh_nsplits(ob, context):
    utils.geometry.rm_geometric_cache(ob=ob)
    if ob.bf_has_tmp:
        utils.geometry.rm_tmp_objects()

class OP_MESH_IJK(BFParam):
    label = "IJK"
    description = "Cell numbers along axis"
    fds_label = "IJK"
    bpy_type = Object
    bpy_idname = "bf_mesh_ijk"
    bpy_prop = IntVectorProperty
    bpy_default = (10, 10, 10)
    bpy_other = {"size": 3, "min": 1}


class OP_MESH_nsplits(BFParam):
    label = "Split IJK"
    description = "Evenly split IJK along each axis generating an array\nof MESH namelists, conserving the cell size"
    bpy_type = Object
    bpy_idname = "bf_mesh_nsplits"
    bpy_prop = IntVectorProperty
    bpy_default = (1, 1, 1)
    bpy_other = {"size": 3, "min": 1, "update": update_bf_mesh_nsplits}
    bpy_export = "bf_mesh_nsplits_export"
    bpy_export_default = False


class OP_MESH_XB_BBOX(OP_XB_BBOX):
    # This class implements OP_XB_BBOX
    # adding MESH split, IJK calculations, and MPI processes

    def to_fds_list(self, context) -> FDSList:
        ob = self.element
        hids, ijks, xbs, msgs = get_mesh_geometry(context=context, ob=ob)
        match len(xbs):
            case 0:
                return FDSList()
            case 1:
                return FDSParam(fds_label="XB", value=xbs[0], precision=LP, msgs=msgs)
            case _:
                iterable = (
                    (FDSParam(fds_label="ID", value=hid) for hid in hids),
                    (FDSParam(fds_label="IJK", value=ijk) for ijk in ijks),
                    (FDSParam(fds_label="XB", value=xb, precision=LP) for xb in xbs),
                )
                return FDSMulti(iterable=iterable, msgs=msgs)


class ON_MESH(BFNamelistOb):
    label = "MESH"
    description = "Domain of simulation"
    collection = "Domain"
    enum_id = 1014
    fds_label = "MESH"
    bf_params = (
        OP_namelist_cls,
        OP_ID,
        OP_FYI,
        OP_MESH_IJK,
        OP_MESH_nsplits,
        OP_MESH_XB_BBOX,
        OP_other_MULT_ID,
        OP_RGB_override,
        OP_COLOR_override,
        OP_other,
    )
    bf_other = {"appearance": "BBOX"}

    def draw_operators(self, context, layout):
        col = layout.column(align=True)
        col.operator("object.bf_set_mesh_cell_size")
        col.operator("object.bf_align_selected_meshes")
