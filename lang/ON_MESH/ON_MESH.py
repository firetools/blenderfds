"""!
MESH namelist definition
"""

import logging
from bpy.types import Object
from bpy.props import IntVectorProperty, IntProperty
from ...config import LENGTH_PRECISION
from ...types import BFParam, BFNamelistOb, FDSParam, FDSMulti, FDSList, BFException
from ... import utils
from ..bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_other, OP_RGB, OP_COLOR
from ..OP_XB import OP_XB_BBOX, xbs_to_ob
from ..ON_MULT import OP_other_MULT_ID, multiply_xbs
from .calc_meshes import get_mesh_geometry, get_mesh_mpis

log = logging.getLogger(__name__)


def update_bf_mesh_nsplits(ob, context):
    # Remove cache and tmp objects
    ob["ob_to_xbs_cache"] = None
    utils.geometry.rm_tmp_objects()


class OP_MESH_MPI_PROCESS_qty(BFParam):  # to_fds_list() in XB_BBOX
    label = "MPI_PROCESS Qty"
    description = "Number MPI processes assigned to these MESH instances"
    bpy_type = Object
    bpy_idname = "bf_mesh_mpi_process_qty"
    bpy_prop = IntProperty
    bpy_default = 1
    bpy_other = {"min": 1}
    bpy_export = "bf_mesh_mpi_process_qty_export"
    bpy_export_default = False


class OP_MESH_IJK(BFParam):
    label = "IJK"
    description = "Cell numbers along axis"
    fds_label = "IJK"
    bpy_type = Object
    bpy_idname = "bf_mesh_ijk"
    bpy_prop = IntVectorProperty
    bpy_default = (10, 10, 10)
    bpy_other = {"size": 3, "min": 1}

    def draw(self, context, layout):
        ob = context.object
        _, _, _, msgs = get_mesh_geometry(context, ob)
        for msg in msgs:
            layout.label(text=msg)
        # Operators
        row = layout.row()
        row.operator("object.bf_set_mesh_cell_size")
        row.operator("object.bf_align_selected_meshes")
        # Integrate IJK and Split IJK panel
        # super().draw(context, layout)
        col = layout.row(align=True)
        row = col.row(align=True)
        if ob.bf_mesh_nsplits_export:
            row.prop(ob, "bf_mesh_ijk", text=self.label + ", Splits")
            row.prop(ob, "bf_mesh_nsplits", text="")
        else:
            row.prop(ob, "bf_mesh_ijk", text=self.label)
        layout.prop(ob, "bf_mesh_nsplits_export", text="Split IJK")


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

    def draw(self, context, layout):
        # Split IJK panel is integrated with IJK
        pass


class OP_MESH_XB_BBOX(OP_XB_BBOX):
    # This class implements OP_XB_BBOX
    # adding MESH split, IJK calculations, and MPI processes

    def to_fds_list(self, context) -> FDSList:
        ob = self.element
        hids, ijks, xbs, msgs = get_mesh_geometry(context, ob)
        mpis = get_mesh_mpis(context, ob, xbs)
        fds_multi = FDSMulti(
            iterable=(
                (FDSParam(fds_label="ID", value=hid) for hid in hids),
                (FDSParam(fds_label="IJK", value=ijk) for ijk in ijks),
                (
                    FDSParam(fds_label="XB", value=xb, precision=LENGTH_PRECISION)
                    for xb in xbs
                ),
            ),
            msgs=msgs,
        )
        if mpis:
            fds_multi.append(
                (FDSParam(fds_label="MPI_PROCESS", value=mpi) for mpi in mpis),
            )
        return fds_multi

    def show_fds_geometry(self, context, ob_tmp):
        ob = self.element
        _, _, xbs, _ = get_mesh_geometry(context, ob)
        xbs, _ = multiply_xbs(xbs, hids=None, ob=ob)
        xbs_to_ob(context=context, ob=ob_tmp, xbs=xbs, bf_xb="BBOX", add=True)


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
        OP_MESH_MPI_PROCESS_qty,
        OP_RGB,
        OP_COLOR,
        OP_other,
    )
    bf_other = {"appearance": "BBOX"}
