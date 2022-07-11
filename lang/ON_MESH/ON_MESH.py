# SPDX-License-Identifier: GPL-3.0-or-later

"""!
MESH namelist definition
"""

import logging
from bpy.types import Object
from bpy.props import IntVectorProperty
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

    def draw(self, context, layout):
        ob = self.element
        try:
            _, _, _, nmesh, nsplit, nmult, ncell_tot, ncell, cs, aspect, has_good_ijk = get_mesh_geometry(context=context, ob=ob)
        except BFException:
            col = layout.column(align=True)
            col.label(text="No MESH info while in Edit Mode.")
        else:
            col = layout.column(align=True)
            col.label(text=f"MESH Qty: {nmesh} | Splits: {nsplit} | Multiples: {nmult}")
            col.label(text=f"Cell Qty: {ncell_tot} (~{ncell} each)")
            col.label(text=f"Size: {cs[0]:.{LP}f}·{cs[1]:.{LP}f}·{cs[2]:.{LP}f}m | Aspect: {aspect:.1f} | Poisson: {has_good_ijk}")

        col = layout.column()
        row = col.row(align=True)
        row.prop(ob, "bf_mesh_ijk", text="IJK, Splits")    
        sub = row.row(align=True)
        sub.active = ob.bf_mesh_nsplits_export
        sub.prop(ob, "bf_mesh_nsplits", text="") 

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
        layout.prop(self.element, "bf_mesh_nsplits_export", text="Split")

class OP_MESH_XB_BBOX(OP_XB_BBOX):
    # This class implements OP_XB_BBOX
    # adding MESH split, IJK calculations, and MPI processes

    def to_fds_list(self, context) -> FDSList:
        ob = self.element
        hids, ijks, xbs, _, _, _, _, ncell, cs, aspect, has_good_ijk = get_mesh_geometry(context=context, ob=ob)
        msg = f" | Size: {cs[0]:.{LP}f}·{cs[1]:.{LP}f}·{cs[2]:.{LP}f}m | Aspect: {aspect:.1f} | Poisson: {has_good_ijk}"
        match len(xbs):
            case 0:
                return FDSList()  # FIXME raise exception?
            case 1:
                msg = f"Cell Qty: {ncell}{msg}"
                return FDSParam(fds_label="XB", value=xbs[0], precision=LP, msgs=(msg,))
            case _:
                iterable = (
                    (FDSParam(fds_label="ID", value=hid) for hid in hids),
                    (FDSParam(fds_label="IJK", value=ijk, msgs=(f"Cell Qty: {ijk[0]*ijk[1]*ijk[2]}{msg}",)) for ijk in ijks),
                    (FDSParam(fds_label="XB", value=xb, precision=LP) for xb in xbs),
                )
                return FDSMulti(iterable=iterable)

class OP_MESH_MPI_PROCESS(BFParam):  # only import
    label = "MPI_PROCESS"
    description = "MPI process number"
    fds_label = "MPI_PROCESS"  # only import
    bpy_type = Object

    def draw(self, context, layout):
        pass 

    def set_value(self, context, value=None):
        # Set the number of MPI processes from the FDS file
        sc = context.scene
        sc.bf_config_mpi_processes_export = True
        if value + 1 > sc.bf_config_mpi_processes:
            sc.bf_config_mpi_processes = value + 1

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
        OP_MESH_MPI_PROCESS,
        OP_MESH_IJK,
        OP_MESH_nsplits,
        OP_MESH_XB_BBOX,
        OP_other_MULT_ID,
        OP_RGB_override,
        OP_COLOR_override,
        OP_other,
    )
    bf_other = {"appearance": "BBOX"}
    bf_import_order = 30

    def draw_operators(self, context, layout):
        col = layout.column(align=True)
        col.operator("object.bf_set_suggested_mesh_cell_size")
        col.operator("object.bf_set_mesh_cell_size")
        col.operator("object.bf_align_to_mesh")
