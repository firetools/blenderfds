"""!
MESH namelist definition
"""

import logging
from bpy.types import Object
from bpy.props import IntVectorProperty
from ....types import BFParam, BFNamelistOb, FDSParam, BFException
from .... import geometry
from ..object import OP_ID, OP_FYI, OP_other
from ..XB import OP_XB_BBOX
from .split import split_mesh, get_nsplit
from .align import get_n_for_poisson

log = logging.getLogger(__name__)


class OP_MESH_IJK(BFParam):
    label = "IJK"
    description = "Cell number in x, y, and z direction"
    fds_label = "IJK"
    bpy_type = Object
    bpy_idname = "bf_mesh_ijk"
    bpy_prop = IntVectorProperty
    bpy_default = (10, 10, 10)
    bpy_other = {"size": 3, "min": 1}

    def draw(self, context, layout):
        ob = context.object
        for msg in get_msgs(context, ob):
            layout.label(text=msg)
        super().draw(context, layout)


class OP_MESH_nsplits(BFParam):
    label = "Splits"
    description = "Split this MESH along each axis in an array of MESHes of similar cell number, conserving cell size"
    bpy_type = Object
    bpy_idname = "bf_mesh_nsplits"
    bpy_prop = IntVectorProperty
    bpy_default = (1, 1, 1)
    bpy_other = {"size": 3, "min": 1}
    bpy_export = "bf_mesh_nsplits_export"
    bpy_export_default = False

    def check(self, context):
        ob = self.element
        requested_nsplit = self.value[0] * self.value[1] * self.value[2]
        nsplit, _ = get_nsplit(ob)
        if ob.bf_mesh_nsplits_export and requested_nsplit != nsplit:
            raise BFException(self, "Too many splits requested for current IJK")


class OP_MESH_XB(OP_XB_BBOX):
    def to_fds_param(self, context):  # FIXME
        ob = self.element
        fds_param = super().to_fds_param(context)  # use father
        # Split
        ids, ijks, xbs = split_mesh(
            hid=ob.name,
            ijk=ob.bf_mesh_ijk,
            nsplits=ob.bf_mesh_nsplits,
            xb=fds_param.value,
        )
        result = tuple(
            (
                FDSParam(fds_label="ID", value=hid),
                FDSParam(fds_label="IJK", value=ijk),
                FDSParam(fds_label="XB", value=xb, precision=6),
            )
            for hid, xb, ijk in zip(ids, xbs, ijks)
        )  # multi
        # Send message
        result[0][0].msgs = get_msgs(context, ob)
        return result


class ON_MESH(BFNamelistOb):
    label = "MESH"
    description = "Domain of simulation"
    enum_id = 1014
    fds_label = "MESH"
    bf_params = OP_ID, OP_FYI, OP_MESH_IJK, OP_MESH_nsplits, OP_MESH_XB, OP_other
    bf_other = {"appearance": "WIRE"}

    def draw_operators(self, context, layout):
        col = layout.column()
        col.operator("object.bf_set_mesh_cell_size")
        col.operator("object.bf_align_selected_meshes")


def get_cell_sizes(context, ob):
    """!Get cell sizes by axis."""
    xb = geometry.utils.get_bbox_xb(context=context, ob=ob, world=True)
    ijk = ob.bf_mesh_ijk
    return (
        (xb[1] - xb[0]) / ijk[0],
        (xb[3] - xb[2]) / ijk[1],
        (xb[5] - xb[4]) / ijk[2],
    )


def get_cell_aspect(cell_sizes):
    """!Get max cell aspect ratio."""
    cs = sorted(cell_sizes)
    try:
        return max(
            cs[2] / cs[0],
            cs[2] / cs[1],
            cs[1] / cs[0],
        )
    except ZeroDivisionError:
        return 999.0


def get_poisson_ijk(ijk):
    """!Get an IJK respecting the Poisson constraint, close to the current one."""
    return ijk[0], get_n_for_poisson(ijk[1]), get_n_for_poisson(ijk[2])


def get_ijk_from_desired_cs(context, ob, desired_cs, poisson):
    """!Calc MESH IJK from cell sizes."""
    xb = geometry.utils.get_bbox_xb(context=context, ob=ob, world=True)
    ijk = (
        round((xb[1] - xb[0]) / desired_cs[0]) or 1,
        round((xb[3] - xb[2]) / desired_cs[1]) or 1,
        round((xb[5] - xb[4]) / desired_cs[2]) or 1,
    )
    if poisson:
        return get_poisson_ijk(ijk)
    else:
        return ijk


def get_msgs(context, ob):
    """!Get message for MESHes."""
    ijk = ob.bf_mesh_ijk
    cs = get_cell_sizes(context, ob)
    has_good_ijk = tuple(ijk) == get_poisson_ijk(ijk) and "Yes" or "No"
    aspect = get_cell_aspect(cs)
    nsplit, qty = get_nsplit(ob)
    if nsplit > 1:
        split = f"Cell Qty: {qty} · {nsplit} splits"
    else:
        split = f"Cell Qty: {qty}"
    return (
        f"MESH {split}",
        f"Cell Size: {cs[0]:.3f}m · {cs[1]:.3f}m · {cs[2]:.3f}m | Aspect: {aspect:.1f} | Poisson: {has_good_ijk}",
    )
