"""!
MESH namelist definition
"""

import logging
from bpy.types import Object
from bpy.props import IntVectorProperty
from ...types import BFParam, BFNamelistOb, FDSParam, FDSMulti, FDSList, BFException
from ... import utils
from ..bf_object import OP_namelist_cls, OP_ID, OP_FYI, OP_other, OP_RGB, OP_COLOR
from ..OP_XB import OP_XB_BBOX, ob_to_xbs, xbs_to_ob
from ..ON_MULT import OP_other_MULT_ID, multiply_xbs, get_nmult
from .split_mesh import split_mesh, get_nsplit
from .align_meshes import get_n_for_poisson

log = logging.getLogger(__name__)


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
        for msg in _get_mesh_msgs(context, ob):
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
    bpy_other = {"size": 3, "min": 1}
    bpy_export = "bf_mesh_nsplits_export"
    bpy_export_default = False

    def check(self, context):
        ob = self.element
        value = self.get_value(context)
        requested_nsplit = value[0] * value[1] * value[2]
        nsplit, _ = get_nsplit(ob)
        if ob.bf_mesh_nsplits_export and requested_nsplit != nsplit:
            raise BFException(self, "Too many splits requested for current IJK")

    def draw(self, context, layout):
        # Split IJK panel is integrated with IJK
        pass


class OP_MESH_XB_BBOX(OP_XB_BBOX):
    # This class implements OP_XB_BBOX
    # with MESH split and IJK calculations
    def _get_geometry(self, context):
        ob = self.element
        xbs, msgs = ob_to_xbs(context=context, ob=ob, bf_xb="BBOX")
        ids, ijks, xbs = split_mesh(
            hid=ob.name,
            ijk=ob.bf_mesh_ijk,
            export=ob.bf_mesh_nsplits_export,
            nsplits=ob.bf_mesh_nsplits,
            xb=xbs[0],
        )
        msgs.extend(_get_mesh_msgs(context, ob))
        return ob, ids, ijks, xbs, msgs

    def to_fds_list(self, context) -> FDSList:
        _, ids, ijks, xbs, msgs = self._get_geometry(context)
        return FDSMulti(
            iterable=(
                FDSList(
                    iterable=(
                        FDSParam(fds_label="ID", value=hid),
                        FDSParam(fds_label="IJK", value=ijk),
                        FDSParam(fds_label="XB", value=xb, precision=6),
                    )
                )
                for hid, xb, ijk in zip(ids, xbs, ijks)
            ),
            msgs=msgs,
        )

    def show_fds_geometry(self, context, ob_tmp):
        ob, _, _, xbs, _ = self._get_geometry(context)
        xbs, _ = multiply_xbs(xbs, hids=None, ob=ob)
        xbs_to_ob(context=context, ob=ob_tmp, xbs=xbs, bf_xb="BBOX", add=True)

    # def from_fds() inherited


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
        OP_RGB,
        OP_COLOR,
        OP_other,
    )
    bf_other = {"appearance": "BBOX"}


# Helper functions


def get_cell_sizes(context, ob):
    """!Get cell sizes by axis."""
    xb = utils.geometry.get_bbox_xb(context=context, ob=ob, world=True)
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
    xb = utils.geometry.get_bbox_xb(context=context, ob=ob, world=True)
    ijk = (
        round((xb[1] - xb[0]) / desired_cs[0]) or 1,
        round((xb[3] - xb[2]) / desired_cs[1]) or 1,
        round((xb[5] - xb[4]) / desired_cs[2]) or 1,
    )
    if poisson:
        return get_poisson_ijk(ijk)
    else:
        return ijk


def _get_mesh_msgs(context, ob):
    """!Get message for MESHes."""
    # Calc
    ijk = ob.bf_mesh_ijk
    cs = get_cell_sizes(context, ob)
    has_good_ijk = tuple(ijk) == get_poisson_ijk(ijk) and "Yes" or "No"
    aspect = get_cell_aspect(cs)
    nsplit, qty = get_nsplit(ob)
    nmult = get_nmult(ob)
    nmesh = nsplit * nmult
    qty_tot = ijk[0] * ijk[1] * ijk[2] * nmult
    # Prepare msgs
    if nmesh > 1:
        return (
            f"MESH: {nmesh} | Cell Qty: {qty} ({qty_tot} tot) | Splits: {nsplit} | Multiples: {nmult}",
            f"Size: {cs[0]:.3f}m · {cs[1]:.3f}m · {cs[2]:.3f}m | Aspect: {aspect:.1f} | Poisson: {has_good_ijk}",
        )
    else:
        return (
            f"Cell Qty: {qty}",
            f"Size: {cs[0]:.3f}m · {cs[1]:.3f}m · {cs[2]:.3f}m | Aspect: {aspect:.1f} | Poisson: {has_good_ijk}",
        )
