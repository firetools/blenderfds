import logging, bpy
from bpy.types import Object
from ..types import (
    BFNamelistSc,
    BFParam,
    BFNotImported,
)
from .. import utils
from .OP_XB.xbs_to_ob import xbs_to_ob

log = logging.getLogger(__name__)


def mult_xb_to_xbs(
    xb,
    hid,
    dxb,
    dx,
    dx0,
    dy,
    dy0,
    dz,
    dz0,
    i_lower,
    i_lower_skip,
    i_upper,
    i_upper_skip,
    j_lower,
    j_lower_skip,
    j_upper,
    j_upper_skip,
    k_lower,
    k_lower_skip,
    k_upper,
    k_upper_skip,
    n_lower,
    n_lower_skip,
    n_upper,
    n_upper_skip,
):
    hids, xbs = list(), list()
    if n_lower or n_upper:
        # Check skip
        has_skip = n_lower_skip is not None or n_upper_skip is not None
        if n_lower_skip is None:
            n_lower_skip = n_lower
        if n_upper_skip is None:
            n_upper_skip = n_upper
        # Build
        for n in range(n_lower, n_upper + 1):
            if has_skip and n_lower_skip <= n <= n_upper_skip:
                continue
            hids.append(f"{hid}_{n}")
            xbs.append(
                (
                    xb[0] + dx0 + n * dxb[0],
                    xb[1] + dx0 + n * dxb[1],
                    xb[2] + dy0 + n * dxb[2],
                    xb[3] + dy0 + n * dxb[3],
                    xb[4] + dz0 + n * dxb[4],
                    xb[5] + dz0 + n * dxb[5],
                )
            )
    else:  # mesh refining example
        # Check skip
        has_skip = True
        if all(
            x is None
            for x in (
                i_lower_skip,
                i_upper_skip,
                j_lower_skip,
                j_upper_skip,
                k_lower_skip,
                k_upper_skip,
            )
        ):
            has_skip = False
        if i_lower_skip is None:
            i_lower_skip = i_lower
        if i_upper_skip is None:
            i_upper_skip = i_upper
        if j_lower_skip is None:
            j_lower_skip = j_lower
        if j_upper_skip is None:
            j_upper_skip = j_upper
        if k_lower_skip is None:
            k_lower_skip = k_lower
        if k_upper_skip is None:
            k_upper_skip = k_upper
        # Build
        for i in range(i_lower, i_upper + 1):
            for j in range(j_lower, j_upper + 1):
                for k in range(k_lower, k_upper + 1):
                    if has_skip and (
                        i_lower_skip <= i <= i_upper_skip
                        and j_lower_skip <= j <= j_upper_skip
                        and k_lower_skip <= k <= k_upper_skip
                    ):
                        continue
                    hids.append(f"{hid}_{i}_{j}_{k}")
                    xbs.append(
                        (
                            xb[0] + dx0 + i * dx,
                            xb[1] + dx0 + i * dx,
                            xb[2] + dy0 + j * dy,
                            xb[3] + dy0 + j * dy,
                            xb[4] + dz0 + k * dz,
                            xb[5] + dz0 + k * dz,
                        )
                    )
    return hids, xbs


# Classes


class SN_MULT(BFNamelistSc):
    # This namelist is not displayed in the bf_namelist_cls menu,
    # and has no panel. Used for importing only, it translates
    # FDS MULT transformations in a scene dict
    label = "MULT"
    description = "Multiplier transformations"
    enum_id = False  # no bf_namelist_cls menu, no automatic export
    fds_label = "MULT"

    def to_fds_namelist(self, context):
        pass

    def from_fds(self, context, fds_namelist):
        # Read fds_params
        ps = {  # label: default value
            "ID": None,
            "DXB": None,
            "DX": 0.0,
            "DX0": 0.0,
            "DY": 0.0,
            "DY0": 0.0,
            "DZ": 0.0,
            "DZ0": 0.0,
            "I_LOWER": 0,
            "I_LOWER_SKIP": None,
            "I_UPPER": 0,
            "I_UPPER_SKIP": None,
            "J_LOWER": 0,
            "J_LOWER_SKIP": None,
            "J_UPPER": 0,
            "J_UPPER_SKIP": None,
            "K_LOWER": 0,
            "K_LOWER_SKIP": None,
            "K_UPPER": 0,
            "K_UPPER_SKIP": None,
            "N_LOWER": 0,
            "N_LOWER_SKIP": None,
            "N_UPPER": 0,
            "N_UPPER_SKIP": None,
        }
        for fds_label in ps:
            fds_param = fds_namelist.get_fds_param(fds_label=fds_label, remove=True)
            if fds_param:
                ps[fds_label] = fds_param.get_value(context)  # assign value
        # Check ID
        hid = ps["ID"]
        if not hid:
            raise BFNotImported(self, f"Missing ID in: {fds_namelist}")
        # Prepare Scene dict
        if "bf_mult_coll" not in context.scene:
            context.scene["bf_mult_coll"] = dict()
        bf_mult_coll = context.scene["bf_mult_coll"]
        # Set Scene dict
        bf_mult_coll[hid] = ps


# This namelist is called by other BFNamelistOb
# When called the Blender Mesh should already be available
# Used by ON_MESH then ON_INIT  ON_OBST ON_VENT ON_HOLE


class OP_MULT_ID(BFParam):
    label = "MULT_ID"
    description = "Reference to geometric transformation"
    fds_label = "MULT_ID"
    bpy_type = Object

    # Not used, MULT never exported
    # def to_fds_param(self, context):
    #     if self.get_exported(context):
    #         return FDSParam(fds_label="MULT_ID", value=f"{self.element.name}_mult")

    def from_fds(self, context, value):
        try:
            ps = context.scene["bf_mult_coll"][value]
        except KeyError as err:
            raise BFNotImported(self, f"Missing MULT ID={value}")
        # Multiplication algorithm
        ob = self.element
        ob.bf_xb = "BBOX"
        ob.bf_xb_export = True
        hids, xbs = mult_xb_to_xbs(
            xb=utils.geometry.get_bbox_xb(context, ob=ob, world=True),
            hid=ob.name,
            dxb=ps["DXB"],
            dx=ps["DX"],
            dx0=ps["DX0"],
            dy=ps["DY"],
            dy0=ps["DY0"],
            dz=ps["DZ"],
            dz0=ps["DZ0"],
            i_lower=ps["I_LOWER"],
            i_lower_skip=ps["I_LOWER_SKIP"],
            i_upper=ps["I_UPPER"],
            i_upper_skip=ps["I_UPPER_SKIP"],
            j_lower=ps["J_LOWER"],
            j_lower_skip=ps["J_LOWER_SKIP"],
            j_upper=ps["J_UPPER"],
            j_upper_skip=ps["J_UPPER_SKIP"],
            k_lower=ps["K_LOWER"],
            k_lower_skip=ps["K_LOWER_SKIP"],
            k_upper=ps["K_UPPER"],
            k_upper_skip=ps["K_UPPER_SKIP"],
            n_lower=ps["N_LOWER"],
            n_lower_skip=ps["N_LOWER_SKIP"],
            n_upper=ps["N_UPPER"],
            n_upper_skip=ps["N_UPPER_SKIP"],
        )
        obs_new = list((ob,))  # original
        co = ob.users_collection[0]
        for i in range(len(xbs) - 1):  # copies
            ob_new = ob.copy()
            ob_new.data = ob.data.copy()
            co.objects.link(ob_new)
            obs_new.append(ob_new)
        for i, ob_new in enumerate(obs_new):
            ob_new.name = hids[i]
            xbs_to_ob(
                context,
                ob=ob_new,
                xbs=xbs[i : i + 1],
                bf_xb="BBOX",
                set_origin=True,
            )

    def draw(self, context, layout):
        pass
