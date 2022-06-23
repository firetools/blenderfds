# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Object
from bpy.props import FloatProperty, IntProperty, FloatVectorProperty
from ...config import T34_P
from ...types import BFNamelist, BFParam, BFNotImported, FDSList, FDSNamelist, FDSParam
from ... import utils
from .t34 import calc_t34, calc_bl_matrix

log = logging.getLogger(__name__)


class OP_MOVE_ID(BFParam):
    label = "ID"
    description = "Geometric transformation name"
    fds_label = "ID"
    bpy_type = Object

    def get_value(self, context):
        return f"{self.element.name}_move"

    def set_value(self, context, value=None):
        pass


class OP_MOVE_T34(BFParam):
    label = "T34"
    description = "Geometric transformation 3x4 matrix"
    fds_label = "T34"
    bpy_type = Object
    bpy_other = {"precision": T34_P}

    def get_value(self, context):
        return calc_t34(m=self.element.matrix_world)

    def set_value(self, context, value=None):
        utils.geometry.transform_ob(
            ob=self.element,
            m=calc_bl_matrix(t34=value),
            force_othogonal=False,
        )


class ON_MOVE(BFNamelist):  # not in namelist menu
    label = "MOVE"
    description = "Geometric Transformation"
    bpy_type = Object
    enum_id = None
    fds_label = "MOVE"
    bf_params = (
        OP_MOVE_ID,
        OP_MOVE_T34,
    )

    def from_fds_list(self, context, fds_list, fds_label=None):
        # Read fds_params
        ps = {  # label: default value
            "T34": None,
            "X0": 0.0,
            "Y0": 0.0,
            "Z0": 0.0,
            "AXIS": (0.0, 0.0, 1.0),
            "ROTATION_ANGLE": 0.0,
            "SCALE": None,
            "SCALEX": 1.0,
            "SCALEY": 1.0,
            "SCALEZ": 1.0,
            "DX": 0.0,
            "DY": 0.0,
            "DZ": 0.0,
        }
        for fds_label in ps:
            fds_param = fds_list.get_fds_param(fds_label=fds_label, remove=True)
            if fds_param:
                ps[fds_label] = fds_param.get_value()  # assign value
        m = calc_bl_matrix(
            t34=ps["T34"],
            dx=ps["DX"],
            dy=ps["DY"],
            dz=ps["DZ"],
            scale=ps["SCALE"],
            scalex=ps["SCALEX"],
            scaley=ps["SCALEY"],
            scalez=ps["SCALEZ"],
            x0=ps["X0"],
            y0=ps["Y0"],
            z0=ps["Z0"],
            rotation_angle=ps["ROTATION_ANGLE"],
            axis=ps["AXIS"],
        )
        utils.geometry.transform_ob(ob=self.element, m=m, force_othogonal=False)


# Called by other namelists
# that support a MOVE_ID
# and the relative MOVE namelist
# (See also: ON_MULT)


class OP_other_MOVE_ID(BFParam):
    label = "MOVE_ID"
    description = "Reference to geometric transformation"
    fds_label = "MOVE_ID"
    bpy_type = Object

    def get_value(self, context):
        return f"{self.element.name}_move"

    def set_value(self, context, value=None):
        # Get required MOVE parameters from dict created by SN_MOVE
        try:
            f90_params = context.scene["bf_move_coll"][value]
        except KeyError as err:
            raise BFNotImported(self, f"Missing MOVE ID='{value}'")
        ON_MOVE(element=self.element).from_fds_list(
            context=context,
            fds_list=FDSList(f90_params=f90_params),
        )

    def get_active(self, context):  # set in namelists that use it (eg. ON_GEOM)
        return False

    def get_exported(self, context):
        return self.get_active(context)

    def to_fds_list(self, context) -> FDSList:
        if self.get_exported(context):
            return FDSList(
                iterable=(
                    super().to_fds_list(context),
                    ON_MOVE(element=self.element).to_fds_list(context),
                )
            )
        else:
            return FDSList()

    def draw(self, context, layout):  # only label
        row = layout.split(factor=0.4)
        active = self.get_active(context)
        row.active = active
        row.alignment = "RIGHT"
        row.label(text=self.label)
        row.alignment = "EXPAND"
        row.label(icon=active and "LINKED" or "UNLINKED")
