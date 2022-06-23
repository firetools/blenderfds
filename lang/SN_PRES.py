# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Scene
from bpy.props import BoolProperty, FloatProperty, IntProperty
from ..config import VELOCITY_TOLERANCE_P
from ..types import (
    BFParam,
    BFParamOther,
    BFParamFYI,
    BFNamelistSc,
)
from ..bl.ui_lists import (
    WM_PG_bf_other,
    WM_UL_bf_other_items,
)

log = logging.getLogger(__name__)


class SP_PRES_FYI(BFParamFYI):
    bpy_type = Scene
    bpy_idname = "bf_pres_fyi"


class SP_PRES_BAROCLINIC(BFParam):
    label = "BAROCLINIC"
    description = "Consider baroclinic torque"
    fds_label = "BAROCLINIC"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_pres_baroclinic"


class SP_PRES_VELOCITY_TOLERANCE(BFParam):
    label = "VELOCITY_TOLERANCE"
    description = "Maximum allowable normal velocity component\non the solid boundary or the largest error at a mesh interface"
    fds_label = "VELOCITY_TOLERANCE"
    bpy_type = Scene
    bpy_idname = "bf_pres_velocity_tolerance"
    bpy_prop = FloatProperty
    bpy_export = "bf_pres_velocity_tolerance_export"
    bpy_export_default = False
    bpy_other = {"precision": VELOCITY_TOLERANCE_P, "min": 0.0, "max": 1.0}


class SP_PRES_MAX_PRESSURE_ITERATIONS(BFParam):
    label = "MAX_PRESSURE_ITERATIONS"
    description = "Maximum number of pressure iterations for each half of the time step"
    fds_label = "MAX_PRESSURE_ITERATIONS"
    fds_default = 10
    bpy_type = Scene
    bpy_idname = "bf_pres_max_pressure_iterations"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}
    bpy_export = "bf_pres_max_pressure_iterations_export"
    bpy_export_default = False


class SP_PRES_other(BFParamOther):
    bpy_type = Scene
    bpy_idname = "bf_pres_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class SN_PRES(BFNamelistSc):
    label = "PRES"
    description = "Pressure Solver"
    enum_id = 3007
    fds_label = "PRES"
    bpy_export = "bf_pres_export"
    bpy_export_default = False
    bf_params = (
        SP_PRES_FYI,
        SP_PRES_BAROCLINIC,
        SP_PRES_VELOCITY_TOLERANCE,
        SP_PRES_MAX_PRESSURE_ITERATIONS,
        SP_PRES_other,
    )
