# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Scene
from bpy.props import BoolProperty, IntProperty
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


class SP_RADI_FYI(BFParamFYI):
    bpy_type = Scene
    bpy_idname = "bf_radi_fyi"


class SP_RADI_RADIATION(BFParam):
    label = "RADIATION"
    description = "Turn on/off the radiation solver"
    fds_label = "RADIATION"
    fds_default = True
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_radi_radiation"


class SP_RADI_NUMBER_RADIATION_ANGLES(BFParam):
    label = "NUMBER_RADIATION_ANGLES"
    description = "Number of angles for spatial resolution of radiation solver"
    fds_label = "NUMBER_RADIATION_ANGLES"
    fds_default = 100
    bpy_type = Scene
    bpy_idname = "bf_radi_number_radiation_angles"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}
    bpy_export = "bf_radi_number_radiation_angles_export"
    bpy_export_default = False


class SP_RADI_TIME_STEP_INCREMENT(BFParam):
    label = "TIME_STEP_INCREMENT"
    description = "Frequency of calls to the radiation solver in time steps"
    fds_label = "TIME_STEP_INCREMENT"
    fds_default = 3
    bpy_type = Scene
    bpy_idname = "bf_radi_time_step_increment"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}
    bpy_export = "bf_radi_time_step_increment_export"
    bpy_export_default = False


class SP_RADI_ANGLE_INCREMENT(BFParam):
    label = "ANGLE_INCREMENT"
    description = "Increment over which the angles are updated"
    fds_label = "ANGLE_INCREMENT"
    fds_default = 5
    bpy_type = Scene
    bpy_idname = "bf_radi_angle_increment"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}
    bpy_export = "bf_radi_angle_increment_export"
    bpy_export_default = False


class SP_RADI_RADIATION_ITERATIONS(BFParam):
    label = "RADIATION_ITERATIONS"
    description = "Number of times the radiative intensity is updated in a time step"
    fds_label = "RADIATION_ITERATIONS"
    fds_default = 1
    bpy_type = Scene
    bpy_idname = "bf_radi_radiation_iterations"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}
    bpy_export = "bf_radi_radiation_iterations_export"
    bpy_export_default = False


class SP_RADI_other(BFParamOther):
    bpy_type = Scene
    bpy_idname = "bf_radi_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class SN_RADI(BFNamelistSc):
    label = "RADI"
    description = "Radiation parameters"
    enum_id = 3006
    fds_label = "RADI"
    bpy_export = "bf_radi_export"
    bpy_export_default = False
    bf_params = (
        SP_RADI_FYI,
        SP_RADI_RADIATION,
        SP_RADI_NUMBER_RADIATION_ANGLES,
        SP_RADI_TIME_STEP_INCREMENT,
        SP_RADI_ANGLE_INCREMENT,
        SP_RADI_RADIATION_ITERATIONS,
        SP_RADI_other,
    )
