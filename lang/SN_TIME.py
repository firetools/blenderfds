# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Scene
from bpy.props import BoolProperty, FloatProperty
from ..config import TIME_P
from ..types import BFParam, BFParamOther, BFNamelistSc, FDSParam, FDSList
from ..bl.ui_lists import WM_PG_bf_other, WM_UL_bf_other_items

log = logging.getLogger(__name__)


class SP_TIME_setup_only(BFParam):
    label = "Smokeview Geometry Setup"
    description = "Set Smokeview to setup only geometry"
    bpy_type = Scene
    bpy_idname = "bf_time_setup_only"
    bpy_prop = BoolProperty
    bpy_default = False


class SP_TIME_T_BEGIN(BFParam):
    label = "T_BEGIN [s]"
    description = "Simulation starting time"
    fds_label = "T_BEGIN"
    fds_default = 0.0
    bpy_type = Scene
    bpy_idname = "bf_time_t_begin"
    bpy_prop = FloatProperty
    bpy_other = {
        "step": 100.0,
        "precision": TIME_P,
    }  # "unit": "TIME", not working

    def get_active(self, context):
        return not self.element.bf_time_setup_only


class SP_TIME_T_END(BFParam):
    label = "T_END [s]"
    description = "Simulation ending time"
    fds_label = "T_END"
    bpy_type = Scene
    bpy_idname = "bf_time_t_end"
    bpy_prop = FloatProperty
    bpy_default = 1.0
    bpy_other = {
        "step": 100.0,
        "precision": TIME_P,
    }  # "unit": "TIME", not working

    def to_fds_list(self, context) -> FDSList:
        if self.element.bf_time_setup_only:
            return FDSParam(
                fds_label="T_END",
                value=0.0,
                msg="Smokeview setup only",
                precision=TIME_P,
            )
        return super().to_fds_list(context)

    def get_active(self, context):
        return not self.element.bf_time_setup_only

    def get_exported(self, context):
        return True


class SP_TIME_other(BFParamOther):
    bpy_type = Scene
    bpy_idname = "bf_time_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class SN_TIME(BFNamelistSc):
    label = "TIME"
    description = "Simulation time settings"
    enum_id = 3002
    fds_label = "TIME"
    bpy_export = "bf_time_export"
    bpy_export_default = True
    bf_params = SP_TIME_T_BEGIN, SP_TIME_T_END, SP_TIME_setup_only, SP_TIME_other
