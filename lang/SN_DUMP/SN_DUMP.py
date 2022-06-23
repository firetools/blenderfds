# SPDX-License-Identifier: GPL-3.0-or-later

import logging, io
from bpy.types import Scene
from bpy.props import BoolProperty, FloatProperty, IntProperty
from ...config import TIME_P
from ... import utils
from ...types import BFParam, BFParamOther, BFParamFYI, BFNamelistSc, FDSParam, FDSList
from ...bl.ui_lists import WM_PG_bf_other, WM_UL_bf_other_items
from .sc_to_ge1 import scene_to_ge1

log = logging.getLogger(__name__)


class SP_DUMP_FYI(BFParamFYI):
    bpy_type = Scene
    bpy_idname = "bf_dump_fyi"


class SP_DUMP_render_file(BFParam):
    label = "Export Geometric Description File"
    description = "Export geometric description file GE1"
    fds_label = "RENDER_FILE"
    bpy_type = Scene
    bpy_idname = "bf_dump_render_file"
    bpy_prop = BoolProperty
    fds_default = False

    def to_fds_list(self, context) -> FDSList:
        if not self.get_exported(context):
            return FDSList()
        filepath = utils.io.transform_rbl_to_abs(
            context=context,
            filepath_rbl=context.scene.bf_config_directory,
            name=self.element.name,
            extension=".ge1",
        )
        ge1_text = scene_to_ge1(context, self)
        utils.io.write_txt_file(filepath, ge1_text)
        return FDSParam(fds_label="RENDER_FILE", value=f"{self.element.name}.ge1")

    def set_value(self, context, value=None):
        self.element.bf_dump_render_file = bool(value)


class SP_DUMP_STATUS_FILES(BFParam):
    label = "STATUS_FILES"
    description = "Export status file (*.notready), deleted when the simulation is completed successfully"
    fds_label = "STATUS_FILES"
    fds_default = False
    bpy_type = Scene
    bpy_prop = BoolProperty
    bpy_idname = "bf_dump_status_files"


class SP_DUMP_NFRAMES(BFParam):
    label = "NFRAMES"
    description = "Number of output dumps per calculation"
    fds_label = "NFRAMES"
    fds_default = 1000
    bpy_type = Scene
    bpy_idname = "bf_dump_nframes"
    bpy_prop = IntProperty
    bpy_other = {"min": 1}
    bpy_export = "bf_dump_nframes_export"
    bpy_export_default = False

    def get_exported(self, context):
        return (
            super().get_exported(context)
            and not self.element.bf_dump_frames_freq_export
        )


class SP_DUMP_frames_freq(BFParam):
    label = "Dump Output Frequency [s]"
    description = "Dump output frequency in seconds"
    bpy_type = Scene
    bpy_idname = "bf_dump_frames_freq"
    bpy_prop = FloatProperty
    bpy_other = {"min": 0.01, "precision": TIME_P}
    bpy_default = 1.0
    bpy_export = "bf_dump_frames_freq_export"
    bpy_export_default = False

    def to_fds_list(self, context) -> FDSList:
        if not self.get_exported(context):
            return FDSList()
        nframes = int(
            (self.element.bf_time_t_end - self.element.bf_time_t_begin)
            // self.element.bf_dump_frames_freq
        )
        if nframes < 1:
            nframes = 1
        return FDSParam(fds_label="NFRAMES", value=nframes)


class SP_DUMP_DT_RESTART(BFParam):
    label = "DT_RESTART [s]"
    description = "Time interval between restart files are saved"
    fds_label = "DT_RESTART"
    fds_default = 600.0
    bpy_type = Scene
    bpy_idname = "bf_dump_dt_restart"
    bpy_prop = FloatProperty
    bpy_other = {"min": 1.0, "precision": TIME_P}
    bpy_export = "bf_dump_dt_restart_export"
    bpy_export_default = False


class SP_DUMP_other(BFParamOther):
    bpy_type = Scene
    bpy_idname = "bf_dump_other"
    bpy_pg = WM_PG_bf_other
    bpy_ul = WM_UL_bf_other_items


class SN_DUMP(BFNamelistSc):
    label = "DUMP"
    description = "Output parameters"
    enum_id = 3005
    fds_label = "DUMP"
    bpy_export = "bf_dump_export"
    bpy_export_default = False
    bf_params = (
        SP_DUMP_FYI,
        SP_DUMP_render_file,
        SP_DUMP_STATUS_FILES,
        SP_DUMP_NFRAMES,
        SP_DUMP_frames_freq,
        SP_DUMP_DT_RESTART,
        SP_DUMP_other,
    )
