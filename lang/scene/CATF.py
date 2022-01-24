import logging, os
from bpy.types import Scene
from bpy.props import BoolProperty
from ... import utils
from ...types import (
    BFParam,
    BFParamOther,
    BFNamelistSc,
    FDSParam,
    BFException,
    BFNotImported,
)
from ...bl.ui_lists import WM_PG_bf_filepaths, WM_UL_bf_filepaths_items


log = logging.getLogger(__name__)


class SP_CATF_check_files(BFParam):
    label = "Check File Existance While Exporting"
    description = "Check file existence while exporting filepaths"
    bpy_type = Scene
    bpy_idname = "bf_catf_check_files"
    bpy_prop = BoolProperty
    bpy_default = False

    def to_fds_param(self, context):
        pass


class SP_CATF_files(BFParamOther):
    label = "Concatenated File Paths"
    description = "Concatenated files (eg. PROP='/drive/test.catf')"
    fds_label = "OTHER_FILES"
    bpy_type = Scene
    bpy_idname = "bf_catf_files"
    bpy_pg = WM_PG_bf_filepaths
    bpy_ul = WM_UL_bf_filepaths_items

    def to_fds_param(self, context):
        coll = getattr(self.element, self.bpy_idname)
        result = list()
        for p in coll:
            if not p.bf_export or not p.name:
                continue
            # Get filepaths
            filepath, filepath_rfds = utils.io.transform_rbl_to_abs_and_rfds(
                context=context, filepath_rbl=p.name
            )
            # If requested, check existence
            if self.element.bf_catf_check_files and not utils.io.is_file(filepath):
                raise BFException(self, f"File does not exist: <{filepath}>")
            # Append result
            result.append(FDSParam(fds_label="OTHER_FILES", value=filepath_rfds))
        # Make multi
        result = tuple(tuple((r,)) for r in result)  # make multi
        return result

    def from_fds(self, context, value):
        if not value:
            self.set_value(context, None)
            return
        if isinstance(value, str):
            value = (value,)
        for v in value:
            filepath, filepath_rbl = utils.io.transform_rfds_to_abs_and_rbl(
                context, filepath_rfds=v
            )
            if not utils.io.is_file(filepath):
                raise BFNotImported(self, f"File does not exist: <{filepath}>")
            self.set_value(context, filepath_rbl)


class SN_CATF(BFNamelistSc):
    label = "CATF"
    description = "Concatenated file paths"
    fds_label = "CATF"
    bpy_export = "bf_catf_export"
    bpy_export_default = False
    bf_params = SP_CATF_check_files, SP_CATF_files
