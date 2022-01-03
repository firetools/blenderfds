import logging, io
from bpy.types import Scene
from bpy.props import BoolProperty
from ... import utils
from ...types import (
    BFParam,
    BFParamOther,
    BFNamelistSc,
    FDSParam,
    BFException,
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
        el = self.element
        coll = getattr(self.element, self.bpy_idname)
        result = list()
        for p in coll:
            if p.bf_export and p.name:
                if el.bf_catf_check_files and not utils.is_file(p.name):
                    raise BFException(self, f"File path <{p.name}> does not exist")
                result.append(tuple((FDSParam(fds_label="OTHER_FILES", value=p.name),)))
        return tuple(result)  # multi

    def from_fds(self, context, value):
        if not value:
            self.set_value(context, None)
        elif isinstance(value, str):
            self.set_value(context, value)
        else:  # tuple of str
            for v in value:
                self.set_value(context, v)


class SN_CATF(BFNamelistSc):
    label = "CATF"
    description = "Concatenated file paths"
    fds_label = "CATF"
    bpy_export = "bf_catf_export"
    bpy_export_default = False
    bf_params = SP_CATF_check_files, SP_CATF_files
