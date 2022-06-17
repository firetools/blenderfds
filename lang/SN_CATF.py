# SPDX-License-Identifier: GPL-3.0-or-later

import logging, os
from bpy.types import Scene
from .. import utils
from ..types import BFParam, BFNamelistSc, BFNotImported


log = logging.getLogger(__name__)


class SP_CATF_files(BFParam):
    label = "Concatenated File Paths"
    description = "Concatenated file paths"
    fds_label = "OTHER_FILES"
    bpy_type = Scene

    def set_value(self, context, value=None):
        if not value:
            return
        if isinstance(value, str):
            value = (value,)
        for v in value:
            filepath = utils.io.transform_rfds_to_abs(context=context, filepath_rfds=v)
            context.scene.from_fds(context=context, filepath=filepath)


class SN_CATF(BFNamelistSc):  # for importing only
    label = "CATF"
    description = "Concatenated file paths"
    fds_label = "CATF"
    bf_params = (SP_CATF_files,)
    bf_import_order = 50

    def get_exported(self, context):
        return False
