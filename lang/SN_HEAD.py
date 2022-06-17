# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from bpy.types import Scene
from ..types import BFParam, BFParamFYI, BFNamelistSc, FDSList, BFException
from .. import utils

log = logging.getLogger(__name__)


class SP_HEAD_CHID(BFParam):
    label = "CHID"
    description = "Case identificator, also used as case filename"
    fds_label = "CHID"
    bpy_type = Scene
    bpy_idname = "name"

    def check(self, context):
        name = self.element.name
        if "." in name:
            raise BFException(self, f"No periods allowed")

    def copy_to(self, context, dest_element):
        pass


class SP_HEAD_TITLE(BFParamFYI):
    label = "TITLE"
    description = "Case description"
    fds_label = "TITLE"
    bpy_type = Scene
    bpy_idname = "bf_head_title"
    bpy_other = {"maxlen": 64}


class SN_HEAD(BFNamelistSc):
    label = "HEAD"
    description = "Case header"
    enum_id = 3001
    fds_label = "HEAD"
    bpy_export = "bf_head_export"
    bpy_export_default = True
    bf_params = SP_HEAD_CHID, SP_HEAD_TITLE
    bf_import_order = 0  # first imported


class SN_TAIL(BFNamelistSc):  # importing only, prevent free_text
    label = "TAIL"
    description = "Case closing"
    enum_id = 3010  # to avoid TAIL import into free_text
    fds_label = "TAIL"
    bf_import_order = 1e5  # last imported

    def get_exported(self, context):
        return False

    def from_fds_list(self, context, fds_list):
        pass
