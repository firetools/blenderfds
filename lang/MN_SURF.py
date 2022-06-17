# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from ..types import BFNamelistMa
from .bf_material import (
    MP_namelist_cls,
    MP_ID,
    MP_FYI,
    MP_DEFAULT,
    MP_RGB,
    MP_COLOR,
    MP_TRANSPARENCY,
    MP_other,
)
from .. import config

log = logging.getLogger(__name__)


class MN_SURF(BFNamelistMa):
    label = "SURF"
    description = "Boundary condition"
    enum_id = 2000
    fds_label = "SURF"
    bpy_export = "bf_surf_export"
    bpy_export_default = True
    bf_params = (
        MP_namelist_cls,
        MP_ID,
        MP_FYI,
        MP_DEFAULT,
        MP_RGB,
        MP_COLOR,
        MP_TRANSPARENCY,
        MP_other,
    )
    bf_import_order = 40

    def get_exported(self, context):  # was in MP_namelist_cls
        if self.element.name in config.DEFAULT_MAS:
            return False
        return super().get_exported(context)
