import logging
from ..types import BFNamelistMa
from .bf_material import MP_ID, MP_FYI, MP_RGB, MP_COLOR, MP_TRANSPARENCY, MP_other

log = logging.getLogger(__name__)


class MN_SURF(BFNamelistMa):
    label = "SURF"
    description = "Boundary condition"
    enum_id = 2000
    fds_label = "SURF"
    bpy_export = "bf_surf_export"
    bpy_export_default = True
    bf_params = (
        MP_ID,
        MP_FYI,
        MP_RGB,
        MP_COLOR,
        MP_TRANSPARENCY,
        MP_other,
    )

# TODO bc templates op