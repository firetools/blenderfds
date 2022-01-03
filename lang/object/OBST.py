import logging
from ...types import BFNamelistOb
from .object import OP_ID, OP_FYI, OP_ID_suffix, OP_other
from .SURF_ID import OP_SURF_ID
from .XB import OP_XB, OP_XB_custom_voxel, OP_XB_voxel_size, OP_XB_center_voxels

log = logging.getLogger(__name__)


class ON_OBST(BFNamelistOb):
    label = "OBST"
    description = "Obstruction"
    enum_id = 1000
    fds_label = "OBST"
    bf_params = (
        OP_ID,
        OP_FYI,
        OP_SURF_ID,
        OP_XB,
        OP_XB_custom_voxel,
        OP_XB_voxel_size,
        OP_XB_center_voxels,
        OP_ID_suffix,
        OP_other,
    )
    bf_other = {"appearance": "TEXTURED"}
