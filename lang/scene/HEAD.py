import logging
from bpy.types import Scene
from ...types import BFParam, BFParamFYI, BFNamelistSc

log = logging.getLogger(__name__)


class SP_HEAD_CHID(BFParam):
    label = "CHID"
    description = "Case identificator, also used as case filename"
    fds_label = "CHID"
    bpy_type = Scene
    bpy_idname = "name"

    def copy_to(self, dest_element):
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


class SN_TAIL(BFNamelistSc):
    label = "TAIL"
    description = "Case closing"
    enum_id = 3010
    fds_label = "TAIL"

    def to_fds_namelist(self, context):
        pass

    def from_fds(self, context, fds_namelist):
        pass
