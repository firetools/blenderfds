import logging
from math import radians
from mathutils import Matrix, Vector
from bpy.types import Object
from .. import utils
from ..types import (
    BFNamelistSc,
    BFParam,
    FDSNamelist,
    FDSParam,
    FDSList,
    BFNotImported,
)

log = logging.getLogger(__name__)

class SN_MOVE(BFNamelistSc):  # FIXME FIXME FIXME
    # This namelist is not displayed in the bf_namelist_cls menu,
    # and has no panel. Used for importing only, it translates
    # FDS MOVE transformations in Blender matrix of a scene dict
    label = "MOVE"
    description = "Geometric transformations"
    enum_id = False  # no bf_namelist_cls menu, no automatic export
    fds_label = "MOVE"

    def get_exported(self, context):
        return False

    def from_fds(self, context, fds_namelist):
        # Read fds_params
        ps = {  # label: default value
            "ID": None,
            "T34": None,
            "X0": 0.0,
            "Y0": 0.0,
            "Z0": 0.0,
            "AXIS": (0.0, 0.0, 1.0),
            "ROTATION_ANGLE": 0.0,
            "SCALE": None,
            "SCALEX": 1.0,
            "SCALEY": 1.0,
            "SCALEZ": 1.0,
            "DX": 0.0,
            "DY": 0.0,
            "DZ": 0.0,
        }
        for fds_label in ps:
            fds_param = fds_namelist.get_fds_label(fds_label=fds_label, remove=True)
            if fds_param:
                ps[fds_label] = fds_param.get_value()  # assign value
        # Check ID
        hid = ps["ID"]
        if not hid:
            raise BFNotImported(self, f"Missing ID in: {fds_namelist}")
        # Prepare Scene dict
        if "bf_move_coll" not in context.scene:
            context.scene["bf_move_coll"] = dict()
        bf_move_coll = context.scene["bf_move_coll"]
        # Treat T34
        if ps["T34"]:
            m = fds_move_to_bl_matrix(t34=ps["T34"])
        # Treat other cases
        else:
            m = fds_move_to_bl_matrix(
                x0=ps["X0"],
                y0=ps["Y0"],
                z0=ps["Z0"],
                axis=ps["AXIS"],
                rotation_angle=ps["ROTATION_ANGLE"],
                scale=ps["SCALE"],
                scalex=ps["SCALEX"],
                scaley=ps["SCALEY"],
                scalez=ps["SCALEZ"],
                dx=ps["DX"],
                dy=ps["DY"],
                dz=ps["DZ"],
            )
        # Set Scene dict
        bf_move_coll[hid] = m


# This namelist is called by other BFNamelistOb
# When called the Blender Mesh should already be available

# FIXME move to ON_MOVE.py


