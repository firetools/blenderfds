# SPDX-License-Identifier: GPL-3.0-or-later

import logging
from ..types import BFNamelistSc, BFNotImported

log = logging.getLogger(__name__)

# This namelist is not displayed in the bf_namelist_cls menu,
# and has no panel.
# Used for importing only, it translates
# FDS MOVE transformations to a Scene dict
# It is used in conjuction with ON_MOVE


class SN_MOVE(BFNamelistSc):
    label = "MOVE"
    description = "Geometric transformations"
    enum_id = False  # no bf_namelist_cls menu
    fds_label = "MOVE"
    bf_import_order = 10

    def get_exported(self, context):  # No automatic export
        return False

    def from_fds_list(self, context, fds_list):
        # Get ID
        fds_param = fds_list.get_fds_param(fds_label="ID", remove=True)
        if not fds_param:
            raise BFNotImported(self, f"Missing ID in: {fds_list}")

        # Prepare Scene dict
        if "bf_move_coll" not in context.scene:
            context.scene["bf_move_coll"] = dict()
        bf_move_coll = context.scene["bf_move_coll"]

        # Set Scene dict by ID, eg. {"ob_move": "A=3 B=4 C=5"}
        f90_params = " ".join((item.to_string() for item in fds_list))
        bf_move_coll[fds_param.get_value()] = f90_params
