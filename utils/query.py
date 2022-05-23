# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, input/output routines.
"""

import os, bpy, logging
from pathlib import Path
from ..types import BFException, BFNotImported, FDSList
from .. import config
from .. import utils

log = logging.getLogger(__name__)


def get_referenced_ids(context, sc, fds_label="SURF_ID"):  # TODO unused
    """!
    Get fds_label IDs referenced in Free Text and CATF files (eg. SURF_ID).
    """
    fds_list = FDSList()
    sc = context.scene
    # Get namelists from Free Text
    if sc.bf_config_text:
        fds_list.from_fds(f90_namelists=sc.bf_config_text.as_string())
    # Get namelists from available CATF files
    if sc.bf_catf_export:
        for item in sc.bf_catf_files:
            if not item.bf_export:
                continue
            filepath = item.name
            try:
                f90_namelists = utils.io.read_txt_file(filepath)
            except IOError:
                pass
            else:
                fds_list.from_fds(f90_namelists=f90_namelists)
    # Prepare list of IDs
    items = list()
    for fds_namelist in fds_list:
        fds_param = fds_namelist.get_fds_label(fds_label=fds_label)
        if fds_param:
            hid = fds_param.get_value()
            items.append(hid)
    items.sort(key=lambda k: k[0])
    return items


def _get_namelist_items(self, context, fds_label):  # TODO unused
    """!
    Get fds_label namelist IDs available in Free Text and CATF files.
    """
    fds_list = FDSList()
    sc = context.scene
    # Get namelists from Free Text
    if sc.bf_config_text:
        fds_list.from_fds(f90_namelists=sc.bf_config_text.as_string())
    # Get namelists from available CATF files
    if sc.bf_catf_export:
        for item in sc.bf_catf_files:
            if not item.bf_export:
                continue
            filepath = item.name
            try:
                f90_namelists = utils.io.read_txt_file(filepath)
            except IOError:
                pass
            else:
                fds_list.from_fds(f90_namelists=f90_namelists)
    # Prepare list of IDs
    items = list()
    while True:
        fds_namelist = fds_list.get_fds_label(fds_label=fds_label, remove=True)
        if not fds_namelist:
            break
        fds_param = fds_namelist.get_fds_label(fds_label="ID", remove=True)
        if fds_param:
            hid = fds_param.get_value()
            items.append((hid, hid, ""))
    items.sort(key=lambda k: k[0])
    return items
