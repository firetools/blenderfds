# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, Blender Operators.
"""

from . import (
    check_geom,
    choose_namelist_id,
    copy_params,
    gis,
    load_bf_settings,
    mesh_tools,
    scene_export,
    scene_import,
    show_fds_code,
    show_fds_geometry,
    show_ui,
    run_external,
    clean_ma_slots,
    update_addon,
)

ms_to_register = (
    scene_import,
    scene_export,
    check_geom,
    choose_namelist_id,
    copy_params,
    gis,
    load_bf_settings,
    mesh_tools,
    show_fds_code,
    show_fds_geometry,
    show_ui,
    run_external,
    clean_ma_slots,
    update_addon,
)


def register():
    for m in ms_to_register:
        m.register()


def unregister():
    for m in reversed(ms_to_register):
        m.unregister()
