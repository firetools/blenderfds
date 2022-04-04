from . import (
    check_geom,
    choose_namelist_id,
    copy_params,
    gis,
    load_bf_settings,
    mesh_tools,
    show_fds_code,
    show_fds_geometry,
    show_ui,
)

ms_to_register = (
    check_geom,
    choose_namelist_id,
    copy_params,
    gis,
    load_bf_settings,
    mesh_tools,
    show_fds_code,
    show_fds_geometry,
    show_ui,
)


def register():
    for m in ms_to_register:
        m.register()


def unregister():
    for m in reversed(ms_to_register):
        m.unregister()
