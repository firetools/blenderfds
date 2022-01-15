from . import (
    check_geom,
    choose_namelist_id,
    copy_params,
    gis,
    load_bf_settings,
    mesh_tools,
    show_fds_code,
    show_fds_geometry,
)

mods = (
    check_geom,
    choose_namelist_id,
    copy_params,
    gis,
    load_bf_settings,
    mesh_tools,
    show_fds_code,
    show_fds_geometry,
)


def register():
    for m in mods:
        m.register()


def unregister():
    for m in reversed(mods):
        m.unregister()
