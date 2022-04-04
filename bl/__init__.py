from . import handlers, menus, operators, panels, preferences, ui_lists, ui

ms_to_register = (
    handlers,
    menus,
    operators,
    panels,
    preferences,
    ui_lists,
    ui,
)


def register():
    for m in ms_to_register:
        m.register()


def unregister():
    for m in ms_to_register:
        m.unregister()
