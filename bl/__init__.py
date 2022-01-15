from . import handlers, menus, operators, panels, preferences, ui_lists, ui

ms = (
    handlers,
    menus,
    operators,
    panels,
    preferences,
    ui_lists,
    ui,
)


def register():
    for m in ms:
        m.register()


def unregister():
    for m in ms:
        m.unregister()
