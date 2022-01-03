from . import handlers, menus, operators, panels, preferences, ui_lists, ui


def register():
    handlers.register()
    menus.register()
    operators.register()
    panels.register()
    preferences.register()
    ui_lists.register()
    ui.register()


def unregister():
    handlers.unregister()
    menus.unregister()
    operators.unregister()
    panels.unregister()
    preferences.unregister()
    ui_lists.unregister()
    ui.unregister()
