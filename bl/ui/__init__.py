from . import simplify_ui
from .simplify_ui import toggle_simple_ui

ms_to_register = (simplify_ui,)


def register():
    for m in ms_to_register:
        m.register()


def unregister():
    for m in reversed(ms_to_register):
        m.unregister()
