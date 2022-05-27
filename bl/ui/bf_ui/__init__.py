# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, classes for simplified ui.
"""

from . import properties, topbar, view3d

# Register/Unregister

ms_to_register = (
    properties,
    topbar,
    view3d,
)


def register():
    for m in ms_to_register:
        m.register()


def unregister():
    for m in ms_to_register:
        m.unregister()
