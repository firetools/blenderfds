# BlenderFDS, an open tool for the NIST Fire Dynamics Simulator
# Copyright (C) 2013  Emanuele Gissi, http://www.blenderfds.org
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTIBILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.

bl_info = {
    "name": "BlenderFDS",
    "author": "Emanuele Gissi",
    "description": "BlenderFDS, an open graphical editor for the NIST Fire Dynamics Simulator",
    "blender": (2, 90, 1),
    "version": (5, 0, 0),
    "location": "File > Export > FDS Case (.fds)",
    "warning": "",
    "category": "Import-Export",
    "wiki_url": "http://www.blenderfds.org/",
    "tracker_url": "https://github.com/firetools/blenderfds/issues",
    "support": "COMMUNITY",
}


# Register

import bpy, logging

from . import ext
from .bl import operators, panels, menus, ui, handlers, preferences


logging.basicConfig(level=logging.INFO)
log = logging.getLogger(__name__)


def register():
    log.debug("Registering")
    # Preferences
    preferences.register()
    pref = bpy.context.preferences.addons[__package__].preferences
    # Set log level from preferences
    log.setLevel(pref.bf_loglevel)
    # Register Blender properties, ops, panels, ...
    ext.register()  # -> lang.py -> types.py
    operators.register()
    panels.register()
    menus.register()
    handlers.register()
    # Simplify UI, if preferred
    if pref.bf_pref_simplify_ui:
        ui.register()


def unregister():
    log.debug("Unregistering")
    # ui.unregister() # restart needed
    menus.unregister()
    panels.unregister()
    operators.unregister()
    ext.unregister()
    handlers.unregister()
    preferences.unregister()

