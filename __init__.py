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
    "blender": (3, 2, 0),
    "version": (6, 0, 0),
    "location": "File > Export > FDS Case (.fds)",
    "warning": "",
    "category": "Import-Export",
    "doc_url": "http://www.blenderfds.org/",
    "tracker_url": "https://github.com/firetools/blenderfds/issues",
    "support": "COMMUNITY",
}

import logging

# Reading class definitions,
# bl should be imported before lang,
# because it imports ui_lists
from . import bl, lang

logging.basicConfig(level=logging.INFO)  # INFO or DEBUG
log = logging.getLogger(__name__)


# Automatic registering/deregistering
# of Blender entities
def register():
    log.info("Register BlenderFDS...")
    bl.register()
    lang.register()


def unregister():
    log.info("Unregister BlenderFDS...")
    lang.unregister()
    bl.unregister()
