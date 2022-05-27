# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to load default settings.
"""

import os, sys, bpy, logging
from bpy.types import Operator

log = logging.getLogger(__name__)


class WM_OT_bf_load_blenderfds_settings(Operator):
    """!
    Load default BlenderFDS settings, deleting current data.
    """

    bl_label = "Load Default BlenderFDS Settings"
    bl_idname = "wm.bf_load_blenderfds_settings"
    bl_description = "Load default BlenderFDS settings, deleting current data!"

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        # Set default startup.blend
        filepath = os.path.join(
            os.path.dirname(sys.modules[__package__].__file__), "../../startup.blend"
        )
        bpy.ops.wm.open_mainfile(filepath=filepath, load_ui=True, use_scripts=True)
        bpy.ops.wm.save_homefile()
        # Load default commands
        bpy.ops.wm.bf_load_default_commands()
        # Save user preferences
        bpy.ops.wm.save_userpref()
        # Open new file (unlink startup)
        bpy.ops.wm.read_homefile()
        # Report
        self.report({"INFO"}, "Default settings loaded")
        return {"FINISHED"}


bl_classes = [
    WM_OT_bf_load_blenderfds_settings,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
