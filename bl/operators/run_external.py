# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to run external commands (eg. fds, smokeview, ...).
"""

import os, sys, bpy, logging
from bpy.types import Operator
from bpy.props import EnumProperty
from ... import config, utils

log = logging.getLogger(__name__)


class WM_OT_bf_restore_default_commands(Operator):
    """!
    Load default commands, deleting current data.
    """

    bl_label = "Restore Default Command"
    bl_idname = "wm.bf_restore_default_commands"
    bl_description = "Restore default commands for your platform"

    bf_command: EnumProperty(
        name="Restore Command",
        description="Select command to restore",
        items=[
            ("All", "All", "Restore all default commands for your platform"),
            ("FDS", "FDS", "Restore default FDS command for your platform"),
            (
                "Smokeview",
                "Smokeview",
                "Restore default Smokeview command for your platform",
            ),
            (
                "Terminal",
                "Terminal",
                "Restore default terminal command for your platform",
            ),
        ],
        default="All",
    )

    def invoke(self, context, event):
        # Ask for confirmation
        wm = context.window_manager
        return wm.invoke_confirm(self, event)

    def execute(self, context):
        prefs = context.preferences.addons[__package__.split(".")[0]].preferences
        platform = sys.platform
        if self.bf_command in ("All", "FDS"):
            prefs.bf_pref_fds_command = config.FDS_COMMAND.get(platform, "")
        if self.bf_command in ("All", "Smokeview"):
            prefs.bf_pref_smv_command = config.SMV_COMMAND.get(platform, "")
        if self.bf_command in ("All", "Terminal"):
            prefs.bf_pref_term_command = config.TERM_COMMAND.get(platform, "")
        # Report
        self.report({"INFO"}, "Default restored")
        return {"FINISHED"}


class SCENE_OT_bf_run_fds(Operator):
    """!
    Run FDS in a terminal.
    """

    bl_label = "Run FDS"
    bl_idname = "scene.bf_run_fds"
    bl_description = "Export current case and run FDS on it"

    @classmethod
    def poll(cls, context):
        return context.scene

    # def invoke(self, context, event):
    #     # Ask for confirmation
    #     wm = context.window_manager
    #     return wm.invoke_confirm(self, event)

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        sc = context.scene

        # Prepare path
        try:
            fds_filepath = utils.io.transform_rbl_to_abs(
                context=context,
                filepath_rbl=sc.bf_config_directory,
                name=sc.name,
                extension=".fds",
            )
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}
        fds_path, _ = utils.io.extract_path_name(fds_filepath)

        # Export the current case
        try:
            sc.to_fds(context, full=True, save=True)
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        # Prepare the command
        prefs = context.preferences.addons[__package__.split(".")[0]].preferences
        fds_command = prefs.bf_pref_fds_command
        term_command = prefs.bf_pref_term_command

        n = sc.bf_config_mpi_processes_export and sc.bf_config_mpi_processes or 1
        t = sc.bf_config_openmp_threads_export and sc.bf_config_openmp_threads or 1
        fds_command = (
            fds_command.replace("{n}", str(n))
            .replace("{t}", str(t))
            .replace("{f}", fds_filepath)
            .replace("{p}", fds_path)
        )
        command = term_command.replace("{c}", fds_command)

        # Run
        log.info(f"Run FDS:\n<{command}>")
        os.system(command)

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, "See the command prompt")
        return {"FINISHED"}


class SCENE_OT_bf_run_smv(Operator):
    """!
    Run Smokeview in a terminal.
    """

    bl_label = "Open Smokeview"
    bl_idname = "scene.bf_run_smv"
    bl_description = "Open Smokeview on the current FDS case"

    @classmethod
    def poll(cls, context):
        return context.scene

    def execute(self, context):
        w = context.window_manager.windows[0]
        w.cursor_modal_set("WAIT")
        sc = context.scene

        # Prepare path and check file existence
        try:
            smv_filepath = utils.io.transform_rbl_to_abs(
                context=context,
                filepath_rbl=sc.bf_config_directory,
                name=sc.name,
                extension=".smv",
            )
        except Exception as err:
            w.cursor_modal_restore()
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        smv_path, _ = utils.io.extract_path_name(smv_filepath)

        if not utils.io.is_file(smv_filepath):
            w.cursor_modal_restore()
            self.report({"ERROR"}, f"Run FDS, before opening Smokeview!")
            return {"CANCELLED"}

        # Prepare the command
        prefs = context.preferences.addons[__package__.split(".")[0]].preferences
        smv_command = prefs.bf_pref_smv_command
        term_command = prefs.bf_pref_term_command

        smv_command = smv_command.replace("{f}", smv_filepath).replace("{p}", smv_path)
        command = term_command.replace("{c}", smv_command)

        # Run
        log.info(f"Run Smokeview:\n<{command}>")
        os.system(command)

        # Close
        w.cursor_modal_restore()
        self.report({"INFO"}, "See the command prompt")
        return {"FINISHED"}


bl_classes = [
    WM_OT_bf_restore_default_commands,
    SCENE_OT_bf_run_fds,
    SCENE_OT_bf_run_smv,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
