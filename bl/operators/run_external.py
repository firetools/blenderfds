# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, operators to run external commands (eg. fds, smokeview, ...).
"""

import os, sys, logging, csv
from datetime import datetime, timedelta

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


def get_datetime(timestring):
    # From FDS format: 2022-07-11T14:55:15.587+02:00
    return datetime.strptime(timestring[:-6], "%Y-%m-%dT%H:%M:%S.%f")


def get_timedelta(timestring1, timestring0):
    delta = get_datetime(timestring1) - get_datetime(timestring0)
    return delta.total_seconds()


class SCENE_OT_bf_eta_fds(Operator):
    """!
    Show estimated fds calc duration.
    """

    bl_label = "Estimate completion wall time"
    bl_idname = "scene.bf_eta_fds"
    bl_description = (
        "Estimate ongoing FDS calculation completion wall time,"
        "\nAs it is based on the most recent time step,"
        "\nwait until the simulated velocity field stabilises (eg. due"
        "\nto fire growth, ventilation) to obtain a dependable estimate"
    )

    @classmethod
    def poll(cls, context):
        return context.scene

    def execute(self, context):
        sc = context.scene
        # Prepare path and check file existence
        steps_filename = f"{sc.name}_steps.csv"
        try:
            steps_filepath = utils.io.transform_rbl_to_abs(
                context=context,
                filepath_rbl=sc.bf_config_directory,
                name=steps_filename,
            )
        except Exception as err:
            self.report({"ERROR"}, str(err))
            return {"CANCELLED"}

        if not utils.io.is_file(steps_filepath):
            self.report({"ERROR"}, f"<{steps_filename}> file not found, run FDS first!")
            return {"CANCELLED"}

        # Load the step file
        try:
            with open(steps_filepath, newline="") as csvfile:
                # Get rows
                rows = csv.reader(csvfile, delimiter=",")
                rows = tuple(r for r in rows)
                if len(rows) < 4:
                    self.report({"ERROR"}, "Calc just started, try again later")
                    return {"CANCELLED"}

                # Get timestep number
                nstep1 = float(rows[-1][0])
                nstep0 = float(rows[-2][0])

                # Get wall time delta in seconds
                wall_datetime1 = get_datetime(timestring=rows[-1][1])
                wall_datetime0 = get_datetime(timestring=rows[-2][1])
                wall_deltatime = (wall_datetime1 - wall_datetime0).total_seconds()

                # Get last time step sim duration, and calculated sim time
                step_size1 = float(rows[-1][2])
                sim_time1 = float(rows[-1][3])
        except Exception as err:
            self.report({"ERROR"}, f"Error reading <{steps_filename}>: {err}")
            return {"CANCELLED"}

        # Get TIME T_END
        if not sc.bf_time_export:
            self.report({"ERROR"}, "Set the TIME T_END parameter first")
            return {"CANCELLED"}
        t_end = sc.bf_time_t_end
        if t_end <= sim_time1:
            self.report({"INFO"}, "Calculation already completed")
            return {"CANCELLED"}

        # Calc the number of remaining timesteps
        nsteps = (t_end - sim_time1) / step_size1

        # Calc current wall time per timestep
        wall_time_per_step = wall_deltatime / (nstep1 - nstep0)

        # Calc remaining duration in seconds and final time
        duration_delta = timedelta(seconds=wall_time_per_step * nsteps)
        duration_delta_str = ""
        if duration_delta.seconds >= 3600:
            duration_delta_str = f", in {duration_delta.seconds/3600:.1f} hours"
        end_datetime = datetime.now() + duration_delta
        end_datetime_str = end_datetime.strftime("%B %d at %H:%M")

        # Close
        self.report(
            {"INFO"},
            f"If running, estimated completion time: {end_datetime_str}{duration_delta_str}",
        )
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
            self.report({"ERROR"}, "Run FDS, before opening Smokeview!")
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
    SCENE_OT_bf_eta_fds,
]


def register():
    from bpy.utils import register_class

    for c in bl_classes:
        register_class(c)


def unregister():
    from bpy.utils import unregister_class

    for c in reversed(bl_classes):
        unregister_class(c)
