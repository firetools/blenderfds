#!/usr/bin/env python3

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

"""!
Automatic verification script for continous integration.
"""

import os, sys, subprocess, csv, difflib, time
from datetime import datetime
import filecmp
import tempfile

# Config

BLENDER_PATHFILE = "/opt/blender/blender"

# Utils


class bcolors:
    HEADER = "\033[95m\033[1m"
    OKBLUE = "\033[94m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


def rm_empty_lines(string):
    return "\n".join([s for s in string.splitlines() if s.strip()])


# FDS


def _clean_fds(path, full=False):
    """!
    Clean all FDS runtime files from path.
    @param path: path to be cleaned.
    """
    extensions = [
        ".binfo",
        ".sinfo",
        ".end",
        ".out",
        ".s3d",
        ".s3d.sz",
        ".csv",
        "_git.txt",
        ".smv",
        ".ge",
        ".ge2",
        ".bf",
        ".bf.bnd",
        ".sf",
        ".sf.bnd",
        ".prt5",
        ".prt5.bnd",
    ]
    if full:
        extensions.extend((".fds", ".bingeom"))
    print(f"Cleaning <{path}> from FDS runtime files.")
    for file in os.scandir(path):
        n = file.name
        for e in extensions:
            if n.endswith(e):
                os.unlink(file.path)
                break


def run_fds_case(filepath):
    """!
    Execute FDS case
    @param filepath: FDS case filepath.
    @return tuple containing success flag and output log.
    """
    path, _ = os.path.split(filepath)
    c = subprocess.run(
        ["fds", os.path.abspath(filepath)],
        cwd=path,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        # timeout=60,
        check=True,
        encoding="utf-8",
    )
    success, log = False, rm_empty_lines(c.stdout)
    if log.find("STOP: FDS completed successfully") > 0:
        success, log = True, ""
    return success, log


def run_fds_cases(test, in_dir):
    """!
    Test: run fds on all fds cases
    @param test: running test (use test.name and test.path).
    @param in_dir: abs input directory containing fds files.
    """
    for f in os.scandir(in_dir):
        if f.name.endswith(".fds"):
            start_time = time.time()
            r = Result(test=test.name, desc=f"Run fds on <{f.name}>")
            r.success, r.log = run_fds_case(f.path)
            r.time = time.time() - start_time
            r.print()
    # Use a tmp dir as in_dir, instead of cleaning it


def _diff_fds_files(filepath1, filepath2):
    """!
    Compare two FDS files
    @param filepath: FDS case filepath.
    @return tuple containing equality flag and diff output.
    """
    if not os.path.isfile(filepath1):
        return False, f"<{filepath1}> does not exist"
    if not os.path.isfile(filepath2):
        return False, f"<{filepath2}> does not exist"
    success, log = True, str()
    with open(filepath1, "r") as f1, open(filepath2, "r") as f2:
        lines1 = f1.read().splitlines()
        lines2 = f2.read().splitlines()
    for l in difflib.unified_diff(lines1, lines2, n=0):
        if l[:3] in ("---", "+++", "@@ ") or l[1:7] in ("! Gene", "! Date", "! File",):
            continue
        log += f"\n{l}"
    if log:
        success = False
    return success, log


def _compare_binary_files(filepath1, filepath2):
    if filecmp.cmp(filepath1, filepath2):
        return True, None
    else:
        return (
            False,
            f"Binary files are different:\nfile1: {filepath1}\nfile2: {filepath2}",
        )


def compare_fds_files(test, in_dir, ref_dir="_ref"):
    """!
    Test: compare all fds files in ./export with relatives in ./ref
    @param test: running test (use test.name and test.path).
    @param in_dir: abs input directory containing fds files.
    @param ref_dir: relative directory containing reference files.
    """
    # FIXME compare number of files
    for f in os.scandir(in_dir):
        if f.name.endswith(".bingeom"):
            start_time = time.time()
            print(f"Compare <{f.name}> with ref")
            r = Result(test=test.name, desc=f"Compare <{f.name}> with ref")
            r.success, r.log = _compare_binary_files(
                filepath1=f.path, filepath2=os.path.join(test.path, ref_dir, f.name),
            )
            r.time = time.time() - start_time
            r.print()
        elif f.name.endswith(".fds"):
            start_time = time.time()
            print(f"Compare <{f.name}> with ref")
            r = Result(test=test.name, desc=f"Compare <{f.name}> with ref")
            r.success, r.log = _diff_fds_files(
                filepath1=f.path, filepath2=os.path.join(test.path, ref_dir, f.name),
            )
            r.time = time.time() - start_time
            r.print()


def open_blend_file(test, f):
    """!
    Test: open Blender file
    @param test: running test (use test.name and test.path)
    @param f: blend file (use f.name, and f.path)
    """
    r = Result(test=test.name, desc=f"Open <{f.name}>")
    start_time = time.time()
    try:
        bpy.ops.wm.open_mainfile(filepath=os.path.abspath(f.path))
    except Exception as err:
        r.time, r.success, r.log = time.time() - start_time, False, str(err)
        r.print()
    else:
        r.time, r.success = time.time() - start_time, True
        r.print()


# Library of tests


def export_to_fds(test, run_fds=False, exclude_files=None):
    """!
    Export all scenes from blend files in path (non recursive) to fds.
    @param test: running test (use test.name and test.path)
    @param path: root path containing blend files. If None use test.path.
    @param exclude_files: excluded files.
    """
    print(f"Exporting from <{test.path}>...")
    # Init
    exclude_files = exclude_files or tuple()
    # Run
    with tempfile.TemporaryDirectory() as tmpdir:
        for f in os.scandir(test.path):
            if (
                os.path.isfile(f)
                and f.name.endswith(".blend")
                and not f.name in exclude_files
            ):
                open_blend_file(test=test, f=f)
                # Export all scenes in current blend file
                for sc in bpy.data.scenes:
                    bpy.context.window.scene = sc
                    export_context_scene_to_fds(test=test, out_dir=tmpdir)
                # Compare results
                compare_fds_files(test=test, in_dir=tmpdir)
                # Run fds on them
                if run_fds:
                    run_fds_cases(test=test, in_dir=tmpdir)


def export_context_scene_to_fds(test, out_dir):  # Ok
    start_time = time.time()
    sc = bpy.context.scene
    path = os.path.abspath(test.path)
    bl_filepath = os.path.join(path, out_dir, sc.name + "_tmp.blend")
    fds_filepath = os.path.join(path, out_dir, sc.name + ".fds")
    desc = f"Export context Blender Scene <{sc.name}> to path <{fds_filepath}>"
    r = Result(test=test.name, desc=desc)
    # Save tmp blend file to set bpy.data.filepath
    bpy.ops.wm.save_as_mainfile(filepath=bl_filepath)
    # Export fds file
    try:
        bpy.ops.export_scene.fds(all_scenes=False, filepath=fds_filepath)
    except Exception as err:
        r.time, r.success, r.log = time.time() - start_time, False, str(err)
        r.print()
    else:
        r.time, r.success = time.time() - start_time, True
        r.print()


def import_from_fds_case(test, filepath):  # ok
    """!
    Import fds case at filepath into new Scene and set it to context.
    @param filepath: filepath containing fds case.
    """
    start_time = time.time()
    r = Result(test=test.name, desc=f"Import <{filepath}> to a new scene")
    # Open default blend file
    bpy.ops.wm.read_homefile()
    # Import to new scene
    try:
        bpy.ops.import_scene.fds(filepath=filepath, new_scene=True)
    except Exception as err:
        r.time, r.success, r.log = time.time() - start_time, False, str(err)
        r.print()
    else:
        # Get new scene, use filename as default name  # FIXME CHID?
        name = os.path.splitext(os.path.basename(filepath))[0]
        sc = bpy.data.scenes[name]
        # Set default scene as context.scene
        bpy.context.window.scene = sc
        # Unlink bingeoms from original files
        for ob in sc.objects:
            ob.bf_geom_binary_directory = str()
        r.time, r.success = time.time() - start_time, True
        r.print()
        return sc


def import_from_fds_tree(  # Ok
    test,
    path,
    compare_with_ref=False,
    run_fds=False,
    exclude_files=None,
    exclude_dirs=None,
):
    """!
    Import all fds cases in path recursively.
    @param test: running test (use test.name and test.path)
    @param path: root path containing fds cases. If None use test.path.
    @param exclude_files: excluded files.
    @param exclude_dirs: excluded dirs.
    """
    # Init and check
    if not path:
        path = test.path
    if not os.path.isdir(path):
        raise IOError(f"<{path}> is not a directory")
    exclude_files = exclude_files or list()
    exclude_dirs = exclude_dirs or list()
    # Run
    with tempfile.TemporaryDirectory() as tmpdir:
        for p, _, files in os.walk(path):  # recursive, sends filename
            # Exclude dirs
            dir_name = os.path.basename(os.path.dirname(p))
            if dir_name in exclude_dirs or dir_name.startswith("_"):
                continue
            # Loop on files
            for filename in files:
                if filename.endswith(".fds") and not filename in exclude_files:
                    filepath = os.path.join(os.path.abspath(p), filename)
                    # Import from fds
                    sc = import_from_fds_case(test=test, filepath=filepath)
                    if not sc:  # importing error?
                        continue
                    # Export context scene to fds
                    export_context_scene_to_fds(test=test, out_dir=tmpdir)
        # Compare all files with ref
        if compare_with_ref:
            compare_fds_files(test=test, in_dir=tmpdir)
        # Run fds on them
        if run_fds:
            run_fds_cases(test=test, in_dir=tmpdir)


# Test results management


class Results(list):
    def check_path_exists(self, path):
        if not os.path.exists(path):
            raise Exception(f"Path <{path}> does not exist!")

    def write_csv(self, options=None):
        if not options:
            options = "all"
        self.check_path_exists("results")
        filename = (
            f"results/{datetime.now().strftime('%Y%m%d_%H%M')}_{options}_results.csv"
        )
        fieldnames = Result.get_fieldnames()
        with open(file=filename, mode="w", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=fieldnames, quoting=csv.QUOTE_ALL)
            writer.writeheader()
            for r in self:
                writer.writerow(r.get_row())

    def write_txt(self, options=None, full=False):
        if not options:
            options = "all"
        self.check_path_exists("results")
        filename = (
            f"results/{datetime.now().strftime('%Y%m%d_%H%M')}_{options}_failures.txt"
        )
        with open(file=filename, mode="w", newline="", encoding="utf-8") as f:
            for r in self:
                if full or not r.success:
                    f.write(str(r) + "\n")

    def print_success(self, options=None):
        if not options:
            options = "all"
        # Overall
        ntest = len(self)
        nsuccess = len(tuple(r for r in self if r.success))
        nfail = ntest - nsuccess
        print(f"{bcolors.HEADER}\nTest results:{bcolors.ENDC}")
        print(
            f"{ntest:4d} done = {bcolors.OKGREEN}{nsuccess:4d} ok{bcolors.ENDC} + {bcolors.FAIL}{nfail:4d} fail{bcolors.ENDC} : all tests"
        )
        # Per test
        test_types = set(r.test for r in self)
        for ty in test_types:
            tests = tuple(r for r in self if r.test == ty)
            ntest = len(tests)
            nsuccess = len(tuple(r for r in tests if r.success))
            nfail = ntest - nsuccess
            print(
                f"{ntest:4d} done = {bcolors.OKGREEN}{nsuccess:4d} ok{bcolors.ENDC} + {bcolors.FAIL}{nfail:4d} fail{bcolors.ENDC} : {ty}"
            )


results = Results()


class Result:
    def __init__(self, test, desc, success=None, log=None, time=0.0):
        results.append(self)  # automatic append at creation
        self.test = test
        self.desc = desc
        self.success = success
        self.log = log or str()
        self.time = time
        print(f"[START] {self.test}: {self.desc}")

    def __str__(self):
        if self.success:
            return f"[OK] {self.test}: {self.desc}: {self.time:.2f} s"
        else:
            return (
                f"[FAIL] {self.test}: {self.desc}: {self.time:.2f} s\n{self.log}\n---"
            )

    def print(self):
        if self.success:
            print(
                f"{bcolors.OKGREEN}[OK] {self.test}: {self.desc}: {self.time:.2f} s{bcolors.ENDC}"
            )
        else:
            print(
                f"{bcolors.FAIL}[FAIL] {self.test}: {self.desc}: {self.time:.2f} s{bcolors.ENDC}\n{self.log}\n---"
            )

    @classmethod
    def get_fieldnames(cls):
        return ["success", "time", "test", "desc"]

    def get_row(self):
        return {
            "success": self.success and "OK" or "FAIL",
            "time": self.time,
            "test": self.test,
            "desc": self.desc,
        }


# Main


def exec_tests(options):
    """!
    Execute tests in current directory.
    """
    # Treat command line options
    print("User options:", options)
    # Exec tests in test directories
    for test in os.scandir():
        # Check
        if (
            not test.is_dir()
            or not test.name.startswith("test")
            or os.path.isfile(f"{test.path}/skip")
            or (options and (test.name not in options))  # test only user choices
        ):
            continue
        if not os.path.isfile(f"{test.path}/test.py"):
            raise AssertionError(f"No <test.py> script available in <{test.name}>")
        # Exec test
        print(f"{bcolors.HEADER}Executing test <{test.name}>{bcolors.ENDC}")
        exec(
            open(f"{test.path}/test.py").read(), globals(), {"test": test},
        )
    # Write
    if not options:
        options = ("all",)
    results.write_csv(options[0])
    results.write_txt(options[0])
    results.print_success(options[0])


if __name__ == "__main__":
    """!
    Launch current script in Blender.
    """
    # Check if Blender is already running
    try:
        import bpy
    except ModuleNotFoundError:
        first_run = True
    else:
        first_run = False
    # First run, launch myself in Blender
    if first_run:
        print(f"{bcolors.HEADER}Launch Blender...{bcolors.ENDC}")
        options = sys.argv[1:]  # pass user options
        process = [
            BLENDER_PATHFILE,
            "--background",
            "--python",
            os.path.abspath(__file__),
            "--",  # allow options
        ]
        process.extend(options)
        try:
            subprocess.run(
                process,
                # timeout=3600,
                check=True,
            )
        except subprocess.CalledProcessError as err:
            print(
                f"{bcolors.FAIL}Running verify.py in Blender returned an error.{bcolors.ENDC}\n{err}"
            )
    # Second run, now run myself in Blender
    else:
        print(f"{bcolors.HEADER}Executing tests...{bcolors.ENDC}")
        # Test if BlenderFDS is correctly loaded
        try:
            bpy.context.scene.bf_config_directory
        except:
            raise ModuleNotFoundError("BlenderFDS addon is not loaded.")
        else:
            print(f"{bcolors.OKGREEN}BlenderFDS is loaded.{bcolors.ENDC}")
        # Run tests
        print(f"{bcolors.HEADER}Executing tests in Blender...{bcolors.ENDC}")
        options = sys.argv[5:]  # pass user options
        exec_tests(options)
        print(f"{bcolors.HEADER}\nCompleted.{bcolors.ENDC}")
