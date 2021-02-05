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
BlenderFDS, extension of Blender types.
"""

import bpy
from bpy.types import Material, Scene, Object, Collection

import time, sys, logging

from . import lang, io, fds
from .lang import bf_namelists_by_cls, bf_namelists_by_fds_label
from .types import FDSCase
from .utils import BFException, BFNotImported

log = logging.getLogger(__name__)

# bf_namelist: bf_params -> fds_param
# material: bf_namelist or None -> fds_namelist or None -> line
# object: bf_namelist or None -> fds_namelist or None -> line
# collection: object -> line
# scene:
#   bf_namelist or None -> fds_namelist or None -> line
#   material or None -> fds_namelist or None -> line
#   collection -> fds_namelist or None -> line


class BFMaterial:
    """!
    Extension of Blender Material.
    """

    @property
    def bf_namelist(self):
        """!
        Return related bf_namelist, instance of BFNamelist.
        """
        return bf_namelists_by_cls[self.bf_namelist_cls](self)

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return None or FDS formatted string, eg. "&OBST ID='Test' /".
        """
        return self.bf_namelist.to_fds(context)

    def from_fds(self, context, fds_namelist):
        """!
        Set self.bf_namelist from FDSNamelist, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: FDSNamelist.
        """
        self.bf_namelist_cls = "MN_SURF"
        self.bf_namelist.from_fds(context, fds_namelist=fds_namelist)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Material.bf_namelist = cls.bf_namelist
        Material.to_fds = cls.to_fds
        Material.from_fds = cls.from_fds

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Material.from_fds
        del Material.to_fds
        del Material.bf_namelist


class BFObject:
    """!
    Extension of Blender Object.
    """

    @property
    def bf_namelist(self):
        """!
        Related bf_namelist, instance of BFNamelist.
        """
        return bf_namelists_by_cls[self.bf_namelist_cls](self)

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        if self.hide_render or self.bf_is_tmp or not self.type == "MESH":
            return
        lines = list()
        # Inject my MOVE namelist
        if self.bf_move_id_export:
            lines.append(
                fds.move_tools.from_matrix(  # TODO cleanup
                    hid=self.bf_move_id or self.name + "_move",
                    matrix=self.matrix_world,
                ).to_fds(context)
            )
        # Add my bf_namelist (never None)
        lines.append(self.bf_namelist.to_fds(context))
        return "\n".join(l for l in lines if l)

    def from_fds(self, context, fds_namelist):
        """!
        Set self.bf_namelist from FDSNamelist, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: FDSNamelist.
        """
        # Set bf_namelist_cls
        bf_namelist = bf_namelists_by_fds_label.get(fds_namelist.fds_label)
        self.bf_namelist_cls = bf_namelist.__name__
        # Prevent default geometry (eg. XB=BBOX)
        self.bf_xb_export, self.bf_xyz_export, self.bf_pb_export = (False, False, False)
        # Import
        self.bf_namelist.from_fds(context, fds_namelist=fds_namelist)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Object.bf_namelist = cls.bf_namelist
        Object.to_fds = cls.to_fds
        Object.from_fds = cls.from_fds

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Object.from_fds
        del Object.to_fds
        del Object.bf_namelist


class BFCollection:
    """!
    Extension of Blender Collection.
    """

    def get_layer_collection(self, context, _layer_collection=None):
        """!
        Return related layer_collection in current context.
        @param context: the Blender context.
        @param _layer_collection: internal use for recursivity.
        @return layer_collection related to self in current context.
        """
        if not _layer_collection:
            _layer_collection = context.view_layer.layer_collection
        found = None
        if _layer_collection.name == self.name:
            return _layer_collection
        for c in _layer_collection.children:
            found = self.get_layer_collection(context, _layer_collection=c)
            if found:
                return found

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string, eg. "&OBST ID='Test' /".
        """
        layer_collection = self.get_layer_collection(context)
        if self.hide_render or layer_collection.exclude:
            return  # exclude from exporting
        obs = list(self.objects)
        obs.sort(key=lambda k: k.name)  # alphabetic by name
        lines = list(ob.to_fds(context) for ob in obs)
        lines.extend(child.to_fds(context) for child in self.children)
        if lines:
            header = f"! --- Geometric namelists from Blender Collection <{self.name}>"
            lines.insert(0, header)
        return "\n".join(l for l in lines if l)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Collection.to_fds = cls.to_fds
        Collection.get_layer_collection = cls.get_layer_collection

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Collection.to_fds
        del Collection.get_layer_collection


class BFScene:
    """!
    Extension of Blender Scene.
    """

    @property
    def bf_namelists(self):
        """!
        Return related bf_namelist, instance of BFNamelist.
        """
        return (n(self) for _, n in bf_namelists_by_cls.items() if n.bpy_type == Scene)

    def to_fds(self, context, full=False, all_surfs=False, filepath=None):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @param full: if True, return full FDS case.
        @param all_surfs: if True, return all SURF namelists, even if not related.
        @param filepath: filepath of FDS case to be exported.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        lines = list()
        # Set mysef as the right Scene instance in the context
        bpy.context.window.scene = self
        # Header
        if full:
            v = sys.modules[__package__].bl_info["version"]
            blv = bpy.app.version_string
            now = time.strftime("%a, %d %b %Y, %H:%M:%S", time.localtime())
            blend_filepath = bpy.data.filepath or "not saved"
            if len(blend_filepath) > 60:
                blend_filepath = "..." + blend_filepath[-57:]
            lines.extend(
                (
                    f"! Generated by BlenderFDS {v[0]}.{v[1]}.{v[2]} on Blender {blv}",
                    f"! Date: <{now}>",
                    f"! File: <{blend_filepath}>",
                    f"! --- Case from Blender Scene <{self.name}>",
                )
            )
        # Scene namelists
        lines.extend(
            bf_namelist.to_fds(context)
            for bf_namelist in self.bf_namelists
            if bf_namelist
        )
        # Free Text
        if self.bf_config_text:
            text = self.bf_config_text.as_string()
            if text:
                text = f"\n! --- Free text from Blender Text <{self.bf_config_text.name}>\n{text}"
                lines.append(text)
        # End if limited to Scene
        if not full:
            return "\n".join(l for l in lines if l)
        # Material namelists
        if all_surfs:  # all SURFs exported
            mas = list(bpy.data.materials)
            header = "\n! --- Boundary conditions from all Blender Materials"
        else:  # only related SURFs exported
            mas = list(
                set(
                    ms.material
                    for ob in self.objects
                    for ms in ob.material_slots
                    if ms.material  # not empty
                )
            )
            header = "\n! --- Boundary conditions from related Blender Materials"
        mas.sort(key=lambda k: k.name)  # alphabetic sorting by name
        ma_lines = list(ma.to_fds(context) for ma in mas)
        if any(ma_lines):
            lines.append(header)
            lines.extend(ma_lines)
        # Objects from collections
        lines.append(" ")
        lines.append(self.collection.to_fds(context))
        # TAIL
        if self.bf_head_export:
            lines.append("\n&TAIL /\n ")
        # Write
        fds_text = "\n".join(l for l in lines if l)
        if filepath:
            io.write_txt_file(filepath, fds_text)
        else:
            return fds_text

    def from_fds(self, context, filepath=None, f90=None):
        """!
        Set self.bf_namelists from FDSCase, on error raise BFException.
        @param context: the Blender context.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        # Set mysef as the right Scene instance in the context
        bpy.context.window.scene = self
        # Init
        if filepath and not f90:
            fds_case = FDSCase(filepath=filepath)
        elif f90 and not filepath:
            fds_case = FDSCase(f90=f90)
        else:
            raise AssertionError("Either filepath or f90 should be set")
        # Prepare free text for unmanaged namelists
        te = bpy.data.texts.new(f"Imported_text")
        self.bf_config_text = te
        # Import SURFs first to new materials
        while True:
            fds_namelist = fds_case.get_by_label(fds_label="SURF", remove=True)
            if not fds_namelist:
                break
            hid = "Imported_SURF"
            ma = bpy.data.materials.new(hid)
            ma.from_fds(context, fds_namelist=fds_namelist)
            ma.use_fake_user = True  # prevent del (eg. used by PART)
        # Import all MOVE namelist
        move_id_to_matrix = dict()
        while True:
            fds_namelist = fds_case.get_by_label(fds_label="MOVE", remove=True)
            if not fds_namelist:
                break
            hid, matrix = fds.move_tools.to_matrix(fds_namelist)
            move_id_to_matrix[hid] = matrix
        # Import OBSTs before VENTs  # TODO generic!
        while True:
            fds_namelist = fds_case.get_by_label(fds_label="OBST", remove=True)
            if not fds_namelist:
                break
            hid = "Imported_OBST"
            me = bpy.data.meshes.new(hid)
            ob = bpy.data.objects.new(hid, object_data=me)
            self.collection.objects.link(ob)
            ob.from_fds(context, fds_namelist=fds_namelist)
        # Then the rest
        fds_case.fds_namelists.reverse()
        while fds_case.fds_namelists:
            fds_namelist = fds_case.fds_namelists.pop()
            imported = False
            # Import to managed BFNamelist
            bf_namelist = bf_namelists_by_fds_label.get(fds_namelist.fds_label, None)
            if bf_namelist:
                hid = f"Imported_{fds_namelist.fds_label}"
                # Into new Object
                if bf_namelist.bpy_type == Object:
                    me = bpy.data.meshes.new(hid)
                    ob = bpy.data.objects.new(hid, object_data=me)
                    self.collection.objects.link(ob)
                    try:
                        ob.from_fds(context, fds_namelist=fds_namelist)
                    except BFNotImported:
                        bpy.data.objects.remove(ob, do_unlink=True)
                    else:
                        imported = True
                # Into current Scene
                elif bf_namelist.bpy_type == Scene:  # in current Scene
                    try:
                        bf_namelist(self).from_fds(context, fds_namelist=fds_namelist)
                    except BFNotImported:
                        pass
                    else:
                        imported = True
                # Unhandled
                else:
                    raise AssertionError(f"Unhandled bf_namelist for <{fds_namelist}>")
            # Import to Free Text
            if not imported:
                te.write(fds_namelist.to_fds(context) + "\n")
        # Transform those Objects that have a MOVE_ID
        for ob in self.collection.objects:
            if ob.bf_move_id_export and ob.bf_move_id:
                try:
                    ob.matrix_world = move_id_to_matrix[ob.bf_move_id] @ ob.matrix_world
                except KeyError:
                    raise BFException(
                        self,
                        f"Unknown MOVE namelist <{ob.bf_move_id}> called by Object <{ob.name}>",
                    )
        # Set imported Scene visibility
        context.window.scene = self
        # Show free text
        te.current_line_index = 0
        bpy.ops.scene.bf_show_text()

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Scene.bf_namelists = cls.bf_namelists
        Scene.to_fds = cls.to_fds
        Scene.from_fds = cls.from_fds

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Scene.from_fds
        del Scene.to_fds
        del Scene.bf_namelists


def register():
    """!
    Register Blender classes.
    """
    log.debug(f"Registering Blender extensions")
    # Register lang
    lang.register()
    # Register extensions to Blender types
    BFObject.register()
    BFMaterial.register()
    BFScene.register()
    BFCollection.register()


def unregister():
    """!
    Unregister Blender classes.
    """
    log.debug(f"Unregistering Blender extensions")
    # Unregister extensions to Blender types
    BFObject.unregister()
    BFMaterial.unregister()
    BFScene.unregister()
    BFCollection.unregister()
    # Unregister lang
    lang.unregister()
