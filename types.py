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
BlenderFDS, Blender to FDS interface metaclasses.
"""

import re, os.path, logging

import bpy
from bpy.types import PropertyGroup, UIList, Object, Scene, Material
from bpy.props import (
    BoolProperty,
    FloatProperty,
    IntProperty,
    StringProperty,
    PointerProperty,
    EnumProperty,
    CollectionProperty,
)

if __name__ != "__main__":
    from .bl import custom_uilist
    from . import io
    from .utils import BFException, BFNotImported, is_iterable

log = logging.getLogger(__name__)


# Blender representations of FDS entities


class BFParam:
    """!
    Blender representation of an FDS parameter.
    """

    ## Object label
    label = "No Label"
    ## Object description
    description = None
    ## Unique integer id for EnumProperty
    enum_id = None
    ## Other BlenderFDS parameters, eg: {'draw_type': 'WIRE', ...}
    bf_other = {}
    ## My sub params, tuple of classes of type BFParam
    bf_params = tuple()
    ## FDS label, eg. "OBST", "ID", ...
    fds_label = None
    ## FDS default value
    fds_default = None
    ## type in bpy.types for Blender property, eg. Object
    bpy_type = None
    ## idname of related bpy.types Blender property, eg. "bf_id"
    bpy_idname = None
    ## prop in bpy.props of Blender property, eg. StringProperty
    bpy_prop = None
    ## Blender property default
    bpy_default = None
    ## Other optional Blender property parameters, eg. {"min": 3., ...}
    bpy_other = {}
    ## idname of export toggle Blender property
    bpy_export = None
    ## default value for export toggle Blender property
    bpy_export_default = None

    ## List of registered bpy_idname for unregistering
    _registered_bpy_idnames = list()

    def __init__(self, element):
        """!
        Class constructor.
        @param element: FDS element represented by this class instance.
        """
        ## FDS element represented by this class instance
        self.element = element

    def __str__(self):
        return f"{self.label}(element='{self.element.name}')"

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        # log.debug(f"Registering <{cls.label}>")
        if not cls.bpy_type:
            raise AssertionError(f"No bpy_type in class <{cls}>")
        # Insert fds_default
        if cls.fds_default is not None:
            # ...in description
            cls.description += f"\n(FDS default: {cls.fds_default})"
            # ...in bpy_default if not present
            if cls.bpy_default is None:
                cls.bpy_default = cls.fds_default
        # Create bpy_prop
        if cls.bpy_prop:
            if not cls.bpy_idname or not cls.label or not cls.description:
                raise AssertionError(
                    f"No bpy_idname, label or description in class <{cls}>"
                )
            bpy_other = cls.bpy_other.copy()
            if cls.bpy_default is not None:
                bpy_other["default"] = cls.bpy_default
            # log.debug(f"Setting <{cls.bpy_idname}> Blender property")
            setattr(
                cls.bpy_type,
                cls.bpy_idname,
                cls.bpy_prop(name=cls.label, description=cls.description, **bpy_other),
            )
            cls._registered_bpy_idnames.append(cls.bpy_idname)
        # Create bpy_export
        if cls.bpy_export:
            if hasattr(cls.bpy_type, cls.bpy_export):
                # log.debug(f"Using <{cls.bpy_export}> Blender property as export ref")
                if cls.bpy_export_default is not None:
                    msg = f"Unused bpy_export_default in class <{cls.__name__}>"
                    raise AssertionError(msg)
            else:
                # log.debug(f"Setting <{cls.bpy_export}> Blender property")
                if cls.bpy_export_default is None:
                    msg = f"Undefined bpy_export_default in class <{cls.__name__}>"
                    raise AssertionError(msg)
                setattr(
                    cls.bpy_type,
                    cls.bpy_export,
                    BoolProperty(
                        name=f"Export {cls.label}",
                        description=f"Set if {cls.label} shall be exported to FDS",
                        default=cls.bpy_export_default,
                    ),
                )
                cls._registered_bpy_idnames.append(cls.bpy_export)

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be registered.
        """
        for bpy_idname in cls._registered_bpy_idnames:
            if not hasattr(cls.bpy_type, bpy_idname):  # already deleted?
                continue
            # log.debug(f"Unregistering <{bpy_idname}> Blender property")
            delattr(cls.bpy_type, bpy_idname)

    @property
    def value(self):
        """!
        Return value from element instance.
        @return any type
        """
        return getattr(self.element, self.bpy_idname)

    def set_value(self, context, value=None):
        """!
        Set element instance value. If value is None, set default value.
        @param context: the Blender context.
        @param value: value to set.
        """
        if value is None:
            setattr(self.element, self.bpy_idname, self.bpy_default)
            return
        else:
            setattr(self.element, self.bpy_idname, value)
            return

    @property
    def exported(self):
        """!
        Return True if self is exported to FDS.
        """
        # Check if empty
        value = self.value
        if value is None or value == "":
            return False
        # Check if identical to FDS default
        d = self.fds_default
        if d is not None:
            if isinstance(value, float):  # floats comparison
                return value > d + 1e-6 or value < d - 1e-6
            elif value == d:  # other comparison
                return False
        # Check if bpy_export is True
        if self.bpy_export is None:
            return True
        return bool(getattr(self.element, self.bpy_export, True))

    def set_exported(self, context, value=None):
        """!
        Set if self is exported to FDS. If value is None, set default value.
        @param context: the Blender context.
        @param value: value to set.
        """
        if self.bpy_export is None:
            if not value:
                msg = f"Cannot set self.exported = False in <{self}>"
                raise AssertionError(msg)
        else:
            if value is None:
                setattr(self.element, self.bpy_export, self.bpy_export_default)
            else:
                setattr(self.element, self.bpy_export, value)

    def check(self, context):
        """!
        Check self validity for FDS, in case of error raise BFException.
        @param context: the Blender context.
        """
        pass

    def draw_operators(self, context, layout):
        """!
        Draw my operators on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
        """
        pass

    def draw(self, context, layout):
        """!
        Draw my UI on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
        """
        if not self.bpy_idname:
            return
        # Set active and alert
        active, alert = bool(self.exported), False
        if active:
            try:
                self.check(context)
            except BFException:
                alert = True
        # Set layout
        if self.bpy_export:
            col = layout.column(align=False, heading=self.label)
            row = col.row(align=True)
            row.active, row.alert = active, alert
            row.prop(self.element, self.bpy_export, text="")
            row.prop(self.element, self.bpy_idname, text="")
        else:
            col = layout.column()
            row = col.row(align=True)
            row.active, row.alert = active, alert
            row.prop(self.element, self.bpy_idname, text=self.label)
        self.draw_operators(context, row)  # along the properties
        return col

    def to_fds_param(self, context):
        """!
        Return the FDSParam representation of element instance.
        @param context: the Blender context.
        @return None, FDSParam, (FDSParam, ...) called "many", or ((FDSParam, ...), ...) called "multi" instances.
        """
        if self.exported:
            self.check(context)
            if self.fds_label:
                return FDSParam(
                    fds_label=self.fds_label,
                    value=self.value,
                    precision=self.bpy_other.get("precision", 3),
                )

    # No to_fds, because to_fds_param can be None, many or multi
    # and it makes no sense

    def from_fds(self, context, value):
        """!
        Set self.value from py value, on error raise BFException.
        @param context: the Blender context.
        @param value: the value to set. Can be of any type.
        """
        self.set_value(context, value)
        self.set_exported(context, True)

    def copy_to(self, dest_element):
        """!
        Copy self values to destination element.
        @param dest_element: element of the same type of self.element.
        """
        log.debug(f"  Copying <{self}> to <{dest_element.name}>")
        if self.bpy_export:
            value = getattr(self.element, self.bpy_export)
            setattr(dest_element, self.bpy_export, value)
        if self.bpy_idname:
            value = getattr(self.element, self.bpy_idname)
            setattr(dest_element, self.bpy_idname, value)


class BFParamXB(BFParam):
    """!
    Blender representation of an FDS parameter, helper for FDS XB parameter.
    """

    pass


class BFParamXYZ(BFParam):
    """!
    Blender representation of an FDS parameter, helper for FDS XYZ parameter.
    """

    pass


class BFParamPB(BFParam):
    """!
    Blender representation of an FDS parameter, helper for FDS PBX PBY PBZ parameters.
    """

    pass


class BFParamStr(BFParam):
    """!
    Blender representation of an FDS parameter, helper for any FDS string parameter.
    """

    bpy_prop = StringProperty

    # def check(self, context):  # No check
    #     value = self.value
    #     if "&" in value or "/" in value or "#" in value:
    #         raise BFException(self, "<&>, </>, and <#> characters not allowed")
    #     # if (
    #     #     "'" in value
    #     #     or '"' in value
    #     #     or "`" in value
    #     #     or "“" in value
    #     #     or "”" in value
    #     #     or "‘" in value
    #     #     or "’‌" in value
    #     # ):
    #     #     raise BFException(self, "Quote characters not allowed")


class BFParamFYI(BFParamStr):
    """!
    Blender representation of an FDS parameter, helper for FDS FYI parameter.
    """

    label = "FYI"
    description = "For your information"
    fds_label = "FYI"
    bpy_idname = "bf_fyi"
    bpy_prop = StringProperty
    bpy_other = {"maxlen": 128}

    def draw(self, context, layout):
        col = layout.column()
        try:
            self.check(context)
        except BFException:
            col.alert = True
        if self.bpy_idname:
            col.prop(self.element, self.bpy_idname, text="", icon="INFO")
        return col


class BFParamOther(BFParam):
    """!
    Blender representation of any FDS parameter, helper for the 'other' FDS parameters.
    """

    label = "Other Parameters"
    description = "Other parameters (eg. PROP='Example')"
    bpy_type = Object  # example
    bpy_idname = "bf_other"

    bpy_pg = None  # PropertyGroup, eg. WM_PG_bf_other
    bpy_ul = None  # UIList, eg. WM_UL_bf_other_items

    @classmethod
    def register(cls):
        cls._ops = custom_uilist.register_collection(
            bpy_type=cls.bpy_type,
            bpy_idname=cls.bpy_idname,
            name=cls.label,
            bpy_pg=cls.bpy_pg,
            description=cls.description,
        )

    @classmethod
    def unregister(cls):
        custom_uilist.unregister_collection(
            bpy_type=cls.bpy_type, bpy_idname=cls.bpy_idname, ops=cls._ops
        )

    @property
    def value(self):
        collection = getattr(self.element, self.bpy_idname)
        return tuple(item.name for item in collection if item.bf_export)

    def set_value(self, context, value):
        collection = getattr(self.element, self.bpy_idname)
        if value is None:
            collection.clear()
        else:
            item = collection.add()
            item.name, item.bf_export = value, True

    def draw(self, context, layout):
        custom_uilist.draw_collection(
            element=self.element,
            context=context,
            layout=layout,
            bpy_type=self.bpy_type,
            bpy_idname=self.bpy_idname,
            name=self.label,
            bpy_ul=self.bpy_ul,
        )

    def to_fds_param(self, context):
        self.check(context)
        coll = getattr(self.element, self.bpy_idname)
        if coll:
            return tuple(
                (FDSParam(fds_label=p.name) for p in coll if p.bf_export and p.name)
            )  # many

    def copy_to(self, dest_element):
        log.debug(f"  Copying <{self}> to <{dest_element.name}>")
        if self.bpy_export:
            value = getattr(self.element, self.bpy_export)
            setattr(dest_element, self.bpy_export, value)
        # Clear destination collection
        dest_collection = getattr(dest_element, self.bpy_idname)
        dest_collection.clear()
        # Copy collection values
        collection = getattr(self.element, self.bpy_idname)
        for item in collection:
            dest_item = dest_collection.add()
            dest_item.name, dest_item.bf_export = item.name, item.bf_export


# BFNamelist is a special BFParam


class BFNamelist(BFParam):
    """!
    Blender representation of an FDS namelist group.
    """

    def __init__(self, element):
        ## FDS element represented by this class instance
        self.element = element
        ## My sub params, tuple of element instances of type BFParam
        self.bf_params = tuple(p(element) for p in self.bf_params)

    # inherits __str__

    @classmethod
    def register(cls):
        super().register()
        # Indexes are used to link both the class and the instance
        cls._bf_param_idx_by_fds_label = dict()  # fds_label: index of param
        cls._bf_param_xb_idx = None  # index of param of type BFParamXB
        cls._bf_param_xyz_idx = None  # ... of type BFParamXYZ
        cls._bf_param_pb_idx = None  # ... of type BFParamPB
        cls._bf_param_other_idx = None  # ... of type BFParamOther
        for i, p in enumerate(cls.bf_params):
            if p.fds_label:
                cls._bf_param_idx_by_fds_label[p.fds_label] = i
            if issubclass(p, BFParamXB):
                cls._bf_param_xb_idx = i
            elif issubclass(p, BFParamXYZ):
                cls._bf_param_xyz_idx = i
            elif issubclass(p, BFParamPB):
                cls._bf_param_pb_idx = i
            elif issubclass(p, BFParamOther):
                cls._bf_param_other_idx = i

    def get_by_label(self, fds_label):
        """!
        Return bf_param (class or instance) by its fds_label.
        @param fds_label: FDS parameter to be obtained.
        @return BFParam or None.
        """
        i = self._bf_param_idx_by_fds_label.get(fds_label)
        if i is not None:
            return self.bf_params[i]

    @property
    def bf_param_xb(self):
        """!
        Return the reference of the XB bf_param (BFParamXB class or instance).
        """
        if self._bf_param_xb_idx is not None:
            return self.bf_params[self._bf_param_xb_idx]

    @property
    def bf_param_xyz(self):
        """!
        Return the reference of the XYZ bf_param (BFParamXYZ class or instance).
        """
        if self._bf_param_xyz_idx is not None:
            return self.bf_params[self._bf_param_xyz_idx]

    @property
    def bf_param_pb(self):
        """!
        Return the reference of the PB bf_param (BFParamPB class or instance).
        """
        if self._bf_param_pb_idx is not None:
            return self.bf_params[self._bf_param_pb_idx]

    @property
    def bf_param_other(self):
        """!
        Return the reference of the other bf_param (class or instance).
        """
        if self._bf_param_other_idx is not None:
            return self.bf_params[self._bf_param_other_idx]

    @property
    def exported(self):
        if self.bpy_export is None:
            return True
        return bool(getattr(self.element, self.bpy_export, True))

    def draw_operators(self, context, layout):
        """!
        Draw my operators on Tools panel.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
        """
        layout.label(text="No tool available.")

    def draw(self, context, layout):
        """!
        Draw my UI on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @return used layout.
        """
        # Check and active
        try:
            self.check(context)
        except BFException:
            layout.alert = True
        layout.active = self.exported
        # Parameters
        col = layout.column()
        for p in self.bf_params:
            p.draw(context, col)
        return col

    def set_appearance(self, context):
        """!
        Set the default appearance of self.element in Blender.
        """
        # Check appearance preference
        if context:
            prefs = context.preferences.addons[__package__].preferences
            if not prefs.bf_pref_appearance:
                return
        # Set appearance of self.element in subclasses

    def to_fds_namelist(self, context):
        """!
        Return the FDSNamelist representation of element instance.
        @param context: the Blender context.
        @return None, FDSNamelist, or (FDSNamelist, ...) instances.
        """
        # Get if exported and check
        if not self.exported or not self.fds_label:
            return
        self.check(context)
        # Assemble from bf_params, protect from None
        return FDSNamelist(
            fds_label=self.fds_label,
            fds_params=list(p.to_fds_param(context) for p in self.bf_params if p),
        )

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        fds_namelist = self.to_fds_namelist(context)
        if fds_namelist:
            if isinstance(fds_namelist, FDSNamelist):
                return fds_namelist.to_fds(context)
            else:
                return "\n".join(line.to_fds(context) for line in fds_namelist if line)

    def from_fds(self, context, fds_namelist):
        """!
        Set self.bf_params value, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: instance of type FDSNamelist.
        """
        for p in fds_namelist.fds_params:
            imported = False
            # Protect from None
            if not p:
                continue
            # Import to manged BFParam
            bf_param = self.get_by_label(p.fds_label)
            if bf_param:
                try:
                    bf_param.from_fds(context, p.value)
                except BFNotImported:
                    pass
                else:
                    imported = True
            # Import to BFParamOther
            if not imported and self.bf_param_other:
                try:
                    self.bf_param_other.set_value(context, value=p.to_fds(context))
                except BFNotImported:
                    pass
                else:
                    imported = True
            # Not imported
            if not imported:
                raise BFNotImported(self, f"Value {p} not imported")
        # All imported, set namelist exported and appearance
        self.set_exported(context, True)
        self.set_appearance(context)

    def copy_to(self, dest_element):
        """!
        Copy self values to destination element.
        @param dest_element: element of the same type of self.element.
        """
        log.debug(f"Copying <{self}> to <{dest_element.name}>")
        # self.bf_namelist_cls is copied by the operator
        if self.bpy_export:
            value = getattr(self.element, self.bpy_export)
            setattr(dest_element, self.bpy_export, value)
        for p in self.bf_params:
            p.copy_to(dest_element)


class BFNamelistSc(BFNamelist):
    """!
    Blender representation of an FDS namelist group related to a Blender Scene.
    """

    bpy_type = Scene

    def set_appearance(self, context):
        pass


class BFNamelistOb(BFNamelist):
    """!
    Blender representation of an FDS namelist group related to a Blender Object.
    """

    bpy_type = Object

    @property
    def exported(self):
        return not self.element.hide_render

    def set_exported(self, context, value=None):
        if value is None:
            self.element.hide_render = not self.bpy_export_default
        else:
            self.element.hide_render = not bool(value)

    def set_appearance(self, context):
        super().set_appearance(context)  # preferences check
        # Init
        ma_inert = bpy.data.materials.get("INERT")
        ma_dummy0 = bpy.data.materials.get("Dummy Color1")  # HOLE
        ma_dummy1 = bpy.data.materials.get("Dummy Color2")  # DEVC, SLCF, PROF, ...
        ma_dummy2 = bpy.data.materials.get("Dummy Color3")  # INIT, ZONE
        # WIRE: MESH, HVAC
        # Set
        appearance = self.bf_other.get("appearance")
        if appearance == "TEXTURED" and ma_inert:
            self.element.show_wire = False
            self.element.display_type = "TEXTURED"
            return
        self.element.show_wire = True
        if appearance == "DUMMY0" and ma_dummy0:
            self.element.active_material = ma_dummy0
            self.element.display_type = "SOLID"
        elif appearance == "DUMMY1" and ma_dummy1:
            self.element.active_material = ma_dummy1
            self.element.display_type = "SOLID"
        elif appearance == "DUMMY2" and ma_dummy2:
            self.element.active_material = ma_dummy2
            self.element.display_type = "SOLID"
        elif appearance == "WIRE":
            self.element.display_type = "WIRE"


class BFNamelistMa(BFNamelist):
    """!
    Blender representation of an FDS namelist group related to a Blender Material.
    """

    bpy_type = Material

    def set_appearance(self, context):
        super().set_appearance(context)  # preferences check
        self.element.use_nodes = False


# Python representations of FDS entities


class FDSParam:
    """!
    Datastructure representing an FDS parameter.
    """

    def __init__(
        self,
        fds_label=None,
        value=None,
        precision=3,
        exponential=False,
        msg=None,
        f90=None,
    ):
        """!
        Class constructor.
        @param fds_label: namelist parameter label.
        @param value: parameter value of any type.
        @param precision: float precision, number of decimal digits.
        @param exponential: if True sets exponential representation of floats.
        @param msg: comment message.
        @param f90: FDS formatted string of value, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        ## parameter label
        self.fds_label = fds_label
        ## parameter value of any type
        self.value = value
        ## float precision, number of decimal digits
        self.precision = precision
        ## if True sets exponential representation of floats
        self.exponential = exponential
        ## comment message
        self.msg = msg
        # Fill self.value from f90 string
        if f90:
            self.from_fds(f90=f90)

    def __str__(self):
        res = self.to_fds()
        if len(res) > 80:
            return res[:37] + " ... " + res[-37:]
        return res

    def _get_formatted_values(self):
        """!
        Return a tuple of FDS formatted values or an empty tuple, eg. "'Test1'","'Test2'".
        """
        values = self.values
        if not values:
            return tuple()
        elif isinstance(values[0], float):
            if self.exponential:
                return tuple(f"{v:.{self.precision}E}" for v in values)
            else:
                return tuple(
                    f"{round(v,self.precision):.{self.precision}f}" for v in values
                )
        elif isinstance(values[0], str):
            return tuple("'" in v and f'"{v}"' or f"'{v}'" for v in values)
        elif isinstance(values[0], bool):  # always before int
            return tuple(v and "T" or "F" for v in values)
        elif isinstance(values[0], int):
            return tuple(str(v) for v in values)
        else:
            raise ValueError(f"Unknown value type <{self.value}>")

    @property
    def values(self):
        """!
        Return an iterable self.value.
        """
        if self.value is None:
            return tuple()
        elif not is_iterable(self.value):
            return tuple((self.value,))
        else:
            return self.value

    def to_fds(self, context=None):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        v = ",".join(self._get_formatted_values())
        if self.fds_label:
            if v:  # "ABC=1,2,3"
                return f"{self.fds_label}={v}"
            else:  # "ABC"
                return self.fds_label

    _re_decimal = r"\.([0-9]+)"  # decimal positions

    _scan_decimal = re.compile(_re_decimal, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _re_integer = r"([0-9]*)\.?[0-9]*[eE]"  # integer postions of exp notation

    _scan_integer = re.compile(_re_integer, re.VERBOSE | re.DOTALL | re.IGNORECASE)

    _scan_values = re.compile(
        r"""'.*?'|".*?"|[^,\s\t]+""", re.VERBOSE | re.DOTALL | re.IGNORECASE
    )

    def from_fds(self, f90):
        """!
        Import from FDS formatted string, on error raise BFException.
        @param f90: FDS formatted string containing value, eg. "2.34, 1.23, 3.44" or ".TRUE.,.FALSE.".
        """
        # Remove trailing spaces and newlines, then scan values
        f90c = " ".join(f90.strip().splitlines())
        values = re.findall(self._scan_values, f90c)
        # Eval values
        for i, v in enumerate(values):
            if v in (".TRUE.", "T"):
                values[i] = True
            elif v in (".FALSE.", "F"):
                values[i] = False
            else:
                try:
                    values[i] = eval(v)
                except Exception as err:
                    msg = f"Error while parsing value <{v}> in <{self.fds_label}={f90}>\n<{err}>"
                    raise BFException(self, msg)
        # Post treatment of float
        if isinstance(values[0], float):  # first value is a float
            # Get precision
            match = re.findall(self._re_decimal, f90c)
            self.precision = match and max(len(m) for m in match) or 1
            # Get exponential
            match = re.findall(self._re_integer, f90c)
            if match:
                self.exponential = True
                self.precision += max(len(m) for m in match) - 1
        # Record in self.value
        if len(values) == 1:
            self.value = values[0]
        else:
            self.value = values


class FDSNamelist:
    """!
    Datastructure representing an FDS namelist.
    """

    ## max number of columns of formatted output
    maxlen = 80  # TODO to config

    def __init__(self, fds_label=None, fds_params=None, msg=None, f90=None):
        """!
        Class constructor.
        @param fds_label: namelist group label.
        @param fds_params: list of FDSParam instances.
        @param msg: comment message.
        @param f90: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        ## namelist group label
        self.fds_label = fds_label
        ## list (single) or list of list (multi) of FDSParam instances
        ## eg. (("ID=X1", "PBX=1"), ("ID=X2", "PBX=2"), ...)
        self.fds_params = fds_params or list()
        ## comment message
        self.msg = msg
        # Fill self.fds_params from f90 string
        if f90:
            self.from_fds(f90=f90)

    def __str__(self):
        return self.to_fds()

    def get_by_label(self, fds_label, remove=False):
        """!
        Return the first FDSParam instance in list by its label.
        @param fds_label: namelist parameter label.
        @param remove: remove it from self
        @return None or FDSParam.
        """
        for res in self.fds_params:
            if res.fds_label == fds_label:
                if remove:
                    self.fds_params.remove(res)
                return res

    def to_fds(self, context=None):
        """!
        Return the FDS formatted string.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or a None.
        """
        msgs = self.msg and self.msg.splitlines() or list()
        # Classify parameters
        invps = list()  # invariant parameters
        multips = list()  # multi parameters
        for p in self.fds_params:
            # Empty
            if not p:  # Protect from empty (eg. None and tuple())
                continue
            # Invariant
            elif isinstance(p, FDSParam):  # single
                invps.append(p)
                msgs.append(p.msg)
            elif isinstance(p, tuple):  # many or multi
                if isinstance(p[0], FDSParam):  # many
                    invps.extend(p)
                    msgs.extend(pi.msg for pi in p)
                elif isinstance(p[0], tuple):  # multi
                    multips = p
                    msgs.extend(
                        p0.msg for p0 in multips[0]
                    )  # msg only from first many of multi
            else:
                raise ValueError(f"Unrecognized type of <{p}>")
        # Treat invariant, many and multi parameters
        # nl = FDSParam, FDSParam, ...
        nls = list()  # list of nl
        if multips:
            # Remove ID parameter, as multi embeds a new indexed ID.
            for i, p in enumerate(invps):
                if p.fds_label == "ID":
                    invps.pop(i)
                    break
            # Add nl with one of multips + invps
            for multip in multips:
                nl = list(multip)
                nl.extend(invps)
                nls.append(nl)
        else:
            nls.append(invps)
        # Prepare message lines
        lines = list(f"! {m}" for m in msgs if m)  # all messages
        # Prepare namelist lines
        if self.fds_label:
            for nl in nls:
                newline = False
                line = f"&{self.fds_label}"
                for p in nl:
                    if not p.fds_label:
                        continue
                    label = p.fds_label
                    vs = p._get_formatted_values()  # list of str
                    if not vs:  # no formatted values
                        if not newline and len(line) + 1 + len(label) <= self.maxlen:
                            # Parameter to the same line
                            newline = False
                            line += " " + label
                        else:
                            # Parameter to new line
                            lines.append(line)
                            line = "      " + label  # new line
                    else:  # available formatted values
                        v = ",".join(vs)  # values str
                        if (
                            not newline
                            and len(line) + 1 + len(label) + 1 + len(v) <= self.maxlen
                        ):
                            # Parameter to the same line
                            newline = False
                            line += " " + label + "=" + v
                        else:
                            # Parameter to new line
                            lines.append(line)
                            line = "      " + label + "="  # new line
                            if len(line) + len(v) <= self.maxlen:
                                # Formatted values do not need splitting
                                line += v
                            else:
                                # Formatted values need splitting
                                newline = True  # the following needs a new line
                                for v in vs:
                                    if len(line) + len(v) + 1 <= self.maxlen:
                                        line += v + ","
                                    else:
                                        lines.append(line)
                                        line = "        " + v + ","  # new line
                                line = line[:-1]  # remove last ","
                line += " /"
                lines.append(line)
        return "\n".join(lines)

    _scan_params = re.compile(
        r"""
        ([A-Z][A-Z0-9_\(\):,]*?)  # label (group 0)
        [,\s\t]*                  # 0+ separators
        =                         # = sign
        [,\s\t]*                  # 0+ separators
        (                         # value (group 1)
            (?:'.*?'|".*?"|.+?)*?     # 1+ any char, protect str, not greedy
                (?=                       # end previous match when:
                    (?:                       # there is another label:
                        [,\s\t]+                  # 1+ separators
                        [A-Z][A-Z0-9_\(\):,]*?    # label
                        [,\s\t]*                  # 0+ separators
                        =                         # = sign
                    )
                |                         # or
                    $                         # it is end of line
                )
        )
        """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE,
    )  # no MULTILINE, so that $ is the end of the file

    def from_fds(self, f90):
        """!
        Import from FDS formatted string of parameters, on error raise BFException.
        @param f90: FDS formatted string of parameters, eg. "ID='Test' PROP=2.34, 1.23, 3.44".
        """
        f90 = " ".join(f90.strip().splitlines())
        for match in re.finditer(self._scan_params, f90):
            label, f90_value = match.groups()
            self.fds_params.append(FDSParam(fds_label=label, f90=f90_value))


class FDSCase:
    """!
    Datastructure representing an FDS case.
    """

    def __init__(self, fds_namelists=None, msg=None, filepath=None, f90=None):
        """!
        Class constructor.
        @param fds_namelists: list of FDSNamelist instances.
        @param msg: comment message.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        ## list of FDSNamelist instances
        self.fds_namelists = fds_namelists or list()
        ## comment message
        self.msg = msg
        # Fill fds_namelists from filepath or f90
        if filepath or f90:
            self.from_fds(filepath=filepath, f90=f90)

    def __str__(self):
        return self.to_fds()

    def get_by_label(self, fds_label, remove=False):
        """!
        Return the first FDSNamelist instance in list by its label.
        @param fds_label: namelist label.
        @param remove: remove it from self
        @return None or FDSNamelist.
        """
        for res in self.fds_namelists:
            if res.fds_label == fds_label:
                if remove:
                    self.fds_namelists.remove(res)
                return res

    def to_fds(self, context=None):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        lines = list()
        if self.msg:
            lines.extend(tuple(f"! {m}" for m in self.msg.splitlines()))
        lines.extend(
            fds_namelist.to_fds() for fds_namelist in self.fds_namelists if fds_namelist
        )
        return "\n".join(l for l in lines if l)

    _scan_namelists = re.compile(
        r"""
        ^&                    # & at the beginning of the line
        ([A-Z]+[A-Z0-9]*)     # namelist label (group 0)
        [,\s\t]*              # 0+ separator, greedy
        (                     # namelist params (group 1)
        (?:'.*?'|".*?"|.*?)*  # 0+ any char, protect quoted strings, greedy
        ) 
        [,\s\t]*              # 0+ separator, greedy
        /                     # / end char
        """,
        re.VERBOSE | re.DOTALL | re.IGNORECASE | re.MULTILINE,
    )  # MULTILINE, so that ^ is the beginning of each line

    def from_fds(self, filepath=None, f90=None):
        """!
        Import from FDS file, on error raise BFException.
        @param filepath: filepath of FDS case to be imported.
        @param f90: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        """
        # Init f90
        if filepath and not f90:
            f90 = io.read_txt_file(filepath)
        elif f90 and filepath:
            raise AssertionError("Cannot set both filepath and f90.")
        # Scan and fill self.fds_namelist
        for match in re.finditer(self._scan_namelists, f90):
            label, f90_params = match.groups()
            try:
                fds_namelist = FDSNamelist(fds_label=label, f90=f90_params)
            except BFException as err:
                err.msg += f"\nin namelist:\n&{label} {f90_params} /"
                raise err
            self.fds_namelists.append(fds_namelist)
