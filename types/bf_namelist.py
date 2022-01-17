"""!
BlenderFDS, Blender interfaces to FDS namelists.
"""

import logging
import bpy
from bpy.types import Object, Scene, Material
from .. import config
from .fds_namelist import FDSNamelist
from .bf_exception import BFException, BFNotImported, BFWarning
from .bf_param import BFParam, BFParamXB, BFParamXYZ, BFParamPB, BFParamOther

log = logging.getLogger(__name__)


class BFNamelist(BFParam):
    """!
    Blender representation of an FDS namelist group.
    """

    ## List of subclassess
    subclasses = list()
    ## Dict of subclassess by fds_label
    subclasses_by_fds_label = dict()
    ## Dict of subclassess by cls name
    subclasses_by_cls_name = dict()

    def __init__(self, element):
        ## FDS element represented by this class instance
        self.element = element
        ## My sub params, tuple of element instances of type BFParam
        self.bf_params = tuple(p(element) for p in self.bf_params)

    # inherit __str__(), __init_subclass__(), get_subclass()

    @classmethod
    def register(cls):
        super().register()
        # Indexes are used to link both the class and the instance
        # otherwise the references are changed when instancing
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

    def get(self, fds_label):
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
    def exported(self):  # FIXME rm property
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
        pass  # Set appearance of self.element in subclasses

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
            fds_params=list(
                bf_param.to_fds_param(context)
                for bf_param in self.bf_params
                if bf_param
            ),
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

    def from_fds(self, context, fds_namelist, free_text=None):
        """!
        Set self.bf_params value, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: instance of type FDSNamelist.
        @param free_text: instance of type Blender Text or None.
        """
        for fds_param in fds_namelist.fds_params:
            imported = False
            # Protect from None
            if not fds_param:
                continue
            # Import to manged BFParam
            bf_param = self.get(fds_param.fds_label)
            if bf_param:
                try:
                    bf_param.from_fds(context=context, value=fds_param.value)
                except BFNotImported as err:
                    if free_text:
                        free_text.write(err.to_fds())
                else:
                    imported = True
            # Import to BFParamOther
            if not imported and self.bf_param_other:
                try:
                    self.bf_param_other.set_value(
                        context, value=fds_param.to_fds(context)
                    )
                except BFNotImported:
                    if free_text:
                        free_text.write(err.to_fds())
                else:
                    imported = True
            # Still not imported?
            if not imported:
                raise BFException(self, f"Value {fds_param} not imported")
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

    @property
    def exported(self):
        if self.element.name in config.default_mas:
            return False  # default fds material
        elif self.element.bf_surf_export:
            return True  # user requested
        return False

    def set_appearance(self, context):
        super().set_appearance(context)  # preferences check
        self.element.use_nodes = False
