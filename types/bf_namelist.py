"""!
BlenderFDS, Blender interfaces to FDS namelists.
"""

import logging
from bpy.types import Object, Scene, Material
from .. import config
from .fds_list import FDSNamelist
from .bf_exception import BFException, BFNotImported, BFWarning
from .bf_param import BFParam, BFParamOther

log = logging.getLogger(__name__)


class BFNamelist(BFParam):
    """!
    Blender representation of an FDS namelist group.
    """

    ## List of subclassess
    subclasses = list()
    ## Dict of subclassess by fds_label
    _subclasses_by_fds_label = dict()
    ## Dict of subclassess by cls name
    _subclasses_by_cls_name = dict()

    def __init__(self, element):
        ## FDS element represented by this class instance
        self.element = element
        ## My sub params, tuple of element instances of type BFParam
        self.bf_params = tuple(p(element=element) for p in self.bf_params)

    # inherit __str__(), __repr()__, __init_subclass__(), get_subclass()

    @classmethod
    def register(cls):
        super().register()
        # Indexes are used to link both the class and the instance
        # otherwise the references are changed when instancing
        cls._bf_param_idx_by_fds_label = dict()  # fds_label: index of param
        cls._bf_param_other_idx = None  # ... of type BFParamOther
        for i, p in enumerate(cls.bf_params):
            if p.fds_label:
                cls._bf_param_idx_by_fds_label[p.fds_label] = i
            elif issubclass(p, BFParamOther):
                cls._bf_param_other_idx = i


    def get_bf_param(self, fds_label):
        """!
        Return bf_param (class or instance) by its fds_label.
        @param fds_label: FDS parameter to be obtained.
        @return BFParam or None.
        """
        i = self._bf_param_idx_by_fds_label.get(fds_label)
        if i is not None:
            return self.bf_params[i]

    def get_bf_param_other(self):
        """!
        Return the reference of the other bf_param (class or instance).
        """
        if self._bf_param_other_idx is not None:
            return self.bf_params[self._bf_param_other_idx]

    def get_exported(self, context):
        """!
        Return True if self is exported to FDS.
        """
        if self.bpy_export:
            return bool(getattr(self.element, self.bpy_export))
        return True

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
        layout.active = self.get_exported(context)
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

    def to_fds_namelist(self, context):  # FIXME to_fds_list
        """!
        Return the FDSNamelist representation of element instance.
        @param context: the Blender context.
        @return None, FDSNamelist, or (FDSNamelist, ...) instances.
        """
        # Get if exported and check
        if not self.get_exported(context) or not self.fds_label:
            return
        self.check(context)
        # Assemble from bf_params, protect from None
        return FDSNamelist(
            (
                bf_param.to_fds_param(context)
                for bf_param in self.bf_params
                if bf_param
            ),
            fds_label=self.fds_label,
        )
        
    def to_fds(self, context):  # FIXME remove
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        fds_namelist = self.to_fds_namelist(context)
        if fds_namelist:
            if isinstance(fds_namelist, FDSNamelist):
                return fds_namelist.to_string(context)
            else:
                return "\n".join(line.to_string(context) for line in fds_namelist if line)

    def from_fds(self, context, fds_namelist, fds_label=None):
        """!
        Set self.bf_params value, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: instance of type FDSNamelist.
        @param fds_label: if set, import only self.bf_params with fds_label
        """
        while True:  # consume fds_namelist
            fds_param = fds_namelist.get_fds_label(fds_label=fds_label, remove=True)
            if not fds_param:
                break
            is_imported = False

            # Try managed bf_param
            bf_param = self.get_bf_param(fds_label=fds_param.fds_label)
            if not is_imported and bf_param:
                try:
                    bf_param.from_fds(
                        context=context, value=fds_param.get_value(context)
                    )
                except BFNotImported as err:
                    context.scene.bf_config_text.write(err.to_fds())
                else:
                    is_imported = True

            # Try bf_param_other
            bf_param_other = self.get_bf_param_other()
            if not is_imported and bf_param_other:
                try:
                    bf_param_other.set_value(
                        context, value=fds_param.to_string(context)
                    )
                except BFNotImported as err:
                    context.scene.bf_config_text.write(err.to_fds())
                else:
                    is_imported = True

            # Raise if still not imported
            if not is_imported:
                raise BFException(self, f"Value {fds_param} not imported")

        # Set namelist exported and appearance
        self.set_exported(context, True)
        self.set_appearance(context)

    def copy_to(self, context, dest_element):
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
            p.copy_to(context=context, dest_element=dest_element)


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

    def get_exported(self, context):
        return not self.element.hide_render

    def set_exported(self, context, value=None):
        if value is None:
            self.element.hide_render = not self.bpy_export_default
        else:
            self.element.hide_render = not bool(value)

    def set_appearance(self, context):
        # Defaults
        display_type = "TEXTURED"
        show_name = False
        # Mods
        match self.bf_other.get("appearance"):
            case "BBOX":
                display_type, show_name = "WIRE", True
            case "WIRE":
                display_type = "WIRE"
            case _:
                pass
        # Set
        ob = self.element
        ob.display_type = display_type
        ob.show_name = show_name
        # ob.show_in_front = show_in_front  # unused
        # ob.show_wire = show_wire  # unused 

class BFNamelistMa(BFNamelist):
    """!
    Blender representation of an FDS namelist group related to a Blender Material.
    """

    bpy_type = Material

    def get_exported(self, context):
        if self.element.name in config.default_mas:
            return False  # default fds material
        elif self.element.bf_surf_export:
            return True  # user requested
        return False

    def set_appearance(self, context):
        pass
