"""!
BlenderFDS, Blender interfaces to FDS namelists.
"""

import logging

from numpy import iterable
from bpy.types import Object, Scene, Material
from .. import config
from .fds_list import FDSList, FDSNamelist
from .bf_exception import BFException, BFNotImported
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

    def draw_header(self, context, layout, panel):
        """!
        Draw my header on layout.
        @param context: the Blender context.
        @param layout: the Blender panel layout.
        @param panel: the calling panel.
        """
        if self.bpy_export:
            layout.prop(self.element, self.bpy_export, icon_only=True)
        if self.description:
            panel.bl_label = f"FDS {self.label} ({self.description})"
        else:
            panel.bl_label = self.label

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

    def to_fds_list(self, context) -> FDSList:
        """!
        Return the FDSList instance from self, never None.
        """
        if not self.get_exported(context):
            return FDSList()
        self.check(context)
        if self.fds_label:
            return FDSNamelist(
                iterable=(
                    bf_param.to_fds_list(context)
                    for bf_param in self.bf_params
                    if bf_param
                ),
                fds_label=self.fds_label,
            )
        return FDSList()

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
                        context=context, value=fds_param.get_value()
                    )
                except BFNotImported as err:
                    context.scene.bf_config_text.write(str(err))
                else:
                    is_imported = True

            # Try bf_param_other
            bf_param_other = self.get_bf_param_other()
            if not is_imported and bf_param_other:
                try:
                    bf_param_other.set_value(
                        context, value=fds_param.to_string()
                    )
                except BFNotImported as err:
                    context.scene.bf_config_text.write(str(err))
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
    bpy_export = "hide_render"
    bpy_export_default = False

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

    def draw_header(self, context, layout, panel):
        ob = self.element
        # Manage temporary Object
        if ob.bf_is_tmp:
            panel.bl_label = "FDS Temporary Geometry"
            return
        # Manage all others
        if self.bpy_export:
            layout.prop(self.element, self.bpy_export, icon_only=True, toggle=False, invert_checkbox=True)
        if self.description:
            panel.bl_label = f"FDS {self.label} ({self.description})"
        else:
            panel.bl_label = self.label

    def draw(self, context, layout):
        ob = self.element
        # Manage temporary Object
        if ob.bf_is_tmp:
            layout.operator("scene.bf_hide_fds_geometry", icon="HIDE_ON")
            return
        # Manage all others
        row = layout.row()
        if ob.bf_has_tmp:
            row.operator("scene.bf_hide_fds_geometry", icon="HIDE_ON")
        else:
            row.operator("object.bf_show_fds_geometry", icon="HIDE_OFF")
        row.operator("object.bf_show_fds_code", icon="HIDE_OFF")    
        return super().draw(context, layout)


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
            
    def draw_header(self, context, layout, panel):
        ma = self.element
        if self.bpy_export and ma.name not in config.default_mas:
            layout.prop(self.element, self.bpy_export, icon_only=True)
        if self.description:
            panel.bl_label = f"FDS {self.label} ({self.description})"
        else:
            panel.bl_label = self.label

    def draw(self, context, layout):
        ma = self.element
        layout.operator("material.bf_show_fds_code", icon="HIDE_OFF")
        # Manage default Materials
        if ma.name in config.default_mas:
            layout.label(text=f"Predefined {self.element.name} boundary condition")
            layout.prop(ma, "diffuse_color", text="RGB")
            return
        # Manage all others
        return super().draw(context, layout)