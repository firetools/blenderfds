"""!
BlenderFDS, Blender interfaces to FDS parameters.
"""

import logging, bpy
from bpy.types import Operator, Mesh
from bpy.props import IntProperty, CollectionProperty, BoolProperty, StringProperty
from .fds_namelist import FDSMany
from .fds_param import FDSParam
from .bf_exception import BFException, BFNotImported, BFWarning

log = logging.getLogger(__name__)


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
    ## type in bpy.types for Blender property (Scene, Material, Object, or Mesh)
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

    ## List of subclassess
    subclasses = list()
    ## Dict of subclassess by fds_label
    _subclasses_by_fds_label = dict()
    ## Dict of subclassess by cls name
    _subclasses_by_cls_name = dict()

    ## List of registered bpy_idname for unregistering
    _registered_bpy_idnames = list()

    def __init__(self, element):
        """!
        Class constructor.
        @param element: FDS element represented by this class instance.
        """
        # Treat Mesh BFParam
        if self.bpy_type == Mesh:
            element = element.data
        ## FDS element represented by this class instance
        self.element = element

    def __init_subclass__(cls, **kwargs):
        """!
        Subclass constructor.
        """
        super().__init_subclass__(**kwargs)
        cls.subclasses.append(cls)  # collection of subclasses
        cls._subclasses_by_cls_name[cls.__name__] = cls
        if cls.fds_label and cls.fds_label not in cls._subclasses_by_fds_label:
            cls._subclasses_by_fds_label[cls.fds_label] = cls  # only the first

    def __str__(self):
        return f"{self.label}(element='{self.element.name}')"

    def __repr__(self) -> str:
        items = ", ".join(f"{k}={v!r}" for k, v in self.__dict__.items())
        return f"<{self.__class__.__name__}({items})>"

    @classmethod
    def get_subclass(cls, fds_label=None, cls_name=None, default=None):
        """!
        Get item in cls collection by fds_label or cls_name.
        """
        if cls_name:
            return cls._subclasses_by_cls_name.get(cls_name, default)
        if fds_label:
            return cls._subclasses_by_fds_label.get(fds_label, default)

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        # log.debug(f"Registering <{cls.label}>")
        if not cls.bpy_type:
            # log.debug(f"No bpy_type in class <{cls}>")
            return
        # Insert fds_default
        if cls.fds_default is not None:
            # ...in description
            cls.description += f"\n(FDS default: {cls.fds_default})"
            # ...in bpy_default if not present
            if cls.bpy_default is None:
                cls.bpy_default = cls.fds_default
        # Create bpy_idname
        if cls.bpy_prop and cls.bpy_idname:
            # Check label and description
            if not cls.label or not cls.description:
                raise AssertionError(f"No label or description in <{cls}>")
            if hasattr(cls.bpy_type, cls.bpy_idname):
                # Already existing bpy_type.bpy_idname
                # log.debug(f"Using existing <{cls.bpy_idname}> Blender property")
                if cls.bpy_other or cls.bpy_default:
                    raise AssertionError(f"Unused bpy_other or bpy_default in <{cls}>")
            else:
                # New bpy_type.bpy_idname
                # log.debug(f"Setting <{cls.bpy_idname}> Blender property")
                bpy_other = cls.bpy_other.copy()
                if cls.bpy_default is not None:
                    bpy_other["default"] = cls.bpy_default
                setattr(
                    cls.bpy_type,
                    cls.bpy_idname,
                    cls.bpy_prop(
                        name=cls.label, description=cls.description, **bpy_other
                    ),
                )
                cls._registered_bpy_idnames.append(cls.bpy_idname)
        # elif cls.bpy_prop or cls.bpy_idname:
        #     log.debug(f"Unused bpy_prop or bpy_idname in <{cls}>")
        # else:
        #     log.debug(f"No bpy_prop and bpy_idname in <{cls}>")
        # Create bpy_export
        if cls.bpy_export:
            if hasattr(cls.bpy_type, cls.bpy_export):
                # Already existing bpy_type.bpy_export
                # log.debug(f"Using <{cls.bpy_export}> Blender property as export ref")
                if cls.bpy_export_default is not None:
                    raise AssertionError(f"Unused bpy_export_default in <{cls}>")
            else:
                # New bpy_type.bpy_export
                # log.debug(f"Setting <{cls.bpy_export}> Blender property")
                if cls.bpy_export_default is None:
                    raise AssertionError(f"Undefined bpy_export_default in <{cls}>")
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

    def get_value(self, context):
        """!
        Return value from element instance.
        @return any type
        """
        if self.bpy_idname:
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
            try:
                setattr(self.element, self.bpy_idname, value)
            except TypeError:
                raise BFNotImported(
                    sender=self,
                    msg=f"Unsupported value: <{value}>",
                )
            return

    def get_exported(self, context):
        """!
        Return True if self is exported to FDS.
        """
        # Check if empty
        value = self.get_value(context)
        if value is None or value == "":
            return False
        # Check if identical to FDS default
        d = self.fds_default
        if d is not None:
            if isinstance(value, float):  # FIXME floats comparison
                return value > d + 1e-6 or value < d - 1e-6
            elif value == d:  # other comparison
                return False
        # Check if bpy_export is True
        if self.bpy_export:
            return bool(getattr(self.element, self.bpy_export))
        return True

    def set_exported(self, context, value=None):
        """!
        Set if self is exported to FDS. If value is None, set default value.
        @param context: the Blender context.
        @param value: value to set.
        """
        if self.bpy_export is None:
            if not value:
                msg = f"Cannot self.set_exported(False) in <{self}>"
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
        active, alert = True, False
        if self.bpy_export:
            active = bool(getattr(self.element, self.bpy_export))
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
        @return None, FDSParam, FDSNamelist, FDSMany or FDSMulti.
        """
        if self.get_exported(context):
            self.check(context)
            if self.fds_label:
                return FDSParam(
                    fds_label=self.fds_label,
                    value=self.get_value(context),
                    precision=self.bpy_other.get("precision", 3),
                )

    def show_fds_geometry(self, context, ob_tmp):
        """!
        Append my FDS geometry to the ob_tmp.
        @param context: the Blender context.
        @param ob_tmp: the tmp Object that receives the geometry.
        """
        return

    # No to_fds, because to_fds_param can be None, many or multi
    # and it makes no sense

    def from_fds(self, context, value):
        """!
        Set self from py value, on error raise BFException.
        @param context: the Blender context.
        @param value: the value to set. Can be of any type.
        """
        self.set_value(context, value)
        self.set_exported(context, True)

    def copy_to(self, dest_element):  # FIXME add context
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


class BFParamFYI(BFParam):
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


# BFParam other, custom_uilist


class _OPSlotAdd:
    """!
    Operator helper to add slot to custom list operator.
    """

    bl_label = "Add"
    bl_description = "Add slot"

    bpy_type = None
    bpy_idx_idname = "bf_other_idx"
    bpy_idname = "bf_other"

    def set_item(self, context, item):
        """!
        Set item in the new slot.
        @param context: the <a href="https://docs.blender.org/api/current/bpy.context.html">blender context</a>.
        @param item: TODO
        """
        pass

    def execute(self, context):
        celem = getattr(context, self.bpy_type.__name__.lower())
        ccollection = getattr(celem, self.bpy_idname)
        item = ccollection.add()
        self.set_item(context, item)
        setattr(celem, self.bpy_idx_idname, len(ccollection) - 1)
        return {"FINISHED"}


class _OPSlotRm:
    """!
    Operator helper to remove slot from custom list operator.
    """

    bl_label = "Remove"
    bl_description = "Remove slot"

    bpy_type = None
    bpy_idx_idname = "bf_other_idx"
    bpy_idname = "bf_other"

    @classmethod
    def poll(cls, context):
        celem = getattr(context, cls.bpy_type.__name__.lower())
        return getattr(celem, cls.bpy_idname)

    def invoke(self, context, event):
        celem = getattr(context, self.bpy_type.__name__.lower())
        cidx = getattr(celem, self.bpy_idx_idname)
        ccollection = getattr(celem, self.bpy_idname)
        if cidx < 0:  # available item?
            return {"FINISHED"}
        ccollection.remove(cidx)
        setattr(celem, self.bpy_idx_idname, max(0, cidx - 1))
        return {"FINISHED"}


class _OPSlotMv:
    """!
    Operator helper to move slot from custom list operator.
    """

    bl_label = "Move"
    bl_description = "Move slot"

    bpy_type = None
    bpy_idx_idname = "bf_other_idx"
    bpy_idname = "bf_other"

    direction: bpy.props.EnumProperty(items=(("UP", "Up", ""), ("DOWN", "Down", "")))

    @classmethod
    def poll(cls, context):
        celem = getattr(context, cls.bpy_type.__name__.lower())
        return getattr(celem, cls.bpy_idname)

    def execute(self, context):
        celem = getattr(context, self.bpy_type.__name__.lower())
        cidx = getattr(celem, self.bpy_idx_idname)
        ccollection = getattr(celem, self.bpy_idname)
        delta = -1 if self.direction == "UP" else 1
        neighbor = cidx + delta
        ccollection.move(neighbor, cidx)
        setattr(celem, self.bpy_idx_idname, max(0, min(neighbor, len(ccollection) - 1)))
        return {"FINISHED"}


class BFParamOther(BFParam):
    """!
    Blender representation of any FDS parameter, helper for the 'other' FDS parameters.
    """

    label = "Other Parameters"
    description = "Other parameters (eg. PROP='Example')"
    bpy_idname = "bf_other"

    bpy_pg = None  # PropertyGroup, eg. WM_PG_bf_other
    bpy_ul = None  # UIList, eg. WM_UL_bf_other_items

    @classmethod
    def register(cls):
        # log.debug(f"Registering <{cls.label}>")
        if not cls.bpy_type:
            # log.debug(f"No bpy_type in class <{cls}>")
            return
        # Register index bpy_idx_idname
        bpy_idx_idname = f"{cls.bpy_idname}_idx"
        prop = IntProperty(name="Index", default=0)
        setattr(cls.bpy_type, bpy_idx_idname, prop)
        # Register collection bpy_idname
        prop = CollectionProperty(
            name=cls.label, description=cls.description, type=cls.bpy_pg
        )
        setattr(cls.bpy_type, cls.bpy_idname, prop)
        # Register operators: add, rm, mv
        op_name = f"{cls.bpy_type.__name__.upper()}_OT_{cls.bpy_idname}"
        op_idname = f"{cls.bpy_type.__name__.lower()}.{cls.bpy_idname}"
        op_add = type(
            op_name + "_slot_add",
            (_OPSlotAdd, Operator),
            {
                "bl_idname": op_idname + "_slot_add",
                "bpy_type": cls.bpy_type,
                "bpy_idx_idname": bpy_idx_idname,
                "bpy_idname": cls.bpy_idname,
            },
        )
        op_rm = type(
            op_name + "_slot_rm",
            (_OPSlotRm, Operator),
            {
                "bl_idname": op_idname + "_slot_rm",
                "bpy_type": cls.bpy_type,
                "bpy_idx_idname": bpy_idx_idname,
                "bpy_idname": cls.bpy_idname,
            },
        )
        op_mv = type(
            op_name + "_slot_mv",
            (_OPSlotMv, Operator),
            {
                "bl_idname": op_idname + "_slot_mv",
                "bpy_type": cls.bpy_type,
                "bpy_idx_idname": bpy_idx_idname,
                "bpy_idname": cls.bpy_idname,
            },
        )
        cls._ops = (op_add, op_rm, op_mv)
        for op in cls._ops:
            bpy.utils.register_class(op)

    @classmethod
    def unregister(cls):
        # log.debug(f"Unregistering <{cls.label}>")
        if not cls.bpy_type:
            # log.debug(f"No bpy_type in class <{cls}>")
            return
        bpy_idx_idname = f"{cls.bpy_idname}_idx"
        # Unregister operators
        for op in cls._ops:
            bpy.utils.unregister_class(op)
        # Unregister collection and index
        delattr(cls.bpy_type, cls.bpy_idname)
        delattr(cls.bpy_type, bpy_idx_idname)

    def get_value(self, context):
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
        # Init
        bpy_idx_idname = f"{self.bpy_idname}_idx"
        op_idname = f"{self.bpy_type.__name__.lower()}.{self.bpy_idname}"
        # Draw
        layout.label(text=self.label)
        row = layout.row()
        row.template_list(
            self.bpy_ul.__name__,
            "",
            self.element,
            self.bpy_idname,
            self.element,
            bpy_idx_idname,
            rows=3,
        )
        col = row.column(align=True)
        col.operator(f"{op_idname}_slot_add", icon="ADD", text="")
        col.operator(f"{op_idname}_slot_rm", icon="REMOVE", text="")
        col.separator()
        col.operator(f"{op_idname}_slot_mv", icon="TRIA_UP", text="").direction = "UP"
        col.operator(
            f"{op_idname}_slot_mv", icon="TRIA_DOWN", text=""
        ).direction = "DOWN"

    def to_fds_param(self, context):
        self.check(context)
        coll = getattr(self.element, self.bpy_idname)
        if coll:
            return FDSMany(
                (
                    FDSParam(fds_label=item.name)
                    for item in coll
                    if item.bf_export and item.name
                )
            )

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
