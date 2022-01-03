import logging
from bpy.types import Material
from ...types import BFNamelist

# from .lang import bf_namelists_by_cls  # FIXME FIXME FIXME

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
        return BFNamelist.subclasses_by_cls_name[self.bf_namelist_cls](self)

    def to_fds(self, context):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @return None or FDS formatted string, eg. "&OBST ID='Test' /".
        """
        return self.bf_namelist.to_fds(context)

    def from_fds(self, context, fds_namelist, free_text=None):
        """!
        Set self.bf_namelist from FDSNamelist, on error raise BFException.
        @param context: the Blender context.
        @param fds_namelist: FDSNamelist.
        """
        self.bf_namelist_cls = "MN_SURF"
        self.bf_namelist.from_fds(
            context, fds_namelist=fds_namelist, free_text=free_text
        )

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
