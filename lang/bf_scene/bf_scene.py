import logging, bpy, os
from bpy.types import Scene
from bpy.props import IntVectorProperty
from ...config import MAXLEN
from ...types import BFNamelist, FDSList
from ... import utils
from . import export_helper, import_helper

log = logging.getLogger(__name__)


class BFScene:
    """!
    Extension of Blender Scene.
    """

    @property
    def bf_namelists(self):
        """!
        Return related bf_namelist, instance of BFNamelist.
        """
        return (n(element=self) for n in BFNamelist.subclasses if n.bpy_type == Scene)

    def to_fds_list(self, context, full=False) -> FDSList:
        """!
        Return the FDSList instance from self, never None.
        """
        # Set mysef as the right Scene instance in the context
        # It is needed, because context.scene is needed elsewhere
        bpy.context.window.scene = self  # set context.scene
        fds_list = FDSList()

        if full:
            export_helper.append_header(context=context, fds_list=fds_list)

        export_helper.append_sc_namelists(context=context, fds_list=fds_list)

        if self.bf_config_text_position == "BEGIN":
            export_helper.append_free_text(context=context, fds_list=fds_list)

        if full:
            export_helper.append_mas_namelists(context=context, fds_list=fds_list)

        export_helper.append_domain_namelists(context, fds_list=fds_list)

        if full:
            export_helper.append_cos_namelists(context=context, fds_list=fds_list)

        if self.bf_config_text_position == "END":
            export_helper.append_free_text(context=context, fds_list=fds_list)

        if full and self.bf_head_export:
            fds_list.append(FDSList(msg="\n&TAIL /\n"))

        return fds_list

    def to_fds(self, context, full=False, save=False):
        """!
        Return the FDS formatted string.
        @param context: the Blender context.
        @param full: if True, return full FDS case.
        @param save: if True, save to disk.
        @return FDS formatted string (eg. "&OBST ID='Test' /"), or None.
        """
        text = self.to_fds_list(context=context, full=full).to_string()
        if save:
            filepath = utils.io.transform_rbl_to_abs(
                context=context,
                filepath_rbl=self.bf_config_directory,
                name=self.name,
                extension=".fds",
            )
            utils.io.write_txt_file(filepath, text)
        return text

    def from_fds(
        self,
        context,
        filepath=None,
        f90_namelists=None,
        co_description="",
        set_tmp=False,
    ):
        """!
        Set self.bf_namelists from FDSList, on error raise BFException.
        @param context: the Blender context.
        @param filepath: filepath of FDS case to be imported.
        @param f90_namelists: FDS formatted string of namelists, eg. "&OBST ID='Test' /\n&TAIL /".
        @param co_description: Collection description string.
        @param set_tmp: set temporary Objects.
        """
        # Set mysef as the right Scene instance in the context
        # this is used by context.scene calls elsewhere
        # Also for visibility
        context.window.scene = self

        # Load fds case from filepath
        if filepath:
            filepath = utils.io.transform_rbl_to_abs(
                context=context,
                filepath_rbl=filepath,
            )
            f90_namelists = utils.io.read_txt_file(filepath=filepath)
            # and set imported fds case dir, because others rely on it
            # it is restored later
            bf_config_directory = self.bf_config_directory
            self.bf_config_directory = os.path.dirname(filepath)

        # Load fds case from f90_namelists
        fds_list = FDSList(f90_namelists=f90_namelists)

        # Prepare free text for unmanaged namelists, no rewind
        if not self.bf_config_text:
            self.bf_config_text = utils.ui.get_text_in_editor(
                context=context,
                text=None,
                name="New Text",
            )

        # Import by fds_label
        fds_labels = (
            "HEAD",
            "MOVE",  # pre-load moves and multiplicity
            "MULT",
            "MESH",  # create domain collection
            "SURF",  # load SURFs
            "CATF",  # load additional SURFs
            "OBST",
            "GEOM",
            None,
        )
        for fds_label in fds_labels:
            import_helper.import_by_fds_label(
                context=context,
                sc=self,
                fds_list=fds_list,
                fds_label=fds_label,
                co_description=co_description,
                set_tmp=set_tmp,  # TODO or Scene var?
            )

        # Restore fds case dir, to avoid overwriting imported case
        if filepath:
            self.bf_config_directory = bf_config_directory

    @classmethod
    def register(cls):
        """!
        Register related Blender properties.
        @param cls: class to be registered.
        """
        Scene.bf_namelists = cls.bf_namelists
        Scene.to_fds_list = cls.to_fds_list
        Scene.to_fds = cls.to_fds
        Scene.from_fds = cls.from_fds
        Scene.bf_file_version = IntVectorProperty(
            name="BlenderFDS File Version", size=3
        )

    @classmethod
    def unregister(cls):
        """!
        Unregister related Blender properties.
        @param cls: class to be unregistered.
        """
        del Scene.bf_file_version
        del Scene.from_fds
        del Scene.to_fds
        del Scene.to_fds_list
        del Scene.bf_namelists
