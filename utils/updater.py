# SPDX-License-Identifier: GPL-3.0-or-later

"""!
BlenderFDS, utils to update the addon.
"""

import addon_utils
import logging, json, ssl, platform, tempfile, os, zipfile, shutil
import urllib.request, urllib

from ..config import ADDON_NAME, REPO_URL, RELEASES_URL, BRANCHES, MIN_RELEASE_VERSION

log = logging.getLogger(__name__)


def get_branch_url(branch):
    """Get branch zipball url."""
    return f"{REPO_URL}/zipball/{branch}"


def get_tags():
    """Get all tags, add branches."""
    tags = get_api(RELEASES_URL) or list()  # get release tags from github
    tags = list(t for t in tags if get_version_tuple(t["tag_name"]) > MIN_RELEASE_VERSION)
    for b in BRANCHES:  # append branches
        tags.append({"name": b.title(), "zipball_url": get_branch_url(b)})
    return tags


def get_raw(url):
    """All API calls to base url."""
    try:
        request = urllib.request.Request(url)
        context = ssl._create_unverified_context()
        request.add_header("User-Agent", "Python/" + str(platform.python_version()))
        with urllib.request.urlopen(request, context=context, timeout=5) as response:
            result_string = response.read()
    except urllib.error.HTTPError as err:
        if str(err.code) == "403":
            raise IOError("HTTP error (access denied)")
        else:
            raise IOError("HTTP error")
    except urllib.error.URLError as err:
        reason = str(err.reason)
        if "TLSV1_ALERT" in reason or "SSL" in reason.upper():
            raise IOError("Connection rejected")
        else:
            raise IOError("URL error, check internet connection")
    except Exception as err:
        raise IOError(f"Network error: {err}")
    return result_string.decode()


def get_api(url):
    """Result of all api calls, decoded into json format."""
    get = get_raw(url)
    if get is not None:
        try:
            return json.JSONDecoder().decode(get)
        except Exception as err:
            raise IOError("API response has invalid JSON format")


def url_retrieve(url_file, filepath):
    """Custom urlretrieve implementation."""
    chunk = 1024 * 8
    f = open(filepath, "wb")
    while True:
        data = url_file.read(chunk)
        if not data:
            break
        f.write(data)
    f.close()


def download(url_file, filepath):
    """Download url_file to filepath."""
    try:
        request = urllib.request.Request(url_file)
        context = ssl._create_unverified_context()
        request.add_header("User-Agent", "Python/" + str(platform.python_version()))
        with urllib.request.urlopen(request, context=context, timeout=10) as response:
            url_retrieve(url_file=response, filepath=filepath)
    except urllib.error.HTTPError as err:
        if str(err.code) == "403":
            raise IOError("HTTP error (access denied)")
        else:
            raise IOError("HTTP error")
    except urllib.error.URLError as err:
        reason = str(err.reason)
        if "TLSV1_ALERT" in reason or "SSL" in reason.upper():
            raise IOError("Connection rejected")
        else:
            raise IOError("URL error, check internet connection")
    except Exception as err:
        raise IOError(f"Error while downloading: {err}")


def unzip(filepath, path):
    """Unzip filepath zip to path."""
    if not zipfile.is_zipfile(filepath):
        raise IOError("Not a zip file, cannot extract")
    try:
        with zipfile.ZipFile(filepath, "r") as zfile:
            zfile.extractall(path=path)
            zpath = os.path.join(path, zfile.namelist()[0])
            log.debug(f"unzipped zpath: {zpath}")
    except Exception as err:
        raise Exception(f"Error while unzipping: {err}")
    if not os.path.isdir(zpath):
        raise IOError("Unzipped file does not contain a directory")
    if not os.path.isfile(os.path.join(zpath, "__init__.py")):
        raise IOError("Unzipped file does not contain a valid Blender addon")
    return zpath


def get_addon_path(name):
    for m in addon_utils.modules():
        if m.bl_info["name"] == name:
            filepath = m.__file__
            return os.path.dirname(os.path.abspath(filepath))
    raise ValueError(f"Cannot find addon path: {name}")


def backup_addon(addon_path, backup_path):
    try:
        shutil.copytree(src=addon_path, dst=backup_path, symlinks=False)
    except Exception as err:
        raise IOError(f"Cannot create backup copy: {err}")


def restore_addon(addon_path, backup_path):
    try:
        shutil.rmtree(addon_path)
        shutil.copytree(src=backup_path, dst=addon_path, symlinks=False)
    except Exception as err:
        raise IOError(f"Install error, cannot restore backup copy: {err}")


def install_addon(url_file):

    # Get addon_path and check it is not a link
    addon_path = get_addon_path(name=ADDON_NAME)
    log.debug(f"addon_path: {addon_path}")
    if os.path.islink(addon_path):
        raise IOError(f"Current addon path is a link, not a directory: {addon_path}")

    zip_filename = "source.zip"
    with tempfile.TemporaryDirectory() as tmp_path:

        # Download and unzip chosen version
        zip_filepath = os.path.join(tmp_path, zip_filename)
        log.debug(f"zip_filepath: {zip_filepath}")
        download(url_file=url_file, filepath=zip_filepath)
        zpath = unzip(filepath=zip_filepath, path=tmp_path)

        # Backup current addon
        backup_path = os.path.join(tmp_path, "backup")
        log.debug(f"backup_filepath: {backup_path}")
        backup_addon(addon_path=addon_path, backup_path=backup_path)

        # Install new version
        shutil.rmtree(addon_path)
        try:
            shutil.copytree(zpath, addon_path)
        except Exception as err:
            restore_addon(addon_path=addon_path, backup_path=backup_path)
            raise Exception(f"Install error, addon restored: {err}")


def get_version_tuple(version_text):
    """Convert text into a tuple of numbers (int).

    Should go through string and remove all non-integers, and for any
    given break split into a different section.
    """
    segments = list()
    tmp = ""
    for char in str(version_text):
        if char.isdigit():
            tmp += char
        else:
            # Number followed by non char
            if len(tmp) > 0:
                segments.append(int(tmp))
                tmp = ""
    # Last one
    if len(tmp) > 0:
        segments.append(int(tmp))

    if len(segments) == 0:
        raise ValueError(f"Version not found: {version_text}")
    return tuple(segments)
