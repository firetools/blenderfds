#!/usr/bin/env python3

"""!
Align MESHes correctly according to cells.
"""

from ...types import BFException
from .calc_meshes import get_n_for_poisson

# Mesh alignment:
#
# Before:
#   |   |rcs|   |   | ri  ref mesh
#   ·---·---·---·---·
#  rx0   <-rl->    rx1
# mx0      <-ml->      mx1
#  ·------·------·------·
#  |      | mcs  |      | mi  other mesh
# ---> axis

# Either protect rl or rcs.
# Instead ml and mcs are changed for alignment.

# After:
#  |   |   |   |   |
#  ·---·---·---·---·-------·
#  |       |       |       |

# Not allowed:
#  |    |     |    |
#  ·----·--·--·----·
#  |       |       |

#  |   |   |   |
#  ·---·---·---·---·
#  |       |       |


def _align_along_axis(ri, rx0, rx1, mi, mx0, mx1, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along an axis."""
    # Init
    if rx0 >= rx1 or mx0 >= mx1:
        raise Exception("Coordinate order error")
    rl, ml = rx1 - rx0, mx1 - mx0  # lengths
    rcs, mcs = rl / ri, ml / mi  # cell sizes
    # Coarsening ratio
    if mcs / rcs < 0.501:  # same or coarser allowed, protect from float err
        raise BFException(
            None, f"Destination MESHes should be coarser than source: {mcs} > {rcs}"
        )
    n = round(mcs / rcs)
    # Set ref cell count multiple of n
    # to allow full cover of coarse cells by ref cells
    ri = round(ri / n) * n
    if poisson:
        ri = get_n_for_poisson(ri)
    if protect_rl:  # protect ref length
        rcs = rl / ri  # reduce ref cell size
    else:
        rl = rcs * ri  # extend ref length due to updated ri
        rx1 = rx0 + rl
    # Calc new coarse cell size from ref cell size
    mcs = rcs * n
    # Calc new coarse cell count,
    # trying to keep ml as close as possible to the original
    mi = round(ml / mcs)
    if poisson:
        mi = get_n_for_poisson(mi)
    # Align coarse mesh positions to the ref mesh
    mx0 = rx0 + round((mx0 - rx0) / mcs) * mcs
    ml = mcs * mi  # extend other mesh due to updated mi
    mx1 = mx0 + ml
    return ri, rx0, rx1, mi, mx0, mx1


def _align_along_x(rijk, rxb, mijk, mxb, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along axis x."""
    rijk[0], rxb[0], rxb[1], mijk[0], mxb[0], mxb[1] = _align_along_axis(
        ri=rijk[0],
        rx0=rxb[0],
        rx1=rxb[1],
        mi=mijk[0],
        mx0=mxb[0],
        mx1=mxb[1],
        poisson=False,  # not needed along x
        protect_rl=protect_rl,
    )


def _align_along_y(rijk, rxb, mijk, mxb, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along axis y."""
    rijk[1], rxb[2], rxb[3], mijk[1], mxb[2], mxb[3] = _align_along_axis(
        ri=rijk[1],
        rx0=rxb[2],
        rx1=rxb[3],
        mi=mijk[1],
        mx0=mxb[2],
        mx1=mxb[3],
        poisson=poisson,  # needed along y
        protect_rl=protect_rl,
    )


def _align_along_z(rijk, rxb, mijk, mxb, poisson=False, protect_rl=False):
    """!Align coarse MESH to fixed ref MESH along axis z."""
    rijk[2], rxb[4], rxb[5], mijk[2], mxb[4], mxb[5] = _align_along_axis(
        ri=rijk[2],
        rx0=rxb[4],
        rx1=rxb[5],
        mi=mijk[2],
        mx0=mxb[4],
        mx1=mxb[5],
        poisson=poisson,  # needed along z
        protect_rl=protect_rl,
    )


# rx0 rx1
#  .---.
#       delta .----.
#            mx0  mx1


def _is_far(rxb, mxb, deltas):
    return (
        rxb[0] - mxb[1] > deltas[0]
        or mxb[0] - rxb[1] > deltas[0]  # x
        or rxb[2] - mxb[3] > deltas[1]
        or mxb[2] - rxb[3] > deltas[1]  # y
        or rxb[4] - mxb[5] > deltas[2]
        or mxb[4] - rxb[5] > deltas[2]  # z
    )


def align_meshes(rijk, rxb, mijk, mxb, poisson=False, protect_rl=False):
    """!
    Function to align meshes.
    @param rijk:  ijk of the ref mesh.
    @param rxb: xbs of the ref mesh.
    @param mijk: ijk of the other mesh.
    @param mxb: xbs of the other mesh.
    @param poisson: True for respecting the Poisson constraint.
    @param protect_rl: True to protect ref length.
    @return return new rijk, rxb, mijk and mxb.
    """
    # Init
    deltas = (  # rcs
        abs(rxb[0] - rxb[1]) / rijk[0],
        abs(rxb[2] - rxb[3]) / rijk[1],
        abs(rxb[4] - rxb[5]) / rijk[2],
    )
    msgs = list()
    # Are meshes far apart?
    if _is_far(rxb=rxb, mxb=mxb, deltas=deltas):
        msgs.append("Far apart, alignment not needed")
        return rijk, rxb, mijk, mxb, msgs
    else:
        msgs.append("Close enough, alignment needed")
    if protect_rl:
        msgs.append("Ref MESH cell size updated, Object size untouched")
    else:
        msgs.append("Ref MESH size updated, cell size untouched")
    # Transform to list()
    rijk, rxb, mijk, mxb = list(rijk), list(rxb), list(mijk), list(mxb)
    # If mesh sides are close, then snap them
    # otherwise align their meshes
    if abs(rxb[0] - mxb[1]) <= deltas[0]:  # -x close?
        msgs.append(f"Close enough at ref x0, snapped")
        mxb[1] = rxb[0]
    elif abs(mxb[0] - rxb[1]) <= deltas[0]:  # +x close?
        msgs.append(f"Close enough at ref x1, snapped")
        mxb[0] = rxb[1]
    else:
        msgs.append("Aligned along x axis")
        _align_along_x(rijk, rxb, mijk, mxb, poisson, protect_rl)
    if abs(rxb[2] - mxb[3]) <= deltas[1]:  # -y close?
        msgs.append(f"Close enough at ref y0, snapped")
        mxb[3] = rxb[2]
    elif abs(mxb[2] - rxb[3]) <= deltas[1]:  # +y close?
        msgs.append(f"Close enough at ref y1, snapped")
        mxb[2] = rxb[3]
    else:
        msgs.append("Aligned along y axis")
        _align_along_y(rijk, rxb, mijk, mxb, poisson, protect_rl)
    if abs(rxb[4] - mxb[5]) <= deltas[2]:  # -z close?
        msgs.append(f"Close enough at ref z0, snapped")
        mxb[5] = rxb[4]
    elif abs(mxb[4] - rxb[5]) <= deltas[2]:  # +z close?
        msgs.append(f"Close enough at ref z1, snapped")
        mxb[4] = rxb[5]
    else:
        msgs.append("Aligned along z axis")
        _align_along_z(rijk, rxb, mijk, mxb, poisson, protect_rl)
    return rijk, rxb, mijk, mxb, msgs


def test():
    rijk, rxb, mijk, mxb, msgs = align_meshes(
        rijk=[15, 37, 51],
        rxb=[0.0, 5.0, 0.0, 5.0, 0.0, 5.0],
        mijk=[9, 38, 20],
        mxb=[0.0, 5.0, 0.0, 5.0, 5.01, 10.0],
        poisson=True,
        protect_rl=True,
    )
    assert rijk == [16, 40, 51]
    assert rxb == [0.0, 5.0, 0.0, 5.0, 0.0, 5.0]
    assert mijk == [8, 40, 20]
    assert mxb == [0.0, 5.0, 0.0, 5.0, 5.0, 10.0]
    assert msgs == [
        "Close enough, alignment needed",
        "Ref MESH cell size updated, Object size untouched",
        "Aligned along x axis",
        "Aligned along y axis",
        "Close enough at ref z1, snapped",
    ]


if __name__ == "__main__":
    test()
