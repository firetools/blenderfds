#!/usr/bin/env python3

"""!
Split MESH parameters according to nsplits.
"""


def _split_cells(ncell, nsplit):
    """!
    Split ncell cells in nsplit parts, conserving the total number ncell of cells
    """
    if ncell < nsplit * 3:
        raise Exception("Too few cells")
    base = ncell // nsplit
    remainder = ncell % nsplit
    ncells = list((base,)) * nsplit
    for i in range(0, remainder):
        ncells[i] += 1
    return ncells


def split_mesh(hid, ijk, nsplits, xb):
    """!
    Split ijk cells along the axis in nsplits, calc new ids, ijks and xbs
    """
    ijks, xbs = list(), list()
    # Split cells along axis
    icells = _split_cells(ijk[0], nsplits[0])
    jcells = _split_cells(ijk[1], nsplits[1])
    kcells = _split_cells(ijk[2], nsplits[2])
    # Prepare new mesh ijks and origins (in cell number)
    origins = list()
    origin_i, origin_j, origin_k = 0, 0, 0
    for i in icells:
        for j in jcells:
            for k in kcells:
                ijks.append((i, j, k))
                origins.append((origin_i, origin_j, origin_k))
                origin_k += k
            origin_k = 0
            origin_j += j
        origin_j = 0
        origin_i += i
    # Calc cell sizes along axis
    cs = (
        (xb[1] - xb[0]) / ijk[0],
        (xb[3] - xb[2]) / ijk[1],
        (xb[5] - xb[4]) / ijk[2],
    )
    # Prepare xbs
    for i, origin in enumerate(origins):
        xbs.append(
            (
                origin[0] * cs[0],
                (origin[0] + ijks[i][0]) * cs[0],
                origin[1] * cs[1],
                (origin[1] + ijks[i][1]) * cs[1],
                origin[2] * cs[2],
                (origin[2] + ijks[i][2]) * cs[2],
            )
        )
    # Prepare ids
    if len(xbs) > 1:
        ids = (f"{hid}_{i}" for i in range(len(xbs)))
    else:
        ids = (hid,)
    return ids, ijks, xbs


def test():
    ids, ijks, xbs = split_mesh(
        hid="H",
        ijk=(10, 10, 10),
        nsplits=(2, 3, 1),
        xb=(0.0, 10.0, 0.0, 10.0, 0.0, 10.0),
    )
    assert tuple(ids) == (
        "H_0",
        "H_1",
        "H_2",
        "H_3",
        "H_4",
        "H_5",
    )
    assert ijks == [
        (5, 4, 10),
        (5, 3, 10),
        (5, 3, 10),
        (5, 4, 10),
        (5, 3, 10),
        (5, 3, 10),
    ]
    assert xbs == [
        (0.0, 5.0, 0.0, 4.0, 0.0, 10.0),
        (0.0, 5.0, 4.0, 7.0, 0.0, 10.0),
        (0.0, 5.0, 7.0, 10.0, 0.0, 10.0),
        (5.0, 10.0, 0.0, 4.0, 0.0, 10.0),
        (5.0, 10.0, 4.0, 7.0, 0.0, 10.0),
        (5.0, 10.0, 7.0, 10.0, 0.0, 10.0),
    ]


if __name__ == "__main__":
    test()
