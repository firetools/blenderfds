import_from_fds_tree(
    test=test,
    path = "../../../fds-master/Verification/",
    compare_with_ref=False,
    run_fds = False,
    exclude_files = (
        'geom_texture.fds','geom_elev.fds','geom_simple2.fds',
        'geom_time2.fds','geom_azim.fds','geom_time4.fds','geom_scale.fds',
        'geom_simple.fds','geom_time3.fds','geom_texture2.fds','geom_time.fds'
    ),
    exclude_dirs= None,
)
