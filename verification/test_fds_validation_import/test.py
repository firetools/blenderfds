import_from_fds_tree(
    test=test,
    path="../../../fds-master/Validation/",
    compare_with_ref=False,
    run_fds = False,
    exclude_files = None,
    exclude_dirs=("Crown_Fires", "Askervein_Hill"),
)


# Exclusions:
# Crown_Fires are huge
# Askervein_hills have bingeom in another repository
