PROJECT = pivot

# Dependencies

PKG_FILE_URL = https://gist.github.com/CamShaft/815c139ad3c1ccf13bad/raw/packages.tsv

DEPS = fast_key riakc riakou websaferl

dep_fast_key = pkg://fast_key master
dep_riakc = pkg://riakc 44a725b71816170e44b9eacccfc97f255b75e024
dep_riakou = pkg://riakou master
dep_websaferl = pkg://websaferl master

include erlang.mk
