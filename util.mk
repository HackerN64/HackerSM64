# util.mk - Miscellaneous utility functions for use in Makefiles

# Usage: $(call find-rom,[us|eu|jp|sh])
# Appends the EXTRACT_ASSETS_VERSIONS variable if rom version is found
define find-rom
  ifneq (,$$(shell python3 tools/detect_baseroms.py $1))
    $(info Locating ROM for region $1...)
    EXTRACT_ASSETS_VERSIONS += $1
  endif
endef

# Throws an error if the value of the variable named by $(1) is not in the list given by $(2)
define validate-option
  # value must be part of the list
  ifeq ($$(filter $($(1)),$(2)),)
    $$(error Value of $(1) must be one of the following: $(2))
  endif
  # value must be a single word (no whitespace)
  ifneq ($$(words $($(1))),1)
    $$(error Value of $(1) must be one of the following: $(2))
  endif
endef

# Returns the path to the command $(1) if exists. Otherwise returns an empty string.
find-command = $(shell which $(1) 2>/dev/null)
