# util.mk - Miscellaneous utility functions for use in Makefiles

# Checks if the assets have been extracted
define extract-assets
  ifneq (,$$(shell python3 tools/detect_baseroms.py $1))
      $(warning "Extracting assets from $1 ROM...\n")
      DUMMY != $$(PYTHON) extract_assets.py $1 >&2 || echo FAIL
      ifeq ($$(DUMMY),FAIL)
        $$(error Failed to extract assets from $1 ROM)
      endif
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
