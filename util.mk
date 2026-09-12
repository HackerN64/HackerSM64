# util.mk - Miscellaneous utility functions for use in Makefiles

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
find-mips-prefix = $(shell test -n "$(call find-command,$(1)-ld)" && test -n "$(call find-command,$(1)-gcc)" && echo $(1))

MIPS_PREFIX_CANDIDATES := mips64-elf mips-n64 mips64 mips-linux-gnu mips64-linux-gnu mips64-none-elf mips mips-suse-linux
define _find-mips-toolchain-internal
$(eval DETECTED_PREFIX :=)
$(eval _unused := $(foreach prefix,$(MIPS_PREFIX_CANDIDATES),\
  $(if $(DETECTED_PREFIX),,\
    $(if $(call find-mips-prefix,$(prefix)),\
      $(eval DETECTED_PREFIX := $(prefix)-)))))
$(if $(DETECTED_PREFIX),,$(error Unable to detect a suitable MIPS toolchain installed))
$(DETECTED_PREFIX)
endef

find-mips-toolchain = $(strip $(call _find-mips-toolchain-internal))
