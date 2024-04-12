# convert binary to object file
$(BUILD_DIR)/%.szp.o: $(BUILD_DIR)/%.bin
	$(call print,Converting BIN to ELF:,$<,$@)
	$(V)$(LD) -r -b binary $< -o $@