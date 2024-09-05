# Compress binary file
$(BUILD_DIR)/%.szp: $(BUILD_DIR)/%.bin
	$(call print,Compressing:,$<,$@)
	$(V)$(LZ4TPACK) $< $@

# convert binary szp to object file
$(BUILD_DIR)/%.szp.o: $(BUILD_DIR)/%.szp
	$(call print,Converting LZ4T to ELF:,$<,$@)
	$(V)$(LD) -r -b binary $< -o $@
