# Compress binary file
$(BUILD_DIR)/%.szp: $(BUILD_DIR)/%.bin
	$(call print,Compressing:,$<,$@)
	$(V)$(MIO0TOOL) $< $@

# convert binary szp to object file
$(BUILD_DIR)/%.szp.o: $(BUILD_DIR)/%.szp
	$(call print,Converting MIO0 to ELF:,$<,$@)
	$(V)$(LD) -r -b binary $< -o $@