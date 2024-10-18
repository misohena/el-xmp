XPSDK = c:/home/k-aki/work/20240927-xmp-toolkit/XMP-Toolkit-SDK-2023.12
SAMPLE_BIN_DIR = $(XPSDK)/samples/target/windows_x64/Release
DUMPMAINXMP = $(SAMPLE_BIN_DIR)/DumpMainXMP

.PHONY: dump
dump: xmp-test-value-types.dump\
	xmp-test-syntax-property-elements.dump

%.dump: %.xmp
	$(DUMPMAINXMP) $< > $@

