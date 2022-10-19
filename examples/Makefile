PROJECTS0=$(patsubst %/main.gasp, %, $(wildcard */main.gasp))
PROJECTS=$(filter-out microbit,$(PROJECTS0))
TARGETS=$(patsubst %,dist/%.hex,$(PROJECTS))
CLEANS=$(patsubst %,%/clean,$(PROJECTS))
BUILDS=$(patsubst %,%/build,$(PROJECTS))

all: $(TARGETS)

%/build/stage2/Makefile: %/main.gasp
	cd $(patsubst %/build/stage2/Makefile,%,$@) && gasp compile

$(shell mkdir -p dist)
dist/%.hex: %/build/stage2/Makefile
	make -C $(dir $<) PROJECT_NAME=$(patsubst %/build/stage2/Makefile,%,$<)
	cp $(dir $<)*.hex dist

%/clean:
	cd $(dir $@) && gasp clean

clean: $(CLEANS)
	rm -r dist
