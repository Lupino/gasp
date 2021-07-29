PROJECTS0=$(patsubst %/main.gasp, %, $(wildcard */main.gasp))
PROJECTS=$(filter-out microbit,$(PROJECTS0))
TARGETS=$(patsubst %,dist/%.hex,$(PROJECTS))
CLEANS=$(patsubst %,%/clean,$(PROJECTS))
BUILDS=$(patsubst %,%/build,$(PROJECTS))

all: $(TARGETS)

%/build/Makefile: %/main.gasp
	cd $(patsubst %/build/Makefile,%,$@) && gasp compile

$(shell mkdir -p dist)
dist/%.hex: %/build/Makefile
	make -C $(dir $<)
	cp $(dir $<)*.hex dist

%/clean:
	cd $(dir $@) && gasp clean

clean: $(CLEANS)
	rm -r dist
