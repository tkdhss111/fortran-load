BIN=fortran-load
MKFILE:=-C .. -f makefile

run: clean release
	../build/app/$(BIN) \
  --wd "/mnt/DATA/load" \
  --table "TEPCO"

debug:
	make debug $(MKFILE)

release:
	make release $(MKFILE)

test:
	make test $(MKFILE)

install:
	make install $(MKFILE)

uninstall:
	make uninstall $(MKFILE)

clean:
	make clean $(MKFILE)
