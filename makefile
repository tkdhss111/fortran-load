CMAKE       := cmake -GNinja .. -DCMAKE_BUILD_WITH_INSTALL_RPATH=ON
CMAKE_RLS   := $(CMAKE) -DCMAKE_BUILD_TYPE=Release
CMAKE_DBG   := $(CMAKE) -DCMAKE_BUILD_TYPE=Debug
MKDIR_BUILD := mkdir -p build && cd build

.PHONY: test install

release:
	$(MKDIR_BUILD) && $(CMAKE_RLS) && ninja

test: 
	$(MKDIR_BUILD) && $(CMAKE_DBG) && ninja && ctest -VV

install:
	cd build && ninja install

uninstall:
	cd build && xargs rm < install_manifest.txt

clean:
	rm -r build
