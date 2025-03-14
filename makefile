# define command for cmake

.PHONY: config
config:
		cmake -B build -DCMAKE_BUILD_TYPE=Release

.PHONY: build
build: config
		cmake --build build

.PHONY: install
install: build
		cmake --install build

.PHONY: debug
debug: clean
		cmake -B build -DCMAKE_BUILD_TYPE=Debug
		cmake --build build
		cmake --install build

TEST_FLAG =

.PHONY: test1 test2
test1:
		./bin/solver --input=./case/NACA0012.nml ${TEST_FLAG}
test2:
		./bin/solver --input=./case/Riemann-2D.nml ${TEST_FLAG}

test2-sp:
		mpirun -np 1 ./bin/solver --input=./case/Riemann-2D.nml ${TEST_FLAG}
test2-mpi:
		mpirun -np 5 ./bin/solver --input=./case/Riemann-2D.nml ${TEST_FLAG}
.PHONY: clean
clean:
		rm -rf build
		rm -rf bin

# for fortls

.PHONY: refresh refresh_all
refresh:
		rm -rf mod/class_*.mod
		rm -rf mod/class_*.smod
		cp ext/CGNS-4.4.0/include/cgns.mod mod/cgns.mod
refresh_base:
		rm -rf mod/class_*.mod
		rm -rf mod/class_*.smod
		rm -rf mod/global_*.mod
		rm -rf mod/global_*.smod
		rm -rf mod/interface_*.mod
		rm -rf mod/interface_*.smod
		rm -rf mod/method_*.mod
		rm -rf mod/method_*.smod
refresh_all:
		rm -rf mod/*.mod
		rm -rf mod/*.smod
		cp ext/CGNS-4.4.0/include/cgns.mod mod/cgns.mod
