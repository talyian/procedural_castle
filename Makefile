run: bin/castle.exe bin/OpenTK.dll bin/proj.dll
	mono bin/castle.exe

profile: bin/castle.exe bin/OpenTK.dll bin/proj.dll
	mono --profile=log:sample bin/castle.exe
	mprof-report output.mlpd | less

bin/castle.exe: castle.fs
	fsharpc castle.fs -o bin/castle.exe -r bin/proj.dll -r packages/OpenTK/lib/NET40/OpenTK.dll

bin/OpenTK.dll: packages
	cp packages/OpenTK/lib/NET40/OpenTK.dll bin

bin/proj.dll: opengl.fs base.fs packages
	fsharpc base.fs opengl.fs -a -o bin/proj.dll -r packages/OpenTK/lib/NET40/OpenTK.dll --nowarn:9

packages: paket.dependencies .paket/paket.exe
	mono .paket/paket.exe install

.paket/paket.exe: .paket/paket.bootstrapper.exe
	mono .paket/paket.bootstrapper.exe
