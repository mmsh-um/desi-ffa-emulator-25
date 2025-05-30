
import subprocess

subprocess.run("cp -r /global/cfs/cdirs/desi/survey/catalogs/main/mocks/FAemu_preliminary/sikandar/Emulator_Cat_Storage/fofin/* fof_v1.0/in/", shell = True)

subprocess.run("cp -r /global/cfs/cdirs/desi/survey/catalogs/main/mocks/FAemu_preliminary/sikandar/Emulator_Cat_Storage/fofout/* fof_v1.0/out/", shell = True)

subprocess.run("cp -r /global/cfs/cdirs/desi/survey/catalogs/main/mocks/FAemu_preliminary/sikandar/Emulator_Cat_Storage/emuout/* emulate_bitw_v1.1/out/", shell = True)