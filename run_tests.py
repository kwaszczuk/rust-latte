import os
import sys
import subprocess

BAD_TESTS_DIRS = ['tests/bad', 'tests/my/bad']
GOOD_TESTS_DIRS = [
    'tests/good',
    'tests/students/good/basic',
    'tests/extensions/arrays1',
    'tests/students/good/arrays',
    'tests/my/good',
]

wrongs = []
prog = sys.argv[1]
opt = sys.argv[2]

for t_dir in BAD_TESTS_DIRS:
    fpaths = sorted([ os.path.join(t_dir, f) for f in os.listdir(t_dir) ])
    for fpath in fpaths:
        result = subprocess.run([f"./{prog}", fpath], capture_output=True)
        if result.returncode != 1:
            wrongs.append(fpath)

for t_dir in GOOD_TESTS_DIRS:
    fpaths = sorted([ os.path.join(t_dir, f) for f in os.listdir(t_dir) if '.lat' in f ])
    for fpath in fpaths:
        result = subprocess.run([f"./{prog}", fpath], capture_output=True)
        if result.returncode != 0:
            print(result.stderr)
            wrongs.append(fpath)
            continue
        if os.path.isfile(fpath.replace('.lat', '.input')):
            continue
        with open(fpath.replace('.lat', '.output')) as output_f:
            exp = output_f.read().encode()
            if opt == 'llvm':
                result = subprocess.run(["lli", fpath.replace(".lat", ".bc")], capture_output=True)
            elif opt == 'x86':
                result = subprocess.run([fpath.replace(".lat", "")], capture_output=True)
            if exp != result.stdout:
                wrongs.append(fpath)

if len(wrongs) != 0:
    print('Wrong results in the following tests:')
    for t in wrongs:
        print(f"  - {t}")
else:
    print('All tests passed!')
