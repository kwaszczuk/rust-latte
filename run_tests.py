import os
import sys
import subprocess

BAD_TESTS_DIRS = ['tests/bad']
GOOD_TESTS_DIRS = ['tests/good']

wrongs = []
prog = sys.argv[1]

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
            wrongs.append(fpath)

if len(wrongs) != 0:
    print('Wrong results in the following tests:')
    for t in wrongs:
        print(f"  - {t}")
else:
    print('All tests passed!')
