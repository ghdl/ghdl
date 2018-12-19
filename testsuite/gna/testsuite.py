#!/usr/bin/env python

import glob
import subprocess
import select
import os
import os.path
import time

DIRS=['bug*', 'sr*', 'deb*', 'ticket*', 'issue*']
NUMJOBS=4

class Job(object):
    def __init__(self, dirname, poll):
        self.dirname = dirname
        self.poll = poll
        self.out = ''

    def start(self):
        self.p = subprocess.Popen(
            ['./testsuite.sh'],
            stdout=subprocess.PIPE, stderr=subprocess.STDOUT, close_fds=True,
            cwd=self.dirname)
        self.out_fd = self.p.stdout
        self.poll.extend([self.out_fd])

    def wait(self):
        self.poll.remove(self.out_fd)
        return self.p.wait()

def run():
    # List of tests to run
    tests = []
    for d in DIRS:
        tests.extend(glob.glob(d))

    start_time = time.time()
    jobs = []
    poll = []
    failures = []
    nbr_tests = len(tests)
    nbr_run = 0
    nbr_err = 0
    while len(tests) != 0 or len(jobs) != 0:
        # Start as many jobs as possible
        if len(tests) > 0 and len(jobs) < NUMJOBS:
            test = tests.pop(0)
            j = Job(test, poll)
            jobs.append(j)
            print('Starting {}'.format(test))
            j.start()
        elif len(jobs) > 0:
            #print('polling {}'.format(poll))
            assert len(poll) == NUMJOBS or len(tests) == 0
            t1 = time.time()
            res = select.select(poll, [], [])
            t2 = time.time()
            #print('poll res {}sec: {}'.format(t2 - t2, res[0]))
            done = set()
            for fd in res[0]:
                d = os.read(fd.fileno(), 1024)
                if len(d) == 0:
                    for j in jobs:
                        if fd == j.out_fd:
                            done.add(j)
                            break
                else:
                    for j in jobs:
                        if fd == j.out_fd:
                            j.out += d
            for j in done:
                print('Finish: {}'.format(j.dirname))
                print(j.out)
                code = j.wait()
                if code != 0:
                    print('############### Error for {}'.format(j.dirname))
                    nbr_err += 1
                    failures.append(j.dirname)
                    tests = []
                jobs.remove(j)
                nbr_run += 1
    end_time = time.time()

    print('{}/{} tests run in {} sec, {} failures'.format(
        nbr_run, nbr_tests, end_time - start_time, nbr_err))
    if failures:
        print('Failure: {}'.format(failures))


if __name__ == '__main__':
    run()

