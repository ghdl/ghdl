#!/usr/bin/env python

import argparse
import glob
import subprocess
import select
import os
import os.path
import sys
import time

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

def run(keep):
    # List of tests to run
    tests = glob.glob('*[0-9]')

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
            print('Starting {} [{}+{} err/{}]'.format(
                test, nbr_run, nbr_err, nbr_tests))
            j.start()
        elif len(jobs) > 0:
            # Wait for output or end of job.
            assert len(poll) == NUMJOBS or len(tests) == 0
            res = select.select(poll, [], [])
            done = set()
            for fd in res[0]:
                d = os.read(fd.fileno(), 1024)
                if len(d) == 0:
                    # EOF => end of job.
                    for j in jobs:
                        if fd == j.out_fd:
                            done.add(j)
                            break
                else:
                    # Gather output
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
                    if not keep:
                        tests = []
                jobs.remove(j)
                nbr_run += 1
    end_time = time.time()

    print('{}/{} tests run in {} sec, {} failures'.format(
        nbr_run, nbr_tests, end_time - start_time, nbr_err))
    if failures:
        print('Failure: {}'.format(failures))
        return False
    return True


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="parallel test driver")
    parser.add_argument("-k", "--keep",
                        help="keep running", action='store_true')
    parser.add_argument("-j", "--jobs", type=int, default=4,
                        help="number of parallel jobs")
    args = parser.parse_args()
    NUMJOBS = args.jobs
    if not run(args.keep):
        sys.exit(1)
