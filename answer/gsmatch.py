#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Gale-Shapley Matching Algorithm

 $Id$
"""
from __future__ import print_function

import os
import sys
import copy
import numpy as np

#_DEBUG_ = True
_DEBUG_ = False

class Element:
    """Element for Gale-Shapley Algorithm
    """
    def __init__(self, nmax):
        # n    : number of available slots
        # pref : list of preference
        # slot : available slot
        self.pref = []
        self.slot = []
        self.n    = nmax

    def set_preference(self, pref):
        "set list of preference"
        self.pref = copy.copy(list(pref))

    def pop(self):
        """Pop a candidate from list of preference

        Return the top ranked candidate from the list of preference.
        None is returned if the list is empty.
        """
        if len(self.pref) > 0:
            c = self.pref[0]
            self.pref.remove(c)
        else:
            c = None
        return c

    def push(self, foo):
        """Push a candidate to list of slot

        If the candidate is in the list of preference, it tries to push it in
        the list of available slots. The slot is sorted according to the
        preference. If the number of available slots is not sufficient, the
        worst ranked candidate will be pushed out or rejected.

        Return tuple of (accepted, rejected) candidates or None if no one was
        rejected.
        """
        if self.pref.count(foo) != 0 and self.slot.count(foo) == 0:
            self.slot.append(foo)
            self.slot.sort(key=lambda x: self.pref.index(x))
            discarded = self.slot[self.n:]
            self.slot = self.slot[0:self.n]
        else:
            discarded = [foo]
        if len(discarded) == 0:
            return None
        elif len(discarded) == 1:
            return discarded[0]
        else:
            raise ValueError('Multiple values were pushed out from the slots')


def print_debug(*args, **kwargs):
    if _DEBUG_:
        print(*args, **kwargs)


def match(x, y):
    """Apply Gale-Shapley Matching Algorithm
    """
    nx = x.shape[0]
    ny = y.shape[0]
    xx = copy.deepcopy(x)
    yy = copy.deepcopy(y)
    # continue the loop while someone is in the list
    indices = list(range(1, nx+1))
    while len(indices) > 0:
        j = indices.pop()
        #
        # find candidate x
        #
        p = xx[j-1].pop()
        if p is None:
            # no candidate found
            continue
        print_debug('X[%2d] proposed to Y[%2d]' % (j, p))
        #
        # push candidate to an element of y
        #
        r = yy[p-1].push(j)
        if r is None:
            # proposed candidate was accepted
            print_debug('Y[%2d] accepted X[%2d]' % (p, j))
            xx[j-1].slot.append(p)
        elif r == j:
            # proposed candidate was rejected
            print_debug('Y[%2d] rejected X[%2d]' % (p, r))
            indices.append(r)
        else:
            # someone else was rejected
            print_debug('Y[%2d] accepted X[%2d] and rejected X[%2d]' % (p, j, r))
            indices.append(r)
            xx[j-1].slot.append(p)
            xx[r-1].slot.pop()
    #
    # pack results
    #
    results = dict()
    for i in range(1, ny+1):
        results[i] = tuple(yy[i-1].slot)
    # get leftover
    results[-1] = []
    for i in range(1, nx+1):
        if len(xx[i-1].slot) == 0:
            results[-1].append(i)
    results[-1] = tuple(results[-1])
    return results


def print_marriage(res):
    n = len(res) - 1
    for i in range(1, n+1):
        print('F[%3d] <=> M[%3d]' % (i, res[i][0]))


def generate_marriage(fn, num=0):
    if num == 0:
        n = 4
        m = 4
        x = np.array([[0, 1, 2, 3],
                      [2, 1, 0, 3],
                      [0, 1, 3, 2],
                      [2, 0, 3, 1]]) + 1
        y = np.array([[0, 1, 2, 3],
                      [1, 0, 3, 2],
                      [1, 2, 0, 3],
                      [0, 3, 2, 1]]) + 1
    else:
        raise ValueError('Error: unrecognized option')

    writefile_marriage(fn, x, y)


def writefile_marriage(fn, x, y):
    if x.shape != y.shape:
        raise ValueError('Error: number of male and female must be the same')
    n  = x.shape[0]

    fn = open(fn, 'w')

    # write number of male/female
    fn.write('%d' % (n))
    fn.write('\n\n')

    # male preference
    for i in range(n):
        for j in range(n):
            fn.write(' %4d' % (x[i,j]))
        fn.write('\n')
    fn.write('\n')

    # female preference
    for i in range(n):
        for j in range(n):
            fn.write(' %4d' % (y[i,j]))
        fn.write('\n')

    fn.close()

def readfile_marriage(fn):
    fp = open(fn, 'r')

    # read number of male/female
    n = 0
    l = fp.readline().strip()
    while l != '':
        n = int(l)
        l = fp.readline().strip()

    # data
    x = np.empty((n,), object)
    y = np.empty((n,), object)

    # male preference
    while l == '':
        l = fp.readline().strip()
    for i in range(n):
        pref = [int(p) for p in l.split()]
        x[i] = Element(1)
        x[i].set_preference(pref)
        l = fp.readline().strip()

    # female preference
    while l == '':
        l = fp.readline().strip()
    for i in range(n):
        pref = [int(p) for p in l.split()]
        y[i] = Element(1)
        y[i].set_preference(pref)
        l = fp.readline().strip()

    return x, y


def print_assignment(res):
    numdept = len(res) - 1
    for i in range(1, numdept+1):
        print('Department[%2d] : [' % (i), end=''),
        for student in res[i]:
            print(' %3d' % (student,), end=''),
        print(']')
    print('')
    print('Failed students:')
    for student in res[-1]:
        print(' %3d' % (student,), end=''),
    print('')


def readfile_assignment(fn):
    """read data for student assignment to department

    fn : filename to read
    """
    fp = open(fn, 'r')

    ##
    ## read department info
    ##
    # read number of departments and subjects
    l = fp.readline().strip()
    while l != '':
        ll = l.split()
        if len(ll) == 2:
            numdept = int(ll[0])
            numsubj = int(ll[1])
            break
        else:
            raise ValueError('Error: unrecognized format')
        # next
        l  = fp.readline().strip()

    # read number of slots and subject weights
    quota  = np.zeros((numdept,), np.int32)
    weight = np.zeros((numdept, numsubj), np.float64)

    l = fp.readline().strip()
    while l == '':
        l = fp.readline().strip()

    for i in range(numdept):
        ll = l.split()
        l  = fp.readline().strip()
        quota[i] = int(ll[0])
        for j in range(numsubj):
            weight[i,j] = float(ll[j+1])

    ##
    ## read student info
    ##
    # read number of students
    while l == '':
        l = fp.readline().strip()
    numstd = int(l.split()[0])

    # read student data
    score = np.zeros((numstd, numsubj), np.float64)
    x = np.empty((numstd,), object)
    y = np.empty((numdept,), object)

    l = fp.readline().strip()
    while l == '':
        l = fp.readline().strip()

    # read student score and preference
    for i in range(numstd):
        ll = l.split()
        l  = fp.readline().strip()
        # score
        for j in range(numsubj):
            score[i,j] = float(ll[j])
        # preference
        ll = np.array(ll[numsubj:], np.int32)
        p = np.count_nonzero(np.greater(ll, 0))
        ll = ll[:p]
        x[i] = Element(1)
        x[i].set_preference(ll)
    fp.close()

    # calculate student ranking by department
    for i in range(numdept):
        pts   = np.sum(weight[i,:][None,:]*score[:,:], axis=1)
        index = np.argsort(-pts)
        rank  = np.arange(numstd)[index] + 1
        y[i]  = Element(quota[i])
        y[i].set_preference(rank)

    return x, y


def writefile_assignment(fn, department, student):
    """write test data for students assignment

    fn         : filename for data output
    department : dictionary containing department info
    student    : dictionary containing student info
    """
    # department data
    numdept = department['numdept']
    numsubj = department['numsubj']
    quota   = department['quota']
    weight  = department['weight']

    # student data
    numstd  = student['numstd']
    average = student['average']
    sigma   = student['sigma']

    if numdept != student['numdept'] or numsubj != student['numsubj']:
        raise ValueError('Error: inconsistent arguments')

    ###
    ### output department data
    ###

    fn = open(fn, 'w')
    fn.write('%3d' % (numdept)) # number of departments
    fn.write('%3d' % (numsubj)) # number of subjects
    fn.write('\n\n')
    # number of slots
    for i in range(numdept):
        fn.write('%3d  ' % (quota[i]))
        for j in range(numsubj):
            fn.write('%5.2f  ' % (weight[i,j]))
        fn.write('\n')
    fn.write('\n')
    fn.write('\n')
    fn.write('\n')


    ###
    ### output student data
    ###

    # score
    xmax  = 99.999
    xmin  =  0.001
    score = np.zeros((numstd, numsubj), dtype=np.float64)
    for i in range(numsubj):
        score[:,i] = np.random.normal(average[i], sigma[i], numstd)
    score[...] = np.where(score > xmax, 2*xmax - score, score)
    score[...] = np.where(score < xmin, 2*xmin - score, score)

    # preference
    npref = np.random.randint(1, numdept+1, numstd)

    # output
    fn.write('%d' % (numstd))
    fn.write('\n\n')
    for i in range(numstd):
        # score for each subject
        for j in range(numsubj):
            fn.write('%5.2f   ' % (score[i,j]))
        # preference
        r = np.argsort(np.random.rand(numdept))
        r[npref[i]+1:] = -2
        for j in range(numdept):
            fn.write(' %4d' % (r[j]+1))
        fn.write('\n')
    fn.close()


def generate_assignment(fn, num=0):
    if num == 0:
        numdept = 10
        numstd  = 40
        numsubj = 3
        quota   = np.array([4, 2, 7, 1, 3, 2, 4, 2, 1, 2], np.int32)
        weight  = np.array(
            [[0.25, 0.25, 0.50],
             [0.50, 0.25, 0.25],
             [0.50, 0.40, 0.10],
             [0.33, 0.33, 0.34],
             [0.50, 0.20, 0.30],
             [0.50, 0.30, 0.20],
             [0.40, 0.30, 0.40],
             [0.33, 0.33, 0.34],
             [0.40, 0.40, 0.20],
             [0.33, 0.33, 0.34]])
        average = np.array([50.0, 40.0, 60.0])
        sigma   = np.array([25.0, 20.0, 10.0])
    elif num == 1:
        numdept = 10
        numstd  = 310
        numsubj = 5
        quota   = np.array([45, 28, 70, 9, 32, 20, 45, 20, 10, 20], np.int32)
        weight  = np.array(
            [[0.60, 0.10, 0.10, 0.10, 0.10],
             [0.50, 0.15, 0.10, 0.10, 0.15],
             [0.20, 0.50, 0.05, 0.05, 0.20],
             [0.20, 0.40, 0.10, 0.10, 0.20],
             [0.20, 0.40, 0.10, 0.10, 0.20],
             [0.20, 0.20, 0.20, 0.20, 0.20],
             [0.10, 0.10, 0.50, 0.10, 0.20],
             [0.10, 0.10, 0.30, 0.30, 0.20],
             [0.20, 0.15, 0.15, 0.30, 0.20],
             [0.15, 0.15, 0.20, 0.30, 0.20]])
        average = np.array([40.0, 50.0, 60.0, 55.0, 60.0])
        sigma   = np.array([25.0, 20.0, 15.0, 10.0, 15.0])
    else:
        raise ValueError('Error: unrecognized option')

    department = dict(
        numdept = numdept,
        numsubj = numsubj,
        quota   = quota,
        weight  = weight,
    )

    student = dict(
        numdept = numdept,
        numstd  = numstd,
        numsubj = numsubj,
        average = average,
        sigma   = sigma,
    )

    writefile_assignment(fn, department, student)


if __name__ == '__main__':
    # for command line options
    from optparse import OptionParser
    usage = \
"""\
Usage: %prog [options] filename

By default, read the given file and apply the Gale-Shapley Matching Algorithm
for the problem of students assignment into departments.
If "-g" option is given, test data will be written out into the file.
If "-m" option is given, input/output file is assumed to be for the stable
marriage problem, instead.
"""
    parser = OptionParser(usage=usage)
    parser.add_option('-g', '--generate-test', dest='test', default=False,
                      action='store_true', help='generate test data file')
    parser.add_option('-m', '--marriage', dest='marriage', default=False,
                      action='store_true', help='marriage problem')

    opts, args = parser.parse_args()
    if len(args) != 1:
        print('Error: filename must be given with the first command line argument')
        print('')
        parser.print_help()
        sys.exit(-1)
    fn = args[0]

    # student assignment or stable marriage
    if opts.marriage:
        generate = generate_marriage
        readfile = readfile_marriage
        printres = print_marriage
    else:
        generate = generate_assignment
        readfile = readfile_assignment
        printres = print_assignment

    if opts.test:
        # generate test data file
        if not os.path.exists(fn):
            num = 1
            generate(fn, num)
        else:
            print('Error: file %s already exists' % (fn))
    else:
        # read data and analyze
        x, y = readfile(fn)
        result = match(x, y)
        printres(result)
