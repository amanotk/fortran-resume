#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Multiple Precision Calculation of Circle Ratio PI

 $Id$
"""

import sys
import math
import mpmath

def print_pi(pi, ndigit):
    "print out pi to the terminal"
    pistr = mpmath.nstr(pi, ndigit+2)[2:ndigit+2]
    sys.stdout.write('PI = 3.')
    # if the number of digits < 50
    if ndigit < 50:
        i = 0
        print '%s %s %s %s %s' \
            % (pistr[50*i+00:50*i+10],
               pistr[50*i+10:50*i+20],
               pistr[50*i+20:50*i+30],
               pistr[50*i+30:50*i+40],
               pistr[50*i+40:50*i+50])
        return
    # else
    for i in range(ndigit/50-1):
        print '%s %s %s %s %s' \
            % (pistr[50*i+00:50*i+10],
               pistr[50*i+10:50*i+20],
               pistr[50*i+20:50*i+30],
               pistr[50*i+30:50*i+40],
               pistr[50*i+40:50*i+50])
        print '      ',
    i = ndigit/50-1
    print '%s %s %s %s %s' \
        % (pistr[50*i+00:50*i+10],
           pistr[50*i+10:50*i+20],
           pistr[50*i+20:50*i+30],
           pistr[50*i+30:50*i+40],
           pistr[50*i+40:50*i+50])

def calc_pi(ndigit, dps):
    "calculate pi using Machin's formula"
    mpmath.mp.dps = dps
    # +10 for small ndigit
    N = int(math.ceil(ndigit / (2*math.log10(5.0)))) + 10
    a = mpmath.mpf(16) / mpmath.mpf(5)
    b = mpmath.mpf(4)  / mpmath.mpf(239)
    c = a - b
    pi = mpmath.mpf(0)
    for i in range(N):
        if i%2 == 1:
            pi = pi - c
        else:
            pi = pi + c
        a = a / (5*5)
        b = b / (239*239)
        c = (a - b) / (2*i+3)
    return pi

if __name__ == '__main__':
    ndigit = int(sys.argv[1])
    dps    = ndigit+10
    print_pi(calc_pi(ndigit, dps), ndigit)
