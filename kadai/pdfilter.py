#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import re
import panflute as pf

###
### this is a hack filter to ignore reStructuredText link
###
regexp_pattern = re.compile(r'^(\S+)\s+<\S+>$')

def action(elem, doc):
    if isinstance(elem, pf.Str):
        m = re.match(regexp_pattern, elem.text)
        if m:
            elem = pf.Str(m.groups()[0])
    if isinstance(elem, pf.Image):
        elem.url = os.path.join('../', elem.url)
    return elem

if __name__ == '__main__':
    pf.run_filter(action)

