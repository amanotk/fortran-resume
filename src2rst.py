#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Convert sample source code to rst

 $Id: src2rst.py,v 1dae6da0fb4f 2019/01/15 09:18:06 amano $
"""

import os
import sys
import string

template ="""\
.. -*- coding: utf-8 -*-

=====================================================
{caption}
=====================================================

`サンプルコードのダウンロード <{src}>`_

.. literalinclude:: {src}
  :language: {language}
  :linenos:
"""

def convert2rst(src):
    sample, chapter, filename = src.split(os.sep)
    name, ext = filename.split('.')
    caption = string.join([chapter, filename], os.sep)
    rstfile = "{chapter}_{name}_{ext}.rst".format(chapter=chapter, name=name, ext=ext)

    # automatically select highlighting language
    if   ext == 'f' or ext == 'f90' or ext == 'f95':
        language = 'fortran'
    elif ext == 'c':
        language = 'c'
    elif ext == 'py':
        language = 'py'
    else:
        raise RuntimeError('Error: language cannot be estimated')

    fp = open(rstfile, 'w')
    fp.write(template.format(src=src, caption=caption, language=language))
    fp.close()

if __name__ == '__main__':
    for src in sys.argv[1:]:
        convert2rst(src)
