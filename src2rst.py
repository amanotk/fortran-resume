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
  :language: fortran
  :linenos:
"""

def convert2rst(src):
    sample, chapter, filename = src.split(os.sep)
    name, ext = filename.split('.')
    caption = string.join([chapter, filename], os.sep)
    rstfile = "{chapter}_{name}.rst".format(chapter=chapter, name=name)
    fp = open(rstfile, 'w')
    fp.write(template.format(src=src, caption=caption))
    fp.close()

if __name__ == '__main__':
    for src in sys.argv[1:]:
        convert2rst(src)
