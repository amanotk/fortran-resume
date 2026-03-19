#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Convert sample source code to rst

$Id: src2rst.py,v 1dae6da0fb4f 2019/01/15 09:18:06 amano $
"""

import os
import sys


OUTPUT_DIR = "docs"

template = """\
.. -*- coding: utf-8 -*-

:orphan:

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
    name, ext = filename.split(".")
    caption = os.sep.join([chapter, filename])
    rstfile = os.path.join(
        OUTPUT_DIR,
        "{chapter}_{name}_{ext}.rst".format(chapter=chapter, name=name, ext=ext),
    )

    # automatically select highlighting language
    if ext == "f" or ext == "f90" or ext == "f95":
        language = "fortran"
    elif ext == "c":
        language = "c"
    elif ext == "py":
        language = "py"
    else:
        raise RuntimeError("Error: language cannot be estimated")

    with open(rstfile, "w") as fp:
        fp.write(template.format(src=src, caption=caption, language=language))


if __name__ == "__main__":
    for src in sys.argv[1:]:
        convert2rst(src)
