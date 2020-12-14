project: Fortran-Logger
src_dir: ../src
output_dir: html/publish/
project_github: https://github.com/ShatrovOA/Fortran-Logger
<!-- summary: PENF, Portability Environment for Fortran poor people -->
author: Oleg Shatrov
github: https://github.com/ShatrovOA
email: shatrov.oleg.a@gmail.com
md_extensions: markdown.extensions.toc(anchorlink=True)
               markdown.extensions.smarty(smart_quotes=False)
               markdown.extensions.extra
               markdown_checklist.extension
docmark: <
display: public
         protected
         private
source: true
warn: true
graph: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html

{!../README.md!}
