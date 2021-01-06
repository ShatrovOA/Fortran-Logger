project: Fortran Logger
src_dir: ../src/lib
src_dir: ../src/tests
output_dir: html/
project_github: https://github.com/ShatrovOA/Fortran-Logger
author: Oleg Shatrov
github: https://github.com/ShatrovOA
email: shatrov.oleg.a@gmail.com
summary: Highly customizable logger and error checker
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
               markdown.extensions.extra
               markdown_checklist.extension
docmark: <
display: public
         protected
         private
source: false
warn: true
graph: false
extra_mods: penf:https://github.com/szaghi/PENF
            datetime_module:https://github.com/wavebitscientific/datetime-fortran
            face:https://github.com/szaghi/FACE
            flap:https://github.com/szaghi/FLAP
            json_module:https://github.com/jacobwilliams/json-fortran

{!../README.md!}
