/bin/ls -1 */fpmake.pp | gawk -F '/' '/fpmake.pp/ { printf "procedure add_%s;\nbegin\n  with Installer do\n    {$include %s}\nend;\n\n",gensub("-","_","g",$1),$0; }' > fpmake_proc.inc
/bin/ls -1 */fpmake.pp | gawk -F '/' '/fpmake.pp/ { printf "  add_%s;\n",gensub("-","_","g",$1); }' > fpmake_add.inc

