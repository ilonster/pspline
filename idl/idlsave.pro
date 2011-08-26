; IDL Version 8.0 (linux x86 m32)
; Journal File for joseph5@grendel
; Working directory: /afs/fepcluster/home/joseph5/ntcc/pspline/idl
; Date: Fri Jul 29 18:52:09 2011
 
g = file_import('~/JRT2011/NSTX/ELMs/g129032.00550')
; % CALL_EXTERNAL: Error loading sharable executable.
;                  Symbol: pdb_open, File = pdb2idl.so
;                  pdb2idl.so: cannot open shared object file: No such file or directory
g = read_gfile('~/JRT2011/NSTX/ELMs/g129032.00550')
; % Variable is undefined: READ_GFILE.
g = read_neqdsk('~/JRT2011/NSTX/ELMs/g129032.00550')
;   nxefit =      129 nyefit =      129
;ERROR Occurred whilst reading G-EQDSK file
hlpe, /str, g
; % Attempt to call undefined procedure/function: 'HLPE'.
help, /str, g
g = read_neqdsk("~/JRT2011/NSTX/ELMs/g129032.00550")
;   nxefit =      129 nyefit =      129
;ERROR Occurred whilst reading G-EQDSK file
g = read_neqdsk("~/JRT2011/NSTX/ELMs/g129019.00425")
;   nxefit =      129 nyefit =      129
;ERROR Occurred whilst reading G-EQDSK file
help, /str, g
