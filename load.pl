% The load file for library plTms.

:- dynamic(user:project/2).
:- multifile(user:project/2).
   user:project(plTms, 'Truth Maintenance Systems implemented in Prolog.').

:- use_module(load_project).
:- load_project([
  plc-'Prolog-Library-Collection',
  plRdf
]).

