:- module(
  tms_print,
  [
    tms_print_justification//3, % +Tms:atom
                                % +Justification:iri
                                % +Options:list(nvpair)
    tms_print_node//3 % +Tms:atom
                      % +Node:iri
                      % +Options:list(nvpair)
  ]
).

/** <module> TMS printing

Support for printing (aspects of) a TMS.

@author Wouter Beek
@version 2013/05, 2013/09-2013/10, 2014/01, 2014/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(generics(option_ext)).

:- use_module(plDcg(dcg_abnf)).
:- use_module(plDcg(dcg_ascii)).
:- use_module(plDcg(dcg_atom)).
:- use_module(plDcg(dcg_bracket)).
:- use_module(plDcg(dcg_content)).
:- use_module(plDcg(dcg_generics)).

:- use_module(plTms(tms)).

:- use_module(plRdf(api/rdfs_read)).

:- rdf_register_prefix(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_print_justification(+,r,+,?,?)).
:- rdf_meta(tms_print_node(+,r,+,?,?)).

:- predicate_options(tms_print_justification/3, 3, [
     pass_to(tms_print_justification0/3, 3)
   ]).
:- predicate_options(tms_print_justification0/3, 3, [
     indent(+nonneg),
     pass_to(tms_print_node/3, 3)
   ]).
:- predicate_options(tms_print_node/3, 3, [
     pass_to(tms_print_justification0/3, 3),
     pass_to(tms_print_node0/2, 2)
   ]).
:- predicate_options(tms_print_node0/2, 2, [
     indent(+nonneg),
     language_preferences(+list(atom))
   ]).



%! tms_print_justification(
%!   +Tms:atom,
%!   +Justification:iri,
%!   +Options:list(nvpair)
%! )// is det.
% The following options are supported:
%   - `indent(+nonneg)`
%     Default: `0`.
%   - `language_preferences(+LanguageTags:list(atom))`
%     Default: `[en]`.

tms_print_justification(Tms, Justification, Options1) -->
  {once(tms_justification(Tms, _, _, Consequence, Justification))},
  % Make sure the indentation option is set.
  {add_default_option(Options1, indent, 0, Options2)},

  % Print the consequence node.
  tms_print_node0(Consequence, Options2),

  % Justifications for a node are written with deeper indentation.
  {update_option(Options2, indent, succ, _, Options3)},

  % Print the justification recursively.
  tms_print_justification0(Tms, Justification, Options3).

%! tms_print_justification0(
%!   +Tms:atom,
%!   +Justification:iri,
%!   +Options:list(nvpair)
%! )// is det.

tms_print_justification0(Tms, Justification, Options1) -->
  {once(tms_justification(Tms, Antecedents, Rule, _, Justification))},
  % Make sure the indentation option is set.
  {add_default_option(Options1, indent, 0, I, Options2)},

  % Write the applied rule.
  indent(I),
  bracketed(square, atom(Rule)),
  nl,

  % Antecedents for a justification are printed with deeper indentation.
  {update_option(Options2, indent, succ, _, Options3)},

  % Print the antecendents recursively.
  % @tbd See whether this can be simplified by using
  %      lambda expressions inside DCGs.
  %      '*'(\X^tms_print_node(Tms, X, Options3), Antecedents).
  tms_print_nodes(Tms, Antecedents, Options3).

tms_print_justifications(_, [], _) --> !, [].
tms_print_justifications(Tms, [H|T], Options) -->
  tms_print_justification0(Tms, H, Options),
  tms_print_justifications(Tms, T, Options).


%! tms_print_node(+Tms:atom, +Node:iri, +Options:list(nvpair))// is det.
% Prints a TMS node, starting with the consequence and
% following with incrementally deeper justifications/antecedents.
%
% Multiple justifications for the same consequence are printed.
%
% The following options are supported:
%   - `indent(+nonneg)`
%     Default: `0`.
%   - `language_preferences(+LanguageTags:list(atom))`
%     Default: `[en]`.

% Print upstream justifications (NONDET).
tms_print_node(Tms, Node, Options1) -->
  % Make sure the indentation option has been set.
  {add_default_option(Options1, indent, 0, Options2)},

  % Print the consequence node.
  tms_print_node0(Node, Options2),

  % Justifications for a node are printed with deeper indentation.
  {update_option(Options2, indent, succ, _, Options3)},

  % Print the justifications for this consequence node.
  {
    aggregate_all(
      set(Justification),
      rdf_has(Justification, tms:has_consequent, Node),
      Justifications
    )
  },
  % @tbd See whether this can be simplified by using
  %      lambda expressions inside DCGs.
  tms_print_justifications(Tms, Justifications, Options3).

%! tms_print_node(+Node:iri, +Options:list(nvpair))// is det.

tms_print_node0(Node, Options1) -->
  {option(indent(I), Options1, 0)},
  indent(I),
  {
    option(language_preferences(LanguageTags), Options1, [en]),
    rdfs_label_value(Node, PreferredLabel, LanguageTags, _)
  },
  atom(PreferredLabel),
  nl.

tms_print_nodes(_, [], _) --> !, [].
tms_print_nodes(Tms, [H|T], Options) -->
  tms_print_node(Tms, H, Options),
  tms_print_nodes(Tms, T, Options).

