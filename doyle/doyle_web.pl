:- module(doyle_web, []).

/** <module> Doyle web

Web-interface to Doyle's TMS.

@author Wouter Beek
@version 2013/05, 2014/01, 2014/03
*/

:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- use_module(plTms(doyle/doyle)).
:- use_module(plTms(tms)).

:- use_module(plHtml(html_table)).

:- multifile(http:location/3).
http:location(tms, root(tms), []).
:- http_handler(tms(doyle), doyle_web, []).



doyle_web(_Request):-
  findall(
    [
      Node,
      SupportStatus,
      %SupportingJustifications,
      SupportingNodes,
      Antecedents,
      Foundations,
      %Ancestors,
      Consequences,
      AffectedConsequences,
      BelievedConsequences,
      Repercussions,
      BelievedRepercussions
    ],
    (
      node(TMS, Node),
      doyle:support_status(Node, SupportStatus),
      %doyle:supporting_justifications(Node, SupportingJustifications),
      doyle:supporting_nodes(Node, SupportingNodes),
      doyle:antecedents(Node, Antecedents),
      doyle:foundations(Node, Foundations),
      %doyle:ancestors(Node, Ancestors),
      doyle:consequences(Node, Consequences),
      doyle:affected_consequences(Node, AffectedConsequences),
      doyle:believed_consequences(Node, BelievedConsequences),
      doyle:repercussions(Node, Repercussions),
      doyle:believed_repercussions(Node, BelievedRepercussions)
    ),
    Rows
  ),

  % Assemble the contents of the header row.
  HeaderRow = [
      'Node',
      'Support status',
      %%%%'Supporting justification',
      'Supporting nodes',
      'Antecedents',
      'Foundations',
      %%%%'Ancestors',
      'Consequences',
      'Affected consequences',
      'Believed consequences',
      'Repercussions',
      'Believed repercussions'],

  reply_html_page(
    plServer_style,
    title('Doyle'),
    html(
      \html_table(
        html('Doyle\'s TMS overview'),
        [HeaderRow|Rows],
        [header_row(true)]
      )
    )
  ).

