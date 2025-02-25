:- module(microtheory, [
    context/1,              % Create/check a context
    ist/2,                  % Basic ist predicate (is true in)
    create_context/2,       % Create a context with properties
    add_to_context/2,       % Add formula to a context
    specialization/2,       % Context hierarchy relationship
    general_context/1,      % The most general context
    lifting_rule/3,         % Define lifting rules
    corefers/3,             % Term coreference across contexts
    enter_context/1,        % Enter a context for reasoning
    exit_context/0,         % Exit the current context
    current_context/1,      % Get the current context
    query_in_context/2,     % Query within a context
    with_context/2,         % Execute Goal in Context
    present_in/2,           % Check if an object is in a context
    context_property/2,     % Get/check context property
    add_context_property/2, % Add property to a context
    default_rule/2,         % Default rules in contexts
    add_default_rule/2,     % Add a default rule to a context
    add_lifting_rule/3,     % Add a lifting rule between contexts
    dump_context/1,         % Debugging helper
    context_assertion/2     % Direct access to context assertions
]).

:- use_module(library(lists)).
:- use_module(microtheory_rules).

% Dynamic predicates for storing context information
:- dynamic context/1.                   % Defined contexts
:- dynamic context_assertion/2.         % Assertions in contexts (ist)
:- dynamic specialization/2.            % Context hierarchy
:- dynamic lifting_rule/3.              % Rules for lifting formulas
:- dynamic corefers/3.                  % Term correspondence
:- dynamic current_context_stack/1.     % Context stack for enter/exit
:- dynamic context_property/2.          % Context properties
:- dynamic default_rule/2.              % Default rules in contexts

% Initialize the system
:- initialization(init_microtheory_system).

init_microtheory_system :-
    retractall(current_context_stack(_)),
    assertz(current_context_stack([])),
    general_context(Base),
    create_context(Base, [type(base_context)]).

% Create a general (base) context
general_context(baseKB).

% Check if a context exists or create one
create_context(Context, Properties) :-
    context(Context), !,
    maplist(add_context_property(Context), Properties).
create_context(Context, Properties) :-
    assertz(context(Context)),
    maplist(add_context_property(Context), Properties).

% Add a property to a context
add_context_property(Context, Property) :-
    (context_property(Context, Property) ->
        true  % Property already exists
    ;
        assertz(context_property(Context, Property))
    ).

% Add formula to a context (syntactic sugar for ist)
add_to_context(Context, Formula) :-
    assertz(context_assertion(Context, Formula)),
    % Apply forward lifting rules
    forward_lift(Context, Formula).

% Forward lifting - apply lifting rules when adding new assertions
forward_lift(SourceContext, SourceFormula) :-
    % For each target context that has a lifting rule from SourceContext
    forall(
        lifting_rule(SourceContext, TargetContext, Rule),
        (
            % Try to apply the rule and add the result to the target context
            (call(Rule, SourceFormula, TargetFormula) ->
                assertz(context_assertion(TargetContext, TargetFormula))
            ;
                true  % Skip if rule doesn't apply
            )
        )
    ).

% Helper to check if an object is present in a context
present_in(Context, Object) :-
    context_assertion(Context, present_in(Object)).

% Add default rule to a context
add_default_rule(Context, Rule) :-
    assertz(default_rule(Context, Rule)).

% Add this to simplified_microtheory.pl to handle rule-based inferences
% This must be added before the other ist/2 clauses to ensure proper precedence

% Add this clause to handle rule-based inferences
ist(Context, Conclusion) :-
    context_assertion(Context, (Conclusion :- Conditions)),
    ist(Context, Conditions).

% Handle conjunctions in conditions
ist(Context, (A, B)) :-
    ist(Context, A),
    ist(Context, B).

% Handle true (empty condition)
ist(_, true).

% ist/2 implementation - Is true in context
% Direct assertion
ist(Context, Formula) :-
    context_assertion(Context, Formula), !.

% Via specialization/inheritance
ist(Context, Formula) :-
    specialization(Context, ParentContext),
    ist(ParentContext, Formula),
    \+ block_inheritance(Context, ParentContext, Formula), !.

% Via lifting rules
ist(TargetContext, TargetFormula) :-
    lifting_rule(SourceContext, TargetContext, Rule),
    % Find a source formula that converts to our target formula
    context_assertion(SourceContext, SourceFormula),
    call(Rule, SourceFormula, TargetFormula), !.

% Via default rules
ist(Context, Formula) :-
    default_rule(Context, Rule),
    call(Rule, Formula), !.

% Helper to block inheritance when exceptions exist
block_inheritance(Context, _, Formula) :-
    context_assertion(Context, not(Formula)).

% Context stack management
current_context(Context) :-
    current_context_stack([Context|_]), !.
current_context(baseKB).

% Enter a context (push)
enter_context(Context) :-
    context(Context),
    current_context_stack(Stack),
    retractall(current_context_stack(_)),
    assertz(current_context_stack([Context|Stack])).

% Exit the current context (pop)
exit_context :-
    current_context_stack([_|Rest]),
    retractall(current_context_stack(_)),
    assertz(current_context_stack(Rest)).

% Execute a goal in a specific context
with_context(Context, Goal) :-
    current_context_stack(OldStack),
    enter_context(Context),
    (call(Goal) ->
        retractall(current_context_stack(_)),
        assertz(current_context_stack(OldStack)),
        true
    ;
        retractall(current_context_stack(_)),
        assertz(current_context_stack(OldStack)),
        false
    ).

% Query within the current context
query_in_context(Context, Query) :-
    context(Context),
    ist(Context, Query).

% Default coreference rule
default_corefers(Term, C1, C2, Term) :-
    context(C1),
    context(C2).

% Add a lifting rule between contexts
add_lifting_rule(SourceContext, TargetContext, Rule) :-
    assertz(lifting_rule(SourceContext, TargetContext, Rule)).

% Add a coreference between terms
add_coreference(Term1, Context1, Context2, Term2) :-
    assertz(corefers(Term1, Context1, Context2, Term2)).

% Dump context contents for debugging
dump_context(Context) :-
    context(Context),
    format('Context: ~w~n', [Context]),
    forall(context_property(Context, Prop), 
           format('  Property: ~w~n', [Prop])),
    forall(context_assertion(Context, Assertion), 
           format('  Assertion: ~w~n', [Assertion])),
    forall(default_rule(Context, Rule), 
           format('  Default Rule: ~w~n', [Rule])),
    forall(specialization(Context, Parent), 
           format('  Parent: ~w~n', [Parent])),
    forall(specialization(Child, Context), 
           format('  Child: ~w~n', [Child])).