# Microtheory System in SWI-Prolog

This is an implementation of a Microtheory/Context system for SWI-Prolog based on the concepts described in "Contexts: A Formalization and Some Applications" by R.V. Guha.

## Overview

The Microtheory system provides a powerful mechanism for organizing knowledge into contexts, supporting default reasoning with exceptions, context hierarchy, and information lifting between contexts. It allows for modular knowledge representation while enabling appropriate information sharing across different domains.

## Core Features

- **Contexts as first-class objects**: Create and manipulate contexts as fundamental units of knowledge organization
- **Context hierarchies**: Establish specialization relationships between contexts with inheritance
- **Lifting rules**: Transform information appropriately between different contexts
- **Default reasoning**: Support for default rules with exceptions
- **Context navigation**: Enter/exit contexts and execute goals within specific contexts

## Key Components

- `simplified_microtheory.pl`: The core implementation of the context mechanism
- `microtheory_rules.pl`: Implementations of specific lifting and default rules
- `microtheory-test-suite.plt`: Test suite demonstrating key functionality
- `debug-test.pl`: Additional debugging test cases

## Usage Examples

### Creating Contexts and Making Assertions

```prolog
% Create contexts with properties
create_context(weather_ctx, [time_formalism(implicit_universal)]),
create_context(winter_ctx, [time_formalism(fixed_time), season(winter)]),

% Set up specialization relationships
assertz(specialization(winter_ctx, weather_ctx)),

% Add assertions to contexts
add_to_context(weather_ctx, temperature(seattle, cold)),
add_to_context(winter_ctx, weather(seattle, foggy)),
```

### Querying Information in Contexts

```prolog
% Check if a statement is true in a context
ist(weather_ctx, temperature(seattle, cold))

% Enter a context to work within it
enter_context(work_context),
% Now queries operate within this context
```

### Defining and Using Lifting Rules

```prolog
% Define a lifting rule between contexts
add_lifting_rule(source_ctx, target_ctx, convert_temperature),

% Define the rule implementation in microtheory_rules.pl
convert_temperature(raw_data(temperature, C, celsius), 
                   processed_data(temperature, F, fahrenheit)) :-
    F is ceiling((C * 9 / 5) + 32), !.
```

### Default Reasoning

```prolog
% Add a default rule to a context
add_default_rule(bird_ctx, bird_flies),

% Define the rule implementation in microtheory_rules.pl
bird_flies(flies(X)) :- 
    ist(bird_ctx, bird(X)),
    \+ ist(bird_ctx, abnormal_bird(X)).
```

## Applications

This system can be effectively used for:

- Knowledge organization with appropriate sharing across domains
- Reasoning with incomplete information and defaults
- Context-dependent interpretation of information
- Hierarchical knowledge representation

It's particularly well-suited for building personal planning assistants, domain-specific reasoning systems, and applications requiring modular knowledge representation.

## Credits

- Original Theory: R.V. Guha, "Contexts: A Formalization and Some Applications"
- Implementation: Claude AI (Anthropic)

## License

This software is provided under the MIT license.
