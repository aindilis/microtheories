:- use_module(simplified_microtheory).
:- use_module(microtheory_rules).

% Debug the temperature conversion issue
debug_temperature :-
    format('~n=== Debugging Temperature Conversion ===~n'),
    create_context(source_ctx, []),
    create_context(target_ctx, []),
    
    % Add source data
    add_to_context(source_ctx, raw_data(temperature, 25, celsius)),
    
    % Add lifting rule
    add_lifting_rule(source_ctx, target_ctx, convert_temperature),
    
    % Direct test of the conversion rule
    convert_temperature(raw_data(temperature, 25, celsius), Result),
    format('Direct rule test result: ~w~n', [Result]),
    
    % Test using ist
    format('Context source_ctx contains: '),
    (ist(source_ctx, raw_data(temperature, 25, celsius)) -> 
        format('yes~n') ; format('no~n')),
    
    format('Attempting to lift to target_ctx...~n'),
    
    % Try full lifting test
    (ist(target_ctx, processed_data(temperature, 77.0, fahrenheit)) -> 
        format('PASS: Found expected result in target context~n') 
    ;
        format('FAIL: Did not find expected result~n'),
        % Try alternate temperature format
        (ist(target_ctx, processed_data(temperature, 77, fahrenheit)) -> 
            format('NOTE: Found result with integer temperature (77 not 77.0)~n')
        ;
            % Try to see what's actually in the target context
            format('Checking what is in target_ctx:~n'),
            dump_context(target_ctx)
        )
    ).

% Debug the feature recommendation issue
debug_recommendation :-
    format('~n=== Debugging Feature Recommendation ===~n'),
    create_context(car_ctx, []),
    
    % Try adding a simple fact
    add_to_context(car_ctx, recommend_feature(car_x, fog_lights)),
    
    % Check if it's retrievable
    format('Direct assertion test: '),
    (ist(car_ctx, recommend_feature(car_x, fog_lights)) -> 
        format('PASS~n') 
    ;
        format('FAIL~n')
    ),
    
    % Dump the context to see what's in it
    dump_context(car_ctx).

% Run both debug tests
:- initialization((
    debug_temperature,
    debug_recommendation,
    halt
)).
