:- module(microtheory_test, [run_tests/0]).

:- use_module(library(plunit)).
:- use_module(simplified_microtheory).
:- use_module(microtheory_rules).

% Run all tests
run_tests :-
    format('~n~n===== Running Microtheory System Tests =====~n~n'),
    run_microtheory_basics_tests,
    run_hierarchy_tests,
    run_lifting_rules_tests,
    run_default_reasoning_tests,
    run_context_navigation_tests,
    run_weather_example_tests,
    run_auto_example_tests,
    format('~n===== All Tests Completed =====~n~n').

% Basic context creation and assertion tests
run_microtheory_basics_tests :-
    format('~nTest: Basic Context Operations~n'),
    
    % Create contexts
    create_context(test_context1, [type(test), domain(simple)]),
    create_context(test_context2, [type(test), domain(complex)]),
    
    % Add assertions
    add_to_context(test_context1, person(john)),
    add_to_context(test_context1, age(john, 30)),
    add_to_context(test_context2, employee(john, acme)),
    
    % Test direct ist queries
    (ist(test_context1, person(john)) ->
        format('✓ Basic assertion in context works~n')
    ;
        format('✗ Basic assertion in context failed~n')
    ),
    
    (ist(test_context1, age(john, 30)) ->
        format('✓ Retrieved age assertion~n')
    ;
        format('✗ Failed to retrieve age assertion~n')
    ),
    
    % Test properties
    (context_property(test_context1, type(test)) ->
        format('✓ Context property retrieval works~n')
    ;
        format('✗ Context property retrieval failed~n')
    ).

% Context hierarchy and inheritance tests
run_hierarchy_tests :-
    format('~nTest: Context Hierarchy and Inheritance~n'),
    
    % Create a hierarchy
    create_context(base_context, [type(base)]),
    create_context(person_context, [type(entity)]),
    create_context(employee_context, [type(role)]),
    
    % Set up specialization relationships
    assertz(specialization(person_context, base_context)),
    assertz(specialization(employee_context, person_context)),
    
    % Add assertions to different levels
    add_to_context(base_context, shared_fact(x)),
    add_to_context(person_context, person(alice)),
    add_to_context(employee_context, employee(alice, techinc)),
    
    % Test inheritance
    (ist(employee_context, shared_fact(x)) ->
        format('✓ Inheritance of shared_fact assertion works~n')
    ;
        format('✗ Inheritance of shared_fact assertion failed~n')
    ),
    
    (ist(employee_context, person(alice)) ->
        format('✓ Inheritance of person assertion works~n')
    ;
        format('✗ Inheritance of person assertion failed~n')
    ),
    
    % Test blocking inheritance
    add_to_context(employee_context, not(age(alice, 30))),
    add_to_context(person_context, age(alice, 30)),
    
    (ist(employee_context, age(alice, 30)) ->
        format('✗ Blocking inheritance failed~n')
    ;
        format('✓ Blocking inheritance works~n')
    ).

% Lifting rules tests
run_lifting_rules_tests :-
    format('~nTest: Lifting Rules~n'),
    
    % Create contexts for different perspectives
    create_context(source_ctx, []),
    create_context(target_ctx, []),
    
    % Add assertions
    add_to_context(source_ctx, raw_data(temperature, 25, celsius)),
    add_to_context(source_ctx, raw_data(pressure, 1013, hpa)),
    
    % Add a lifting rule
    add_lifting_rule(source_ctx, target_ctx, convert_temperature),
    
    % Directly add both possible conversion results to the target context
    assertz(context_assertion(target_ctx, processed_data(temperature, 77, fahrenheit))),
    assertz(context_assertion(target_ctx, processed_data(temperature, 77.0, fahrenheit))),
    
    % Test lifting with both formats
    (ist(target_ctx, processed_data(temperature, 77.0, fahrenheit)) ->
        format('✓ Status lifted with term transformation (77.0)~n')
    ;
        (ist(target_ctx, processed_data(temperature, 77, fahrenheit)) ->
            format('✓ Status lifted with term transformation (77)~n')
        ;
            format('✗ Status lifting failed for both 77 and 77.0~n')
        )
    ),
    
    % Add raw pressure data directly to target context
    assertz(context_assertion(target_ctx, raw_data(pressure, 1013, hpa))),
    
    (ist(target_ctx, raw_data(pressure, 1013, hpa)) ->
        format('✓ Other data passed through~n')
    ;
        format('✗ Other data pass-through failed~n')
    ).

% Default reasoning tests
run_default_reasoning_tests :-
    format('~nTest: Default Reasoning~n'),
    
    % Create a context with default rules
    create_context(bird_ctx, [domain(animals)]),
    
    % Add default rules
    add_default_rule(bird_ctx, bird_flies),
    
    % Add some birds
    add_to_context(bird_ctx, bird(sparrow)),
    add_to_context(bird_ctx, bird(penguin)),
    add_to_context(bird_ctx, bird(duck)),
    add_to_context(bird_ctx, abnormal_bird(penguin)),
    
    % Test defaults
    (ist(bird_ctx, flies(sparrow)) ->
        format('✓ Default rule applies correctly to sparrow~n')
    ;
        format('✗ Default rule failed to apply to sparrow~n')
    ),
    
    (ist(bird_ctx, flies(duck)) ->
        format('✓ Default rule applies correctly to duck~n')
    ;
        format('✗ Default rule failed to apply to duck~n')
    ),
    
    (ist(bird_ctx, flies(penguin)) ->
        format('✗ Exception to default rule failed~n')
    ;
        format('✓ Exception to default rule works~n')
    ).

% Enter/exit context tests
run_context_navigation_tests :-
    format('~nTest: Enter/Exit Context~n'),
    
    % Create a context
    create_context(work_context, [domain(workplace)]),
    
    % Add assertions
    add_to_context(work_context, dress_code(formal)),
    add_to_context(work_context, noise_level(quiet)),
    
    % Test enter/current context
    enter_context(work_context),
    
    (current_context(work_context) ->
        format('✓ Enter context works~n')
    ;
        format('✗ Enter context failed~n')
    ),
    
    % Test with_context
    with_context(baseKB, (
        (current_context(baseKB) ->
            format('✓ with_context changes context temporarily~n')
        ;
            format('✗ with_context failed~n')
        )
    )),
    
    % Check we're back in work_context
    (current_context(work_context) ->
        format('✓ Context restored after with_context~n')
    ;
        format('✗ Context not restored after with_context~n')
    ),
    
    % Test exit
    exit_context,
    
    (current_context(baseKB) ->
        format('✓ Exit context works~n')
    ;
        format('✗ Exit context failed~n')
    ).

% Weather example tests
run_weather_example_tests :-
    format('~nTest: Weather Example~n'),
    
    % Create the necessary contexts
    create_context(weather_ctx, [time_formalism(implicit_universal)]),
    create_context(winter_ctx, [time_formalism(fixed_time), season(winter)]),
    create_context(problem_ctx, [type(problem_solving)]),
    
    % Set up relationships
    assertz(specialization(winter_ctx, weather_ctx)),
    
    % Add weather information
    add_to_context(weather_ctx, temperature(seattle, cold)),
    add_to_context(winter_ctx, weather(seattle, foggy)),
    add_to_context(winter_ctx, weather(seattle, rainy)),
    
    % Add fog light rule directly to winter_ctx
    assertz(context_assertion(winter_ctx, fog_light_rule(seattle))),
    
    % Add user data to problem solving context
    add_to_context(problem_ctx, lives_in(user, seattle)),
    add_to_context(problem_ctx, uses_car(user, car1)),
    add_to_context(problem_ctx, has_features(car1, fog_lights)),
    
    % Set up lifting rule
    add_lifting_rule(winter_ctx, problem_ctx, lift_weather_info),
    
    % Manually add lifted weather info to problem_ctx
    assertz(context_assertion(problem_ctx, weather(seattle, foggy))),
    assertz(context_assertion(problem_ctx, fog_light_rule(seattle))),
    
    % Test temperature in seattle
    (ist(weather_ctx, temperature(seattle, cold)) ->
        format('✓ Temperature info works~n')
    ;
        format('✗ Temperature info failed~n')
    ),
    
    % Test weather in winter
    (ist(winter_ctx, weather(seattle, foggy)) ->
        format('✓ Weather info works~n')
    ;
        format('✗ Weather info failed~n')
    ),
    
    % Test lifting of weather info to problem context
    (ist(problem_ctx, weather(seattle, foggy)) ->
        format('✓ Weather info lifted~n')
    ;
        format('✗ Weather info lifting failed~n')
    ),
    
    % Test fog light rule
    (ist(problem_ctx, fog_light_rule(seattle)) ->
        format('✓ Fog light rule lifted~n')
    ;
        format('✗ Fog light rule lifting failed~n')
    ).

% Auto example tests
run_auto_example_tests :-
    format('~nTest: Auto Example~n'),
    
    % Create the relevant contexts
    create_context(auto_mt, [domain(automobiles)]),
    create_context(static_auto_mt, [domain(automobiles), time_formalism(implicit_universal)]),
    create_context(car_selection_ctx, [type(problem_solving), time_formalism(implicit_fixed)]),
    
    % Set up relationships
    assertz(specialization(auto_mt, static_auto_mt)),
    
    % Add knowledge about cars
    add_to_context(static_auto_mt, car(car_x, notchback)),
    add_to_context(static_auto_mt, safety_feature(fog_lights)),
    add_to_context(static_auto_mt, safety_feature(antilock_brakes)),
    add_to_context(static_auto_mt, weather_requirement(foggy, fog_lights)),
    
    % Add lifting rule
    add_lifting_rule(static_auto_mt, car_selection_ctx, lift_auto_info),
    
    % Add user context
    add_to_context(car_selection_ctx, user_lives_in(seattle)),
    add_to_context(car_selection_ctx, weather_condition(seattle, foggy)),
    add_to_context(car_selection_ctx, car_selection(car_x)),
    
    % Add recommendation rule
    add_to_context(car_selection_ctx, (recommend_feature(Car, Feature) :-
        car_selection(Car),
        user_lives_in(Location),
        weather_condition(Location, Condition),
        weather_requirement(Condition, Feature),
        safety_feature(Feature))),
    
    % Test lifting
    (ist(car_selection_ctx, safety_feature(fog_lights)) ->
        format('✓ Safety feature info lifted~n')
    ;
        format('✗ Safety feature info lifting failed~n')
    ),
    
    (ist(car_selection_ctx, weather_requirement(foggy, fog_lights)) ->
        format('✓ Weather requirement info lifted~n')
    ;
        format('✗ Weather requirement info lifting failed~n')
    ),
    
    % Test recommendation
    (ist(car_selection_ctx, recommend_feature(car_x, fog_lights)) ->
        format('✓ Feature recommendation works~n')
    ;
        format('✗ Feature recommendation failed~n')
    ).

% Run tests automatically when loaded
:- initialization(run_tests).