*"* use this source file for your ABAP unit test classes

CLASS lth_soar_provider DEFINITION
        CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_soar_provider.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zif_soar_provider.

ENDCLASS.


CLASS lth_soar_provider IMPLEMENTATION.

  METHOD create.

    result = NEW lth_soar_provider( ).

  ENDMETHOD.

  METHOD zif_soar_provider~get_abap_source_code.

*    result = VALUE #(
*            ( `PROGRAM.` )
*            ( `INCLUDE zsoar_gensrp_forms.` )
*            ( `CLASS lcl_soar_test DEFINITION.` )
*            ( `  PUBLIC SECTION.` )
*            ( `    INTERFACES zif_soar_test.` )
*            ( `ENDCLASS.` )
*            ( `CLASS lcl_soar_test IMPLEMENTATION.` )
*            ( `  METHOD zif_soar_test~popup.` )
*            ( `    MESSAGE text TYPE 'I'.` )
*            ( `  ENDMETHOD.` )
*            ( `ENDCLASS.` ) ).

  ENDMETHOD.

  METHOD zif_soar_provider~get_abap_hash_keys.
    result = VALUE #( ( srp_id   = 'ZSOAR_MANAGER_TEST_OUTSOURCED'
                        hash_key = 'zksFQhfRoiWQ9X+FlukkbtktEprwjGsAkfpphOGIhLI=' ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_generate_subroutine_pool DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    INTERFACES zif_soar_provider.

  PRIVATE SECTION.

    METHODS invalid_hash_key FOR TESTING RAISING cx_static_check.
    METHODS syntax_error FOR TESTING RAISING cx_static_check.

    DATA srp_id TYPE zsoar_srp_id.
    DATA abap_source_code TYPE zif_soar_manager=>ty_abap_source_code.
    DATA abap_hash_key TYPE zsoar_abap_hash_key.

ENDCLASS.


CLASS ltc_generate_subroutine_pool_2 DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    INTERFACES zif_soar_provider.

  PRIVATE SECTION.

    METHODS create_object_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS create_object_perform FOR TESTING RAISING cx_static_check.
    METHODS factory_method_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS factory_method_perform FOR TESTING RAISING cx_static_check.

    CONSTANTS srp_id TYPE zsoar_srp_id VALUE 'ZSOAR_MANAGER_TEST_OUTSOURCED'.
    CLASS-DATA abap_source_code TYPE zif_soar_manager=>ty_abap_source_code.
    CLASS-DATA abap_hash_key TYPE zsoar_abap_hash_key VALUE ''.
    CLASS-DATA manager TYPE REF TO zif_soar_manager.
    CLASS-DATA provider TYPE REF TO ltc_generate_subroutine_pool_2.

    CLASS-METHODS class_setup.

ENDCLASS.


CLASS ltc_instantiate_inhousedev DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PUBLIC SECTION.

    INTERFACES zif_soar_provider.

  PRIVATE SECTION.

    METHODS create_object_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS create_object_perform FOR TESTING RAISING cx_static_check.
    METHODS factory_method_absolute_type FOR TESTING RAISING cx_static_check.
    METHODS factory_method_perform FOR TESTING RAISING cx_static_check.

    CONSTANTS srp_id TYPE zsoar_srp_id VALUE 'ZSOAR_MANAGER_TEST_INHOUSEDEV'.

    CLASS-DATA manager TYPE REF TO zif_soar_manager.
    CLASS-DATA provider TYPE REF TO ltc_generate_subroutine_pool_2.

    CLASS-METHODS class_setup.

ENDCLASS.


CLASS ltc_generate_subroutine_pool IMPLEMENTATION.

  METHOD invalid_hash_key.

    srp_id = 'ZSOAR_MANAGER_TEST_OUTSOURCED'.
    abap_source_code = VALUE zif_soar_manager=>ty_abap_source_code(
            ( `test` ) ).
    abap_hash_key = 'aaaaaaaaaabbbbbbbbbbb'.
    " This is the part which checks the hash key and does GENERATE SUBROUTINE POOL.
    TRY.
        DATA(manager) = zcl_soar_manager=>zif_soar_manager~create( srp_id   = srp_id
                                                                   provider = me ).
      CATCH cx_root INTO DATA(error) ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( error ).
    cl_abap_unit_assert=>assert_equals( act = error->textid
                                        exp = zcx_soar=>zcx_soar ).
    cl_abap_unit_assert=>assert_equals( act = error->get_text( )
                                        exp = 'Invalid hash key for ZSOAR_MANAGER_TEST_OUTSOURCED. Act: n4bQgYhMfWWaL+qgxVrQFaO/TxsrC4Is0V1sFbDwCgg=. Exp: aaaaaaaaaabbbbbbbbbbb. Please contact the support.' ).

  ENDMETHOD.


  METHOD syntax_error.

    srp_id = 'ZSOAR_MANAGER_TEST_OUTSOURCED'.
    abap_source_code = VALUE zif_soar_manager=>ty_abap_source_code(
            ( `test` ) ).
    abap_hash_key = 'n4bQgYhMfWWaL+qgxVrQFaO/TxsrC4Is0V1sFbDwCgg='.
    " This is the part which checks the hash key and does GENERATE SUBROUTINE POOL.
    TRY.
        DATA(manager) = zcl_soar_manager=>zif_soar_manager~create( srp_id   = srp_id
                                                                   provider = me ).
      CATCH cx_root INTO DATA(error) ##NO_HANDLER.
    ENDTRY.

    cl_abap_unit_assert=>assert_bound( error ).
    cl_abap_unit_assert=>assert_equals( act = error->textid
                                        exp = zcx_soar=>zcx_soar ).
    cl_abap_unit_assert=>assert_equals( act = error->get_text( )
                                        exp = 'Generation error 4 at line 1: The last statement is not complete (period missing).' ).

  ENDMETHOD.


  METHOD zif_soar_provider~get_abap_source_code.

    result = abap_source_code.

  ENDMETHOD.


  METHOD zif_soar_provider~get_abap_hash_keys.

    result = VALUE #( ( srp_id   = srp_id
                        hash_key = abap_hash_key ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_generate_subroutine_pool_2 IMPLEMENTATION.

  METHOD class_setup.

    abap_source_code = VALUE zif_soar_manager=>ty_abap_source_code(
            ( `PROGRAM zsoar_manager_test_inhousedev REDUCED FUNCTIONALITY.` )
            ( `INCLUDE zsoar_srpoo_forms.                                  ` )
            ( `CLASS lcl_test DEFINITION.                                  ` )
            ( `  PUBLIC SECTION.                                           ` )
            ( `    INTERFACES zif_soar_manager_test.                       ` )
            ( `ENDCLASS.                                                   ` )
            ( `CLASS lcl_test IMPLEMENTATION.                              ` )
            ( `  METHOD zif_soar_manager_test~create.                      ` )
            ( `    result = NEW lcl_test( ).                               ` )
            ( `  ENDMETHOD.                                                ` )
            ( `  METHOD zif_soar_manager_test~square.                      ` )
            ( `    result = number ** 2.                                   ` )
            ( `  ENDMETHOD.                                                ` )
            ( `ENDCLASS.                                                   ` ) ).
    abap_hash_key = 'a5amt5QLM7nnI9TCwVLLuEsNfBa8xtA9NV/TU88yzcw='.

    provider = NEW ltc_generate_subroutine_pool_2( ).
    manager = zcl_soar_manager=>zif_soar_manager~create( srp_id   = srp_id
                                                         provider = provider ).
    cl_abap_unit_assert=>assert_char_cp( act = manager->srp_name
                                         exp = '%_T*' ).
    " Warning message
    cl_abap_unit_assert=>fail( msg   = |NOT AN ERROR, is for information. Generated subroutine pool: { manager->srp_name }|
                               level = if_aunit_constants=>TOLERABLE
                               quit  = if_aunit_constants=>no ).

  ENDMETHOD.


  METHOD create_object_absolute_type.

    DATA(test) = CAST zif_soar_manager_test( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD create_object_perform.

    DATA(test) = CAST zif_soar_manager_test( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_absolute_type.

    DATA(test) = CAST zif_soar_manager_test( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SOAR_MANAGER_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_perform.

    DATA(test) = CAST zif_soar_manager_test( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SOAR_MANAGER_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = |\\PROGRAM={ manager->srp_name }\\CLASS=LCL_TEST| ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD zif_soar_provider~get_abap_source_code.

    result = abap_source_code.

  ENDMETHOD.


  METHOD zif_soar_provider~get_abap_hash_keys.

    result = VALUE #( ( srp_id   = srp_id
                        hash_key = abap_hash_key ) ).

  ENDMETHOD.

ENDCLASS.


CLASS ltc_instantiate_inhousedev IMPLEMENTATION.

  METHOD class_setup.

    TYPES ty_table_zsoar_inhousedev TYPE STANDARD TABLE OF zsoar_inhousedev WITH EMPTY KEY.

    DATA(table_zsoar_inhousedev) = VALUE ty_table_zsoar_inhousedev(
        ( srp_id               = 'ZSOAR_MANAGER_TEST_INHOUSEDEV'
          subroutine_pool_name = 'ZSOAR_MANAGER_TEST_INHOUSEDEV'
          inactive             = abap_false ) ).

    MODIFY zsoar_inhousedev FROM TABLE @table_zsoar_inhousedev.

    provider = NEW ltc_generate_subroutine_pool_2( ).
    manager = zcl_soar_manager=>zif_soar_manager~create( srp_id   = srp_id
                                                         provider = provider ).

  ENDMETHOD.


  METHOD create_object_absolute_type.

    DATA(test) = CAST zif_soar_manager_test( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD create_object_perform.

    DATA(test) = CAST zif_soar_manager_test( manager->create_object( class_name  = 'LCL_TEST'
                                                                     via_perform = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_absolute_type.

    DATA(test) = CAST zif_soar_manager_test( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SOAR_MANAGER_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_false ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD factory_method_perform.

    DATA(test) = CAST zif_soar_manager_test( manager->call_static_method( class_name    = 'LCL_TEST'
                                                                          method_name   = 'ZIF_SOAR_MANAGER_TEST~CREATE'
                                                                          result_object = VALUE #( parameter_name = 'RESULT'
                                                                                                   bound_optional = abap_false )
                                                                          via_perform   = abap_true ) ).
    cl_abap_unit_assert=>assert_bound( test ).

    DATA(square_of_3) = test->square( 3 ).
    cl_abap_unit_assert=>assert_equals( act = cl_abap_typedescr=>describe_by_object_ref( test )->absolute_name
                                        exp = '\PROGRAM=ZSOAR_MANAGER_TEST_INHOUSEDEV\CLASS=LCL_TEST' ).
    cl_abap_unit_assert=>assert_equals( act = square_of_3
                                        exp = 9 ).

  ENDMETHOD.


  METHOD zif_soar_provider~get_abap_source_code.

    cl_abap_unit_assert=>fail( msg = 'GET_ABAP_SOURCE_CODE is called (should not)' ).

  ENDMETHOD.


  METHOD zif_soar_provider~get_abap_hash_keys.

    cl_abap_unit_assert=>fail( msg = 'GET_ABAP_HASH_KEYS is called (should not)' ).

  ENDMETHOD.

ENDCLASS.
