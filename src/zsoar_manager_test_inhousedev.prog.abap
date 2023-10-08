PROGRAM zsoar_manager_test_inhousedev REDUCED FUNCTIONALITY.
INCLUDE zsoar_srpoo_forms.
CLASS lcl_test DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_soar_manager_test.
ENDCLASS.
CLASS lcl_test IMPLEMENTATION.
  METHOD zif_soar_manager_test~create.
    result = NEW lcl_test( ).
  ENDMETHOD.
  METHOD zif_soar_manager_test~square.
    result = number ** 2.
  ENDMETHOD.
ENDCLASS.
