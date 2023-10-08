PROGRAM zsoar_demo_inhousedev_form REDUCED FUNCTIONALITY.
INCLUDE zsoar_srpoo_forms.
CLASS lcl_soar_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_soar_demo.
ENDCLASS.
CLASS lcl_soar_demo IMPLEMENTATION.
  METHOD zif_soar_demo~create.
    result = NEW lcl_soar_demo( ).
  ENDMETHOD.
  METHOD zif_soar_demo~popup.
    MESSAGE text TYPE 'I'.
  ENDMETHOD.
ENDCLASS.
