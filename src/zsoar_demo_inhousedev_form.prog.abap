PROGRAM REDUCED FUNCTIONALITY.
INCLUDE zsoar_srpoo_forms.
CLASS lcl_soar_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_soar_demo.
ENDCLASS.
CLASS lcl_soar_demo IMPLEMENTATION.
  METHOD zif_soar_demo~popup.
    MESSAGE text TYPE 'I'.
  ENDMETHOD.
ENDCLASS.
