PROGRAM REDUCED FUNCTIONALITY.
INCLUDE zsoar_srpoo_forms.
CLASS lcl_soar_demo DEFINITION.
  PUBLIC SECTION.
    INTERFACES zif_soar_demo.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zif_soar_demo.
ENDCLASS.
CLASS lcl_soar_demo IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_soar_demo( ).
  ENDMETHOD.
  METHOD zif_soar_demo~popup.
    MESSAGE text TYPE 'I'.
  ENDMETHOD.
ENDCLASS.
