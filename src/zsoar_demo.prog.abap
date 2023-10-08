*&---------------------------------------------------------------------*
*& Report zsoar_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsoar_demo.

CLASS lcl_soar_provider DEFINITION
        CREATE PRIVATE.

  PUBLIC SECTION.

    INTERFACES zif_soar_provider.

    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zif_soar_provider.

ENDCLASS.


CLASS lcl_soar_provider IMPLEMENTATION.

  METHOD create.

    result = NEW lcl_soar_provider( ).

  ENDMETHOD.

  METHOD zif_soar_provider~get_abap_source_code.

    result = VALUE #(
            ( `PROGRAM.` )
            ( `INCLUDE zsoar_srpoo_forms.                      ` )
            ( `CLASS lcl_soar_demo DEFINITION.                 ` )
            ( `  PUBLIC SECTION.                               ` )
            ( `    INTERFACES zif_soar_demo.                   ` )
            ( `ENDCLASS.                                       ` )
            ( `CLASS lcl_soar_demo IMPLEMENTATION.             ` )
            ( `  METHOD zif_soar_demo~create.                  ` )
            ( `    result = NEW lcl_soar_demo( ).              ` )
            ( `  ENDMETHOD.                                    ` )
            ( `  METHOD zif_soar_demo~popup.                   ` )
            ( `    MESSAGE text TYPE 'I'.                      ` )
            ( `  ENDMETHOD.                                    ` )
            ( `ENDCLASS.                                       ` ) ).

  ENDMETHOD.

ENDCLASS.


SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE text_b01.
PARAMETERS outsourc RADIOBUTTON GROUP rb1 DEFAULT 'X'.
PARAMETERS inhouse  RADIOBUTTON GROUP rb1.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE text_b02.
PARAMETERS form RADIOBUTTON GROUP rb2 DEFAULT 'X'.
PARAMETERS full_oo  RADIOBUTTON GROUP rb2.
SELECTION-SCREEN END OF BLOCK b02.

SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text_b03.
PARAMETERS factory RADIOBUTTON GROUP rb3 DEFAULT 'X'.
PARAMETERS createob RADIOBUTTON GROUP rb3.
SELECTION-SCREEN END OF BLOCK b03.

INITIALIZATION.
  text_b01 = 'Outsourced/In-house'(b01).
  text_b02 = 'Way to create object/call method of local class in Subroutine Pool'(b02).
  text_b03 = 'Instantiate via factory method or via create object'(b03).

START-OF-SELECTION.

  TRY.

      DATA(zsoar_inhousedev) = VALUE zsoar_inhousedev(
            srp_id               = 'ZSOAR_DEMO'
            subroutine_pool_name = COND #( WHEN form = abap_true
                                           THEN 'ZSOAR_DEMO_INHOUSEDEV_FORM'
                                           ELSE 'ZSOAR_DEMO_INHOUSEDEV_FULL_OO' )
            inactive             = xsdbool( inhouse = abap_false ) ).
      MODIFY zsoar_inhousedev FROM @zsoar_inhousedev.
      COMMIT WORK.

      DATA(soar_provider) = lcl_soar_provider=>create( ).
      DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( srp_id   = 'ZSOAR_DEMO'
                                                                      provider = soar_provider ).
      DATA(demo) = CAST zif_soar_demo(
                    COND #(
                    WHEN factory = abap_true
                    THEN soar_manager->call_static_method( class_name    = 'LCL_SOAR_DEMO'
                                                           method_name   = 'ZIF_SOAR_DEMO~CREATE'
                                                           result_object = VALUE #(
                                                                            parameter_name = 'RESULT'
                                                                            bound_optional = abap_false )
                                                           via_perform   = xsdbool( form = abap_true ) )
                    ELSE soar_manager->create_object( class_name  = 'LCL_SOAR_DEMO'
                                                      via_perform = xsdbool( form = abap_true ) ) ) ).
      demo->popup( 'Hello world' ).

    CATCH cx_root INTO DATA(error).
      MESSAGE error TYPE 'I' DISPLAY LIKE 'E'.
  ENDTRY.
