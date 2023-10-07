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
      IMPORTING
        instantiate_via_perform TYPE abap_bool
      RETURNING
        VALUE(result)           TYPE REF TO zif_soar_provider.

  PRIVATE SECTION.

    DATA instantiate_via_perform TYPE abap_bool.

ENDCLASS.


CLASS lcl_soar_provider IMPLEMENTATION.

  METHOD create.
    DATA(provider) = NEW lcl_soar_provider( ).
    provider->instantiate_via_perform = instantiate_via_perform.
    result = provider.
  ENDMETHOD.

  METHOD zif_soar_provider~get_abap_source_code.
    result = VALUE #(
            ( `PROGRAM.` )
            ( LINES OF COND #( WHEN instantiate_via_perform = abap_true THEN VALUE #(
                ( `INCLUDE zsoar_gensrp_forms.` ) ) ) )
            ( `CLASS lcl_soar_demo DEFINITION.` )
            ( `  PUBLIC SECTION.` )
            ( `    INTERFACES zif_soar_demo.` )
            ( `ENDCLASS.` )
            ( `CLASS lcl_soar_demo IMPLEMENTATION.` )
            ( `  METHOD zif_soar_demo~popup.` )
            ( `    MESSAGE text TYPE 'I'.` )
            ( `  ENDMETHOD.` )
            ( `ENDCLASS.` ) ).
  ENDMETHOD.

  METHOD zif_soar_provider~get_classes.
    result = VALUE #( ( srp_id   = 'ZSOAR_DEMO'
                        hash_key = 'zksFQhfRoiWQ9X+FlukkbtktEprwjGsAkfpphOGIhLI=' ) ).
  ENDMETHOD.

  METHOD zif_soar_provider~instantiate.
    result = SWITCH #( public_class_name
            WHEN 'LCL_SOAR_DEMO' THEN srp_oo->create_object( class_name  = 'LCL_SOAR_DEMO'
                                                             via_perform = instantiate_via_perform ) ).
  ENDMETHOD.

ENDCLASS.


CLASS ltc_same_as_start_of_selection DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.

  PRIVATE SECTION.

    METHODS instantiate_via_absolute_type FOR TESTING RAISING cx_static_check.

ENDCLASS.


CLASS ltc_same_as_start_of_selection IMPLEMENTATION.

  METHOD instantiate_via_absolute_type.
    DATA(soar_provider) = lcl_soar_provider=>create( instantiate_via_perform = abap_false ).
    DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( srp_id   = 'ZSOAR_DEMO'
                                                                    provider = soar_provider ).
    DATA(demo) = CAST zif_soar_demo( soar_manager->create_object( class_name  = 'LCL_SOAR_DEMO'
                                                                  via_perform = abap_false ) ).
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

INITIALIZATION.
  text_b01 = 'Outsourced/In-house'(b01).
  text_b02 = 'Subroutine pool code with subroutines or full ABAP Objects'(b02).

START-OF-SELECTION.
  DATA(zsoar_inhousedev) = VALUE zsoar_inhousedev(
        srp_id               = 'ZSOAR_DEMO'
        subroutine_pool_name = COND #( WHEN form = abap_true
                                       THEN 'ZSOAR_DEMO_INHOUSEDEV_FORM'
                                       ELSE 'ZSOAR_DEMO_INHOUSEDEV_FULL_OO' )
        inactive             = xsdbool( inhouse = abap_false ) ).
  MODIFY zsoar_inhousedev FROM @zsoar_inhousedev.
  COMMIT WORK.

  DATA(soar_provider) = lcl_soar_provider=>create( instantiate_via_perform = xsdbool( form = abap_true ) ).
  DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( srp_id   = 'ZSOAR_DEMO'
                                                                  provider = soar_provider ).
  DATA(demo) = CAST zif_soar_demo( soar_manager->create_object( class_name  = 'LCL_SOAR_DEMO'
                                                                via_perform = abap_false ) ).
*  DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( provider = soar_provider ).
  demo->popup( 'Hello world' ).
