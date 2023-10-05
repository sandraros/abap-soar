*&---------------------------------------------------------------------*
*& Report zsoar_demo
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsoar_demo.

CLASS lcl_soar_provider DEFINITION
CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_soar_driver.
    CLASS-METHODS create
      RETURNING
        VALUE(result) TYPE REF TO zif_soar_driver.
ENDCLASS.

CLASS lcl_soar_provider IMPLEMENTATION.
  METHOD create.
    result = NEW lcl_soar_provider( ).
  ENDMETHOD.
  METHOD zif_soar_driver~get_abap_source_code.
    result = VALUE #(
            ( `PROGRAM.` )
            ( `INCLUDE zsoar_gensrp_forms.` )
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
  METHOD zif_soar_driver~get_classes.
    result = VALUE #(
            ( interface_name = 'ZIF_SOAR_DEMO' public_class_name = 'LCL_SOAR_DEMO' hash_key = 'zksFQhfRoiWQ9X+FlukkbtktEprwjGsAkfpphOGIhLI=' ) ).
  ENDMETHOD.
  METHOD zif_soar_driver~instantiate.
    result = SWITCH #( public_class_name
            WHEN 'LCL_SOAR_DEMO' THEN gensrp->create_object( class_name = 'LCL_SOAR_DEMO' ) ).
  ENDMETHOD.
ENDCLASS.

CLASS ltc_same_as_start_of_selection DEFINITION
      FOR TESTING
      DURATION SHORT
      RISK LEVEL HARMLESS.
  PRIVATE SECTION.
    METHODS test FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltc_same_as_start_of_selection IMPLEMENTATION.
  METHOD test.
    " 0. Instantiate SOAR data provider
    DATA(soar_provider) = lcl_soar_provider=>create( ).
    " 1. Create SOAR instance with SOAR data provider
    DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( soar_provider ).
    " 2. Request instance of any class in ABAP provider
    DATA(demo) = CAST zif_soar_demo( soar_manager->instantiate( 'LCL_SOAR_DEMO' ) ).
    " 6. Direct call of methods -> calls GSRP.
    demo->popup( 'Hello world' ).
  ENDMETHOD.
ENDCLASS.

PARAMETERS outsourc RADIOBUTTON GROUP rb1 DEFAULT 'X'.
PARAMETERS inhouse  RADIOBUTTON GROUP rb1.

START-OF-SELECTION.
  " 0. Instantiate SOAR data provider
  DATA(soar_provider) = lcl_soar_provider=>create( ).
  " 1. Create SOAR instance with SOAR data provider
  DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( soar_provider ).
  " 2. Request instance of any class in ABAP provider
  DATA(demo) = CAST zif_soar_demo( soar_manager->instantiate( 'LCL_SOAR_DEMO' ) ).
  " 6. Direct call of methods -> calls GSRP.
  demo->popup( 'Hello world' ).
