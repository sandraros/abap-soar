*"* use this source file for your ABAP unit test classes


*INTERFACE lif_oa_1.
*
*  INTERFACES zif_soar_driver.
*
*  METHODS square
*    IMPORTING
*      number        TYPE numeric
*    RETURNING
*      VALUE(result) TYPE decfloat34.
*
*ENDINTERFACE.
*
*
*CLASS lcl_oa_1 DEFINITION
*      FOR TESTING.
*
*  PUBLIC SECTION.
*
*    INTERFACES lif_oa_1.
*
*ENDCLASS.
*
*
*CLASS lth_soar_driver DEFINITION
*      FOR TESTING.
*
*  PUBLIC SECTION.
*
*    INTERFACES zif_soar_driver.
*
*ENDCLASS.
*
*
*CLASS ltc_main DEFINITION
*      FOR TESTING
*      DURATION SHORT
*      RISK LEVEL HARMLESS.
*
*  PRIVATE SECTION.
*
*    METHODS test FOR TESTING RAISING cx_static_check.
*
*ENDCLASS.
*
*
*CLASS lcl_oa_1 IMPLEMENTATION.
*
*  METHOD zif_soar_driver~generate_subroutine_pool.
*
*  ENDMETHOD.
*
*  METHOD zif_soar_driver~instantiate.
*
*  ENDMETHOD.
*
*  METHOD lif_oa_1~square.
*
*    result = number ** 2.
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS lth_soar_driver IMPLEMENTATION.
*
*  METHOD zif_soar_driver~generate_subroutine_pool.
*
*    result = lcl_gensrp=>zif_soar_gensrp~generate_subroutine_pool( VALUE #(
*                ( || ) ) ).
*
*  ENDMETHOD.
*
*
*  METHOD zif_soar_driver~instantiate.
*
*
*
*  ENDMETHOD.
*
*ENDCLASS.
*
*
*CLASS ltc_main IMPLEMENTATION.
*
*  METHOD test.
*
*    DATA(soar_manager) = zcl_soar_manager=>zif_soar_manager~create( NEW lth_soar_driver( ) ).
*    soar_manager->set_outsourced_classes( VALUE #(
*        ( interface_name = '' public_class_name = '' in_development_class_name = '' hash_key = '' ) ) ).
*
*    DATA(oa_1) = CAST lif_oa_1( soar_manager->instantiate( 'LCL_OA_1' ) ).
*    DATA(square_of_5) = oa_1->square( 5 ).
*
*    cl_abap_unit_assert=>assert_equals(
*        act = square_of_5
*        exp = 25 ).
*
*  ENDMETHOD.
*
*ENDCLASS.
