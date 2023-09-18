CLASS zcl_soar_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_soar_manager.

  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES: BEGIN OF ty_gensrp_class,
             class_name TYPE seoclsname,
             gensrp     TYPE REF TO zif_soar_gensrp,
           END OF ty_gensrp_class.
    TYPES ty_gensrp_classes TYPE HASHED TABLE OF ty_gensrp_class WITH UNIQUE KEY class_name.

    DATA driver TYPE REF TO zif_soar_driver.
    DATA gensrp TYPE REF TO zif_soar_gensrp.
    DATA classes TYPE zif_soar_manager=>ty_classes.
    DATA gensrp_classes TYPE ty_gensrp_classes.

    METHODS get_hash_key
      IMPORTING
        abap_source_code TYPE zif_soar_gensrp=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE string
      RAISING
        cx_abap_message_digest.

ENDCLASS.



CLASS zcl_soar_manager IMPLEMENTATION.

  METHOD get_hash_key.

    cl_abap_message_digest=>calculate_hash_for_char(
      EXPORTING
        if_algorithm     = 'SHA2'
        if_data          = concat_lines_of( sep = |\r\n| table = abap_source_code )
      IMPORTING
        ef_hashb64string = result ).

  ENDMETHOD.


  METHOD zif_soar_manager~create.

    DATA(soar_manager) = NEW zcl_soar_manager( ).
    soar_manager->driver = driver.

    result = soar_manager.

  ENDMETHOD.


  METHOD zif_soar_manager~instantiate.

    TYPES ty_ref_to_zif_soar_driver TYPE REF TO zif_soar_driver.
    DATA error TYPE REF TO cx_root.

    DATA(class_info) = REF #( classes[ public_class_name = public_class_name ] OPTIONAL ).
    IF class_info IS NOT BOUND.
      RAISE EXCEPTION NEW zcx_soar( text = 'Programming error: class "&1" is not registered. Please contact the support.'(001) msgv1 = public_class_name ).
    ENDIF.

    " In case the static version of the class exists (class pool in development), use it.
    TRY.
        DATA(soar_driver) = VALUE ty_ref_to_zif_soar_driver( ).
        CALL METHOD (class_info->in_development_class_name)=>zif_soar_driver~instantiate
          RECEIVING
            result = soar_driver.

        TRY.
            result = soar_driver.
          CATCH cx_sy_move_cast_error INTO error.
            RAISE EXCEPTION NEW zcx_soar( text = 'Programming error: receiving variable type does not implement interface ZIF_ACNIP_KEP_EXT_ABAP_CLASS of &1. Please contact the support.'(006) msgv1 = public_class_name ).
          CATCH cx_root INTO error.
            RAISE EXCEPTION NEW zcx_soar( text = 'Programming error: receiving variable cannot be assigned the instance of &1. Please contact the support.'(007) msgv1 = public_class_name ).
        ENDTRY.

      CATCH cx_sy_dyn_call_illegal_class INTO DATA(inst_error).
        " If it doesn't exist (INST_ERROR->TEXTID = INST_ERROR->UNKNOWN_CLASS and INST_ERROR->KERNEL_ERRID = 'DYN_CALL_METH_CLASS_NOT_FOUND'),
        " either the subroutine pool was previously generated, just call it
        " or upload the source code from the laptop + generate subroutine pool
        " and then instantiate the class as usual.

        IF inst_error->textid <> inst_error->unknown_class
            AND inst_error->kernel_errid <> 'DYN_CALL_METH_CLASS_NOT_FOUND' ##NO_TEXT.
          RAISE EXCEPTION NEW zcx_soar( text = 'Programming error: instantiation error "&1". Please contact the support.'(002) msgv1 = public_class_name ).
        ENDIF.

        DATA(gensrp) = VALUE #( gensrp_classes[ class_name = public_class_name ]-gensrp OPTIONAL ).



        IF gensrp IS NOT BOUND.

*          try.
          DATA(abap_source_code) = soar_driver->get_abap_source_code( public_class_name ).
*          catch zcx_soar INTO error.
*              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
*              ENDTRY.

          " Control the hash key
          TRY.
              DATA(hash_key) = get_hash_key( abap_source_code ).
            CATCH cx_abap_message_digest INTO error.
              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
          ENDTRY.

          IF hash_key <> class_info->hash_key.
            RAISE EXCEPTION NEW zcx_soar( text = 'Integrity error. Please contact the support.'(008) msgv1 = public_class_name ).
          ENDIF.

          TRY.
              gensrp = lcl_gensrp=>zif_soar_gensrp~generate_subroutine_pool( abap_source_code ).
*              gensrp = soar_driver->generate_subroutine_pool( public_class_name ).
            CATCH cx_root INTO error.
              RAISE EXCEPTION NEW zcx_soar( previous = error ).
          ENDTRY.
        ENDIF.

        TRY.
            result = driver->instantiate( gensrp            = gensrp
                                          public_class_name = public_class_name ).
          CATCH cx_root INTO error.
            RAISE EXCEPTION NEW zcx_soar( text = 'Call failed'(005) ).
        ENDTRY.

      CATCH cx_root INTO error.
        RAISE EXCEPTION NEW zcx_soar( previous = error text = 'Call failed'(005) ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_soar_manager~set_outsourced_classes.

    me->classes = classes.

  ENDMETHOD.

ENDCLASS.
