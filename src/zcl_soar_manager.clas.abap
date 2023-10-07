CLASS zcl_soar_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_soar_manager.

    TYPES: BEGIN OF ty_generate_subroutine_pool,
             name         TYPE syrepid,
             message      TYPE string,
             line         TYPE i,
             word         TYPE string,
             include      TYPE string,
             message_id   TYPE trmsg_key,
             offset       TYPE i,
             shortdump_id TYPE string,
             trace_file   TYPE string,
           END OF ty_generate_subroutine_pool.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA srp_id TYPE zsoar_srp_id.
    DATA info_generate_subroutine_pool TYPE ty_generate_subroutine_pool.
    DATA provider TYPE REF TO zif_soar_provider.
    DATA classes TYPE zif_soar_manager=>ty_classes.
    DATA zsoar_inhousedev TYPE zsoar_inhousedev.
    DATA srp_name TYPE zcl_soar_manager=>ty_generate_subroutine_pool-name.

    METHODS check_subroutine_pool
      IMPORTING
        subroutine_pool_name TYPE syrepid
      RAISING
        zcx_soar.

    METHODS generate_subroutine_pool
      IMPORTING
        abap_source_code TYPE zif_soar_manager=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE ty_generate_subroutine_pool
      RAISING
        zcx_soar.

    METHODS get_hash_key
      IMPORTING
        abap_source_code TYPE zif_soar_manager=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE string
      RAISING
        cx_abap_message_digest.

    METHODS initialize.

ENDCLASS.



CLASS zcl_soar_manager IMPLEMENTATION.

  METHOD check_subroutine_pool.

    SELECT SINGLE name, subc
        FROM trdir
        WHERE name = @subroutine_pool_name
        INTO @DATA(trdir).

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_soar
        EXPORTING
          text  = 'Subroutine pool &1 does not exist'(004)
          msgv1 = subroutine_pool_name.
    ELSEIF trdir-subc <> 'S'.
      RAISE EXCEPTION TYPE zcx_soar
        EXPORTING
          text  = 'Program &1 is of type &2 but should be S (Subroutine pool)'(005)
          msgv1 = subroutine_pool_name
          msgv2 = trdir-subc.
    ENDIF.

  ENDMETHOD.


  METHOD generate_subroutine_pool.

    GENERATE SUBROUTINE POOL abap_source_code
        NAME         result-name
        MESSAGE      result-message
        LINE         result-line
        WORD         result-word
        INCLUDE      result-include
        MESSAGE-ID   result-message_id
        OFFSET       result-offset
        SHORTDUMP-ID result-shortdump_id.

    IF sy-subrc <> 0.

      RAISE EXCEPTION TYPE zcx_soar
        EXPORTING
          text  = 'Generation error &1 at line &2: &3'(003)
          msgv1 = |{ sy-subrc }|
          msgv2 = |{ result-line }|
          msgv3 = result-message.

    ENDIF.

  ENDMETHOD.


  METHOD initialize.

    DATA error TYPE REF TO cx_root.

    classes = provider->get_classes( ).

    SELECT SINGLE *
        FROM zsoar_inhousedev
        INTO @zsoar_inhousedev
        WHERE srp_id = @srp_id.

    " In case a In-House Development subroutine pool is defined and exists (code being developed), use it.
    TRY.

        IF zsoar_inhousedev-subroutine_pool_name IS NOT INITIAL.

          check_subroutine_pool( zsoar_inhousedev-subroutine_pool_name ).

          srp_name = zsoar_inhousedev-subroutine_pool_name.

*            TRY.
*                DATA(instance) = provider->instantiate( srp_oo            = srp_oo
*                                                        public_class_name = local_class_name ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text = 'Call failed'(005) ).
*            ENDTRY.
*
*            TRY.
*                result = instance.
*              CATCH cx_sy_move_cast_error INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text  = 'Programming error: receiving variable type does not implement interface ZIF_XXXXXXXXX of &1. Please contact the support.'(006)
*                                              msgv1 = local_class_name ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text  = 'Programming error: receiving variable cannot be assigned the instance of &1. Please contact the support.'(007)
*                                              msgv1 = local_class_name ).
*            ENDTRY.

        ELSE.

          DATA(abap_source_code) = provider->get_abap_source_code( srp_id ).

          classes = provider->get_classes( ).

          DATA(class) = REF #( classes[ srp_id = srp_id ] OPTIONAL ).
          IF class IS NOT BOUND.
            RAISE EXCEPTION NEW zcx_soar( text  = 'Subroutine pool ID &1 not defined'(009)
                                          msgv1 = srp_id ).
          ENDIF.

          " Control the hash key
          TRY.
              DATA(hash_key) = get_hash_key( abap_source_code ).
            CATCH cx_abap_message_digest INTO error.
              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
          ENDTRY.

          IF hash_key <> class->hash_key.
            RAISE EXCEPTION NEW zcx_soar( text  = 'Invalid hash key for &1. Please contact the support.'(008)
                                          msgv1 = srp_id ).
          ENDIF.

          TRY.
              info_generate_subroutine_pool = generate_subroutine_pool( abap_source_code ).
            CATCH cx_root INTO error.
              RAISE EXCEPTION NEW zcx_soar( previous = error ).
          ENDTRY.

          srp_name = info_generate_subroutine_pool-name.

*            TRY.
*                result = provider->instantiate( srp_oo             = srp_oo
*                                                public_class_name = local_class_name ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( previous = error
*                                              text     = 'Call failed'(005) ).
*            ENDTRY.

        ENDIF.

*      CATCH cx_sy_dyn_call_illegal_class INTO DATA(inst_error).
*        " If it doesn't exist (INST_ERROR->TEXTID = INST_ERROR->UNKNOWN_CLASS and INST_ERROR->KERNEL_ERRID = 'DYN_CALL_METH_CLASS_NOT_FOUND'),
*        " either the subroutine pool was previously generated, just call it
*        " or upload the source code from the laptop + generate subroutine pool
*        " and then instantiate the class as usual.
*
*        IF inst_error->textid <> inst_error->unknown_class
*            AND inst_error->kernel_errid <> 'DYN_CALL_METH_CLASS_NOT_FOUND' ##NO_TEXT.
*          RAISE EXCEPTION NEW zcx_soar( text = 'Programming error: instantiation error "&1". Please contact the support.'(002) msgv1 = public_class_name ).
*        ENDIF.
*
*        DATA(gensrp) = VALUE #( gensrp_classes[ class_name = public_class_name ]-gensrp OPTIONAL ).
*
*        IF gensrp IS NOT BOUND.
*
**          try.
*          DATA(abap_source_code) = driver->get_abap_source_code( public_class_name ).
**          catch zcx_soar INTO error.
**              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
**              ENDTRY.
*
*          " Control the hash key
*          TRY.
*              DATA(hash_key) = get_hash_key( abap_source_code ).
*            CATCH cx_abap_message_digest INTO error.
*              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
*          ENDTRY.
*
*          IF hash_key <> class_info->hash_key.
*            RAISE EXCEPTION NEW zcx_soar( text = 'Integrity error. Please contact the support.'(008) msgv1 = public_class_name ).
*          ENDIF.
*
*          TRY.
*              gensrp = lcl_gensrp=>zif_soar_gensrp~generate_subroutine_pool( abap_source_code ).
**              gensrp = soar_driver->generate_subroutine_pool( public_class_name ).
*            CATCH cx_root INTO error.
*              RAISE EXCEPTION NEW zcx_soar( previous = error ).
*          ENDTRY.
*        ENDIF.
*
*        TRY.
*            result = driver->instantiate( gensrp            = gensrp
*                                          public_class_name = public_class_name ).
*          CATCH cx_root INTO error.
*            RAISE EXCEPTION NEW zcx_soar( text = 'Call failed'(005) ).
*        ENDTRY.

      CATCH cx_root INTO error.
        RAISE EXCEPTION NEW zcx_soar( previous = error
                                      text     = 'Call failed'(005) ).
    ENDTRY.

  ENDMETHOD.


  METHOD get_hash_key.

    cl_abap_message_digest=>calculate_hash_for_char(
      EXPORTING
        if_algorithm     = 'SHA-256'
        if_data          = concat_lines_of( sep = |\r\n| table = abap_source_code )
      IMPORTING
        ef_hashb64string = result ).

  ENDMETHOD.


  METHOD zif_soar_manager~create.

    DATA(soar_manager) = NEW zcl_soar_manager( ).

    soar_manager->srp_id = srp_id.
    soar_manager->provider = provider.

    soar_manager->initialize( ).

    result = soar_manager.

  ENDMETHOD.


  METHOD zif_soar_manager~call_static_method.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    "=============
    " Preparation
    "=============

    IF result_object-parameter_name IS NOT INITIAL
        AND NOT line_exists( parameters[ name = result_object-parameter_name ] ).
      " The result parameter is missing, append it (TYPE REF TO OBJECT)
      DATA(parameters_local) = VALUE abap_parmbind_tab(
                      BASE parameters
                      ( name  = result_object-parameter_name
                        value = NEW dba_object_ref( ) ) ).
      ASSIGN parameters_local TO <parameters>.
    ELSE.
      ASSIGN parameters TO <parameters>.
    ENDIF.

    "=============
    " Execution
    "=============

    IF via_perform = abap_false.

      DATA(absolute_name) = |\\PROGRAM={ srp_name }\\CLASS={ class_name }|.
*      DATA(dummy_exception_table) = VALUE abap_excpbind_tab( ).
      CALL METHOD (absolute_name)=>(method_name)
        PARAMETER-TABLE <parameters>
        EXCEPTION-TABLE exceptions_.
*        EXCEPTION-TABLE dummy_exception_table.

    ELSE.

      PERFORM call_static_method IN PROGRAM (srp_name)
        USING class_name
              method_name
              <parameters>
              exceptions_.

    ENDIF.

    "=============
    " Follow-up
    "=============

    IF result_object-parameter_name IS NOT INITIAL.

      DATA(ref_returning_parameter) = REF #( <parameters>[ name = result_object-parameter_name ] OPTIONAL ).
      ASSERT ref_returning_parameter IS BOUND.

      ASSIGN ref_returning_parameter->value->* TO FIELD-SYMBOL(<object>).
      TRY.
          result = <object>.
        CATCH cx_sy_move_cast_error.
          RAISE EXCEPTION TYPE zcx_soar
            EXPORTING
              text  = 'Receiving variable is of type &1 (must be REF TO OBJECT)'(001)
              msgv1 = cl_abap_typedescr=>describe_by_data( <object> )->absolute_name.
      ENDTRY.

      IF result_object-bound_optional = abap_false
            AND result IS NOT BOUND.
        RAISE EXCEPTION TYPE zcx_soar
          EXPORTING
            text = 'Result is not bound'(002).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_soar_manager~create_object.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    IF via_perform = abap_false.

      DATA(absolute_name) = |\\PROGRAM={ srp_name }\\CLASS={ class_name }|.
      " EXCEPTION-TABLE is required by kernels before note 2866213, even if no specific need.
      " NB: note 2866213 - ABAP short dump RUNT_ILLEGAL_SWITCH at CREATE OBJECT ... PARAMETER-TABLE
      " at https://me.sap.com/notes/2866213/E.
      CREATE OBJECT result TYPE (absolute_name)
                            PARAMETER-TABLE parameters
                            EXCEPTION-TABLE exceptions_.

    ELSE.

      PERFORM create_object IN PROGRAM (srp_name)
        USING    class_name
                 parameters
                 exceptions_
        CHANGING result.

    ENDIF.

*    IF result IS NOT BOUND.
*      RAISE EXCEPTION TYPE zcx_soar
*        EXPORTING
*          text = 'Result is not bound'(002).
*    ENDIF.

*    METHOD zif_soar_manager~instantiate_srp_class.
*
*      DATA error TYPE REF TO cx_root.
*
*      DATA(class_info) = REF #( classes[ local_class_name = local_class_name ] OPTIONAL ).
*      IF class_info IS NOT BOUND.
*        RAISE EXCEPTION NEW zcx_soar( text  = 'Programming error: class "&1" is not registered. Please contact the support.'(001)
*                                      msgv1 = local_class_name ).
*      ENDIF.
*
*      " In case a In-House Development subroutine pool is defined and exists (code being developed), use it.
*      TRY.
*          DATA(zsoar_inhousedev) = REF #( table_zsoar_inhousedev[ srp_id           = srp_id
*                                                                  local_class_name = local_class_name
*                                                                  inactive         = abap_false ] OPTIONAL ).
*          IF zsoar_inhousedev IS BOUND.
*
*            DATA(srp_oo) = zcl_soar_srpoo=>load_subroutine_pool( zsoar_inhousedev->subroutine_pool_name ).
*            TRY.
*                DATA(instance) = provider->instantiate( srp_oo            = srp_oo
*                                                        public_class_name = local_class_name ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text = 'Call failed'(005) ).
*            ENDTRY.
*
*            TRY.
*                result = instance.
*              CATCH cx_sy_move_cast_error INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text  = 'Programming error: receiving variable type does not implement interface ZIF_XXXXXXXXX of &1. Please contact the support.'(006)
*                                              msgv1 = local_class_name ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text  = 'Programming error: receiving variable cannot be assigned the instance of &1. Please contact the support.'(007)
*                                              msgv1 = local_class_name ).
*            ENDTRY.
*
*          ELSE.
*
*            DATA(abap_source_code) = provider->get_abap_source_code( local_class_name ).
*
*            " Control the hash key
*            TRY.
*                DATA(hash_key) = get_hash_key( abap_source_code ).
*              CATCH cx_abap_message_digest INTO error.
*                RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
*            ENDTRY.
*
*            IF hash_key <> class_info->hash_key.
*              RAISE EXCEPTION NEW zcx_soar( text  = 'Integrity error. Please contact the support.'(008)
*                                            msgv1 = local_class_name ).
*            ENDIF.
*
*            TRY.
*                DATA(result_generate_subroutine_poo) = zcl_soar_srpoo=>generate_subroutine_pool( abap_source_code ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( previous = error ).
*            ENDTRY.
*
*            srp_oo = result_generate_subroutine_poo-srpoo.
*
*            TRY.
*                result = provider->instantiate( srp_oo             = srp_oo
*                                                public_class_name = local_class_name ).
*              CATCH cx_root INTO error.
*                RAISE EXCEPTION NEW zcx_soar( previous = error
*                                              text     = 'Call failed'(005) ).
*            ENDTRY.
*
*          ENDIF.
*
**      CATCH cx_sy_dyn_call_illegal_class INTO DATA(inst_error).
**        " If it doesn't exist (INST_ERROR->TEXTID = INST_ERROR->UNKNOWN_CLASS and INST_ERROR->KERNEL_ERRID = 'DYN_CALL_METH_CLASS_NOT_FOUND'),
**        " either the subroutine pool was previously generated, just call it
**        " or upload the source code from the laptop + generate subroutine pool
**        " and then instantiate the class as usual.
**
**        IF inst_error->textid <> inst_error->unknown_class
**            AND inst_error->kernel_errid <> 'DYN_CALL_METH_CLASS_NOT_FOUND' ##NO_TEXT.
**          RAISE EXCEPTION NEW zcx_soar( text = 'Programming error: instantiation error "&1". Please contact the support.'(002) msgv1 = public_class_name ).
**        ENDIF.
**
**        DATA(gensrp) = VALUE #( gensrp_classes[ class_name = public_class_name ]-gensrp OPTIONAL ).
**
**        IF gensrp IS NOT BOUND.
**
***          try.
**          DATA(abap_source_code) = driver->get_abap_source_code( public_class_name ).
***          catch zcx_soar INTO error.
***              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
***              ENDTRY.
**
**          " Control the hash key
**          TRY.
**              DATA(hash_key) = get_hash_key( abap_source_code ).
**            CATCH cx_abap_message_digest INTO error.
**              RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
**          ENDTRY.
**
**          IF hash_key <> class_info->hash_key.
**            RAISE EXCEPTION NEW zcx_soar( text = 'Integrity error. Please contact the support.'(008) msgv1 = public_class_name ).
**          ENDIF.
**
**          TRY.
**              gensrp = lcl_gensrp=>zif_soar_gensrp~generate_subroutine_pool( abap_source_code ).
***              gensrp = soar_driver->generate_subroutine_pool( public_class_name ).
**            CATCH cx_root INTO error.
**              RAISE EXCEPTION NEW zcx_soar( previous = error ).
**          ENDTRY.
**        ENDIF.
**
**        TRY.
**            result = driver->instantiate( gensrp            = gensrp
**                                          public_class_name = public_class_name ).
**          CATCH cx_root INTO error.
**            RAISE EXCEPTION NEW zcx_soar( text = 'Call failed'(005) ).
**        ENDTRY.
*
*        CATCH cx_root INTO error.
*          RAISE EXCEPTION NEW zcx_soar( previous = error
*                                        text     = 'Call failed'(005) ).
*      ENDTRY.

  ENDMETHOD.

ENDCLASS.
