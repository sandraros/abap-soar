CLASS zcl_soar_manager DEFINITION
  PUBLIC
  FINAL
  CREATE PRIVATE .

  PUBLIC SECTION.

    INTERFACES zif_soar_manager.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA srp_id TYPE zsoar_srp_id.
    DATA info_generate_subroutine_pool TYPE zif_soar_manager=>ty_generate_subroutine_pool.
    DATA provider TYPE REF TO zif_soar_provider.
    DATA classes TYPE zif_soar_manager=>ty_abap_hash_keys.
    DATA zsoar_inhousedev TYPE zsoar_inhousedev.

    METHODS check_subroutine_pool
      IMPORTING
        subroutine_pool_name TYPE syrepid
      RAISING
        zcx_soar.

    METHODS generate_subroutine_pool
      IMPORTING
        abap_source_code TYPE zif_soar_manager=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE zif_soar_manager=>ty_generate_subroutine_pool
      RAISING
        zcx_soar.

    METHODS get_hash_key
      IMPORTING
        abap_source_code TYPE zif_soar_manager=>ty_abap_source_code
      RETURNING
        VALUE(result)    TYPE string
      RAISING
        cx_abap_message_digest.

    METHODS initialize
      RAISING
        zcx_soar.

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

    classes = provider->get_abap_hash_keys( ).

    SELECT SINGLE *
        FROM zsoar_inhousedev
        INTO @zsoar_inhousedev
        WHERE srp_id = @srp_id.

    IF zsoar_inhousedev-subroutine_pool_name IS NOT INITIAL.

      check_subroutine_pool( zsoar_inhousedev-subroutine_pool_name ).

      zif_soar_manager~srp_name = zsoar_inhousedev-subroutine_pool_name.

    ELSE.

      DATA(abap_source_code) = provider->get_abap_source_code( srp_id ).

      classes = provider->get_abap_hash_keys( ).

      DATA(class) = REF #( classes[ srp_id = srp_id ] OPTIONAL ).
      IF class IS NOT BOUND.
        RAISE EXCEPTION NEW zcx_soar( text  = 'Subroutine pool ID &1 not defined'(009)
                                      msgv1 = srp_id ).
      ENDIF.

      " Calculate the hash key
      TRY.
          DATA(hash_key) = get_hash_key( abap_source_code ).
        CATCH cx_abap_message_digest INTO error.
          RAISE EXCEPTION NEW zcx_soar( text = 'Hash key calculation error. Please contact the support.'(006) ).
      ENDTRY.

      " Check authorizations for the hash key
      " Check whether the hash key is permitted
      IF hash_key = class->hash_key.

*        DATA(xuval_hash_key) = CONV xuval( hash_key ). " Truncate after 40 characters
*        AUTHORITY-CHECK OBJECT 'ZSOAR_HASH'
*            ID 'SRP_ID'        FIELD srp_id
*            ID 'ABAP_HASH_KEY' FIELD xuval_hash_key.
*        IF sy-subrc <> 0.
*
*          AUTHORITY-CHECK OBJECT 'ZSOAR_DATE'
*              ID 'SRP_ID' FIELD srp_id
*              ID 'DATE'   FIELD sy-datum.
*
*          IF sy-subrc <> 0.
*            RAISE EXCEPTION NEW zcx_soar( text  = 'Current version of ABAP code is not authorized'(010)
*                                          msgv1 = srp_id ).
*          ENDIF.
*        ENDIF.

      ELSE.

        RAISE EXCEPTION NEW zcx_soar( text  = 'Invalid hash key for &1. Act: &2. Exp: &3. Please contact the support.'(008)
                                      msgv1 = srp_id
                                      msgv2 = hash_key
                                      msgv3 = class->hash_key ).

      ENDIF.

      " Generate the subroutine pool
      info_generate_subroutine_pool = generate_subroutine_pool( abap_source_code ).

      zif_soar_manager~srp_name = info_generate_subroutine_pool-name.

    ENDIF.

  ENDMETHOD.


  METHOD get_hash_key.

    cl_abap_message_digest=>calculate_hash_for_char(
      EXPORTING
        if_algorithm     = 'SHA-256'
        if_data          = concat_lines_of( sep = |\r\n| table = abap_source_code )
      IMPORTING
        ef_hashb64string = result ).

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

      DATA(absolute_name) = |\\PROGRAM={ zif_soar_manager~srp_name }\\CLASS={ class_name }|.
      CALL METHOD (absolute_name)=>(method_name)
        PARAMETER-TABLE <parameters>
        EXCEPTION-TABLE exceptions_.

    ELSE.

      PERFORM call_static_method IN PROGRAM (zif_soar_manager~srp_name)
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


  METHOD zif_soar_manager~create.

    DATA(soar_manager) = NEW zcl_soar_manager( ).

    soar_manager->srp_id = srp_id.
    soar_manager->provider = provider.

    soar_manager->initialize( ).

    result = soar_manager.

  ENDMETHOD.


  METHOD zif_soar_manager~create_object.
    FIELD-SYMBOLS <parameters> TYPE abap_parmbind_tab.

    IF via_perform = abap_false.

      DATA(absolute_name) = |\\PROGRAM={ zif_soar_manager~srp_name }\\CLASS={ class_name }|.
      CREATE OBJECT result TYPE (absolute_name)
                            PARAMETER-TABLE parameters
                            EXCEPTION-TABLE exceptions_.

    ELSE.

      PERFORM create_object IN PROGRAM (zif_soar_manager~srp_name)
        USING    class_name
                 parameters
                 exceptions_
        CHANGING result.

    ENDIF.

  ENDMETHOD.


  METHOD zif_soar_manager~get_last_generate_subr_pool.

    result = info_generate_subroutine_pool.

  ENDMETHOD.

ENDCLASS.
