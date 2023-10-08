INTERFACE zif_soar_manager
  PUBLIC .

  TYPES: BEGIN OF ty_abap_hash_key,
           srp_id   TYPE zsoar_srp_id,
           hash_key TYPE zsoar_abap_hash_key,
         END OF ty_abap_hash_key.
  TYPES ty_abap_hash_keys TYPE HASHED TABLE OF ty_abap_hash_key WITH UNIQUE KEY srp_id.
  TYPES:
    "! Description of the RETURNING parameter of a class method if the sense it's
    "! expected to be a factory method).
    BEGIN OF ty_result_object,
      parameter_name TYPE string,
      bound_optional TYPE abap_bool,
    END OF ty_result_object.
  TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.
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

  DATA srp_name TYPE ty_generate_subroutine_pool-name READ-ONLY.

  METHODS call_static_method
    IMPORTING
      class_name    TYPE seoclsname
      method_name   TYPE seomtdname
      parameters    TYPE abap_parmbind_tab OPTIONAL
      exceptions_   TYPE abap_excpbind_tab OPTIONAL
      result_object TYPE ty_result_object OPTIONAL
      via_perform   TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE REF TO object
    RAISING
      cx_static_check
      cx_dynamic_check.

  CLASS-METHODS create
    IMPORTING
      srp_id        TYPE zsoar_srp_id
      provider      TYPE REF TO zif_soar_provider
    RETURNING
      VALUE(result) TYPE REF TO zif_soar_manager
    RAISING
      zcx_soar.

  METHODS create_object
    IMPORTING
      class_name    TYPE seoclsname
      parameters    TYPE abap_parmbind_tab OPTIONAL
      exceptions_   TYPE abap_excpbind_tab OPTIONAL
      via_perform   TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(result) TYPE REF TO object
    RAISING
      cx_static_check
      cx_dynamic_check.

  METHODS get_last_generate_subr_pool
    RETURNING
      VALUE(result) TYPE ty_generate_subroutine_pool.

ENDINTERFACE.
