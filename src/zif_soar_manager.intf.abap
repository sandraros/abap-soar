INTERFACE zif_soar_manager
  PUBLIC .

  TYPES: BEGIN OF ty_class,
           srp_id           TYPE zsoar_srp_id,
           local_class_name TYPE seoclsname,
           hash_key         TYPE string,
         END OF ty_class.
  TYPES ty_classes TYPE STANDARD TABLE OF ty_class WITH EMPTY KEY.
  TYPES:
    BEGIN OF ty_result_object,
      parameter_name TYPE string,
      bound_optional TYPE abap_bool,
    END OF ty_result_object.
  TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  METHODS call_static_method
    IMPORTING
      class_name    TYPE seoclsname
      method_name   TYPE seocmpname
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
      VALUE(result) TYPE REF TO zif_soar_manager.

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

*  METHODS instantiate_srp_class
*    IMPORTING
*      srp_id           TYPE zsoar_srp_id
*      local_class_name TYPE seoclsname
*    RETURNING
*      VALUE(result)    TYPE REF TO object
*    RAISING
*      zcx_soar.

ENDINTERFACE.
