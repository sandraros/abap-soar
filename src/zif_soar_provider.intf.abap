INTERFACE zif_soar_provider
  PUBLIC .

  TYPES ty_abap_source_code TYPE STANDARD TABLE OF string WITH EMPTY KEY.

  METHODS get_abap_source_code
    IMPORTING
      srp_id        type zsoar_srp_id
    RETURNING
      VALUE(result) TYPE ty_abap_source_code
    RAISING
      zcx_soar.

ENDINTERFACE.
