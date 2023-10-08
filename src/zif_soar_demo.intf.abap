INTERFACE zif_soar_demo
  PUBLIC .
  CLASS-METHODS create
    RETURNING
      VALUE(result) TYPE REF TO zif_soar_demo.
  METHODS popup
    IMPORTING
      text TYPE csequence.
ENDINTERFACE.
