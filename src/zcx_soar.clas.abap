**********************************************************************
*
* https://github.com/sandraros/abap-soar
*
**********************************************************************
*
* MIT License
*
* Copyright (c) 2023 sandraros
*
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
*
* The above copyright notice and this permission notice shall be included in all
* copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
* SOFTWARE.
*
**********************************************************************
CLASS zcx_soar DEFINITION
  INHERITING FROM cx_static_check
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS zcx_soar TYPE sotr_conc VALUE 'E7D1A9770C521EEE99B6A62EF80A8270'.

    METHODS constructor
      IMPORTING
        text                    TYPE clike OPTIONAL
        msgv1                   TYPE clike OPTIONAL
        msgv2                   TYPE clike OPTIONAL
        msgv3                   TYPE clike OPTIONAL
        msgv4                   TYPE clike OPTIONAL
        textid                  LIKE textid DEFAULT zcx_soar
        previous                TYPE REF TO cx_root OPTIONAL
        substitute_placeholders TYPE abap_bool DEFAULT abap_true.

    METHODS get_text REDEFINITION.

    METHODS get_longtext REDEFINITION.

  PRIVATE SECTION.

    DATA text TYPE string.
    DATA msgv1 TYPE string.
    DATA msgv2 TYPE string.
    DATA msgv3 TYPE string.
    DATA msgv4 TYPE string.

ENDCLASS.


CLASS zcx_soar IMPLEMENTATION.

  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( textid   = textid
                        previous = previous ).
    me->text = text.
    me->msgv1 = msgv1.
    me->msgv2 = msgv2.
    me->msgv3 = msgv3.
    me->msgv4 = msgv4.
    IF me->text IS NOT INITIAL.
      IF substitute_placeholders = abap_true.
        me->text = replace( val = me->text sub = '&1' with = msgv1 ).
        me->text = replace( val = me->text sub = '&2' with = msgv2 ).
        me->text = replace( val = me->text sub = '&3' with = msgv3 ).
        me->text = replace( val = me->text sub = '&4' with = msgv4 ).
      ENDIF.
    ELSE.
      me->text = 'General error, please contact the support.'(001).
    ENDIF.
  ENDMETHOD.


  METHOD get_text.
    result = text.
  ENDMETHOD.


  METHOD get_longtext.
    result = get_text( ).
  ENDMETHOD.

ENDCLASS.

