*&---------------------------------------------------------------------*
*& Report ZFIO_CLNTCPY_ODATA_UNASGN_NEW
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zfio_clntcpy_odata_unasgn_new.

DATA  ls_mgdeam         TYPE /iwfnd/cl_mgw_inst_man_dba=>ty_gs_mgdeam.
DATA  lx_destin_finder  TYPE REF TO /iwfnd/cx_destin_finder.
DATA  lx_cof            TYPE REF TO /iwfnd/cx_cof.
DATA  lv_message        TYPE string.
DATA  lo_inst_man_dba   TYPE REF TO /iwfnd/cl_mgw_inst_man_dba.
DATA: l_srv_identifier TYPE /iwfnd/med_mdl_srg_identifier,
      msg_text(255)    TYPE c.

DATA lt_srv_identifier TYPE STANDARD TABLE OF /iwfnd/med_mdl_srg_identifier.

SELECTION-SCREEN BEGIN OF BLOCK sel WITH FRAME TITLE TEXT-001.

  TABLES /iwfnd/i_med_srh.
  SELECT-OPTIONS s_serv FOR /iwfnd/i_med_srh-srv_identifier NO INTERVALS MODIF ID p0. "Gateway Service/Servicio Gateway

  PARAMETERS: p_alias  LIKE /iwfnd/c_dfsyal-system_alias MATCHCODE OBJECT /iwfnd/sh_sap_sys_alias MODIF ID p0, "System Alias
              p_trsp   TYPE e071-trkorr MATCHCODE OBJECT irm_transport MODIF ID p0, "Transport Request/Orden de transporte
              p_check1 AS CHECKBOX USER-COMMAND check1, " Asignar system alias / assign system alias
              p_check2 AS CHECKBOX USER-COMMAND check2. " Eliminar system alias / delete system alias

SELECTION-SCREEN END OF BLOCK sel.

IF s_serv IS INITIAL OR p_alias IS INITIAL.
  MESSAGE 'Por favor llenar el system alias y el ID del servicio' TYPE 'I'.
ELSE.
  PERFORM mass_assign USING s_serv p_alias p_trsp.
ENDIF.


*ENDIF.
*&---------------------------------------------------------------------*
*& Form mass_assign
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> S_SERV
*&      --> P_ALIAS
*&---------------------------------------------------------------------*
FORM mass_assign  USING    p_s_serv
                           p_p_alias
                           p_p_trsp.

  DATA iv_transport TYPE trkorr.

  iv_transport = p_trsp.

  LOOP AT s_serv.

    CLEAR l_srv_identifier.
    l_srv_identifier = s_serv-low.

    CONCATENATE 'Z' l_srv_identifier '_0001' INTO l_srv_identifier.

    TRY .
        CLEAR ls_mgdeam.
        ls_mgdeam-service_id   = l_srv_identifier. " Servicio/service
        ls_mgdeam-system_alias = p_p_alias. "System Alias
        ls_mgdeam-is_default   = abap_true. "Default mode

        lo_inst_man_dba = /iwfnd/cl_mgw_inst_man_dba=>get_inst_man_dba( ).

        IF p_check1 IS NOT INITIAL.
          lo_inst_man_dba->create_mgdeam( is_mgdeam = ls_mgdeam iv_transport = iv_transport ).
        ELSE.
          lo_inst_man_dba->delete_mgdeam( is_mgdeam = ls_mgdeam iv_transport = iv_transport ).
        ENDIF.

      CATCH /iwfnd/cx_destin_finder INTO lx_destin_finder.
        lv_message = /iwfnd/cl_cof_util=>get_message_text( ix_message = lx_destin_finder ).
        MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.

      CATCH /iwfnd/cx_cof INTO lx_cof.
        lv_message = /iwfnd/cl_cof_util=>get_message_text( ix_message = lx_cof ).
        MESSAGE lv_message TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    APPEND l_srv_identifier TO lt_srv_identifier.

  ENDLOOP.

  IF p_check1 IS NOT INITIAL.
    WRITE:/10(67) 'Los siguientes servicios se actualizaron:'.
    NEW-LINE.
    WRITE:/10 sy-uline(70).
    WRITE:/10  sy-vline,
      (66) 'Service ID' COLOR COL_HEADING, sy-vline.
    NEW-LINE.
    WRITE:/10 sy-uline(70).
    LOOP AT lt_srv_identifier INTO l_srv_identifier.
      WRITE:/10 sy-vline,
            (66)l_srv_identifier, sy-vline.
    ENDLOOP.
    WRITE:/10 sy-uline(70).
  ELSE.
    WRITE:/10(67) 'Los siguientes servicios se eliminaron:'.
    NEW-LINE.
    WRITE:/10 sy-uline(70).
    WRITE:/10  sy-vline,
      (66) 'Service ID' COLOR COL_HEADING, sy-vline.
    NEW-LINE.
    WRITE:/10 sy-uline(70).
    LOOP AT lt_srv_identifier INTO l_srv_identifier.
      WRITE:/10 sy-vline,
            (66) l_srv_identifier, sy-vline.
    ENDLOOP.
    WRITE:/10 sy-uline(70).
  ENDIF.

ENDFORM.
