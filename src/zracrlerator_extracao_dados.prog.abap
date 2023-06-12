*&---------------------------------------------------------------------*
*& Report ZRACRLERATOR_EXTRACAO_DADOS
*&---------------------------------------------------------------------*
*&*--------------------------------------------------------------------*
*  Programa de Extração de dados                                       *
*  Nome do arquivo: ZRACRLERATOR_EXTRACAO_DADOS                        *
*  Autor: Marcos A. Stocklen                                           *
*  Adaptado por: Lilian Raissa Gomes Silva                             *
*  Adicionado ao projeto acelerador por: Ana Chaves                    *
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
REPORT zracrlerator_extracao_dados.


TABLES: dd25l ##NEEDED ,
        dd02l ##NEEDED ,
        sscrfields ##NEEDED .
INCLUDE <icon> .

DATA: go_struct_type TYPE REF TO cl_abap_structdescr,
      go_itab_type   TYPE REF TO cl_abap_tabledescr,
      go_dref        TYPE REF TO data.

DATA: gv_name  TYPE viewname,
      gv_selid TYPE rsdynsel-selid,
      gv_erro  TYPE xfeld.

DATA: gt_table_tab  TYPE TABLE OF rsdstabs,
      gt_field_tab  TYPE TABLE OF rsdsfields,
      gt_field_list TYPE ddfields,
      gt_cond_tab   TYPE rsds_twhere.

FIELD-SYMBOLS : <gt_outtab>      TYPE ANY TABLE,
                <gwa_outline>    TYPE any,
                <gt_outtablocal> TYPE ANY TABLE,
                <gwa_cond>       LIKE LINE OF gt_cond_tab.

*************************************************************
*INITIALIZATION                                             *
*************************************************************
INITIALIZATION.

  DATA: lwa_text    TYPE smp_dyntxt.

  MOVE: icon_fencing TO lwa_text-icon_id,
        'Seleção Dinâmica'(270) TO lwa_text-text.

  sscrfields-functxt_01 = lwa_text.

  SELECTION-SCREEN FUNCTION KEY 1 .


*************************************************************
*Selection-Screen                                           *
*************************************************************
  SELECTION-SCREEN BEGIN OF BLOCK b_01 WITH FRAME TITLE TEXT-001.

    PARAMETERS: p_view TYPE dd25l-viewname,
                p_tab  TYPE dd02l-tabname.

    PARAMETERS: p_rvis TYPE char01 RADIOBUTTON GROUP gr2 USER-COMMAND tabvis DEFAULT 'X'.
    PARAMETERS: p_rtab TYPE char01 RADIOBUTTON GROUP gr2.

  SELECTION-SCREEN END OF BLOCK b_01.

  SELECTION-SCREEN BEGIN OF BLOCK b_02 WITH FRAME TITLE TEXT-002.

    PARAMETERS: p_arq TYPE rlgrap-filename.
    PARAMETERS: p_hard TYPE char01 RADIOBUTTON GROUP gr1 USER-COMMAND path DEFAULT 'X'.
    PARAMETERS: p_serv TYPE char01 RADIOBUTTON GROUP gr1 ##NEEDED.

* parameter to save free selection into variant
    PARAMETERS: pt_free TYPE rsds_trange    NO-DISPLAY, "Tabela de parametro usado em tela, não demonstrada
                pt_cond TYPE rsds_twhere    NO-DISPLAY, "Tabela de parametro usado em tela, não demonstrada
                p_table TYPE dd02l-tabname  NO-DISPLAY,
                p_selid TYPE rsdynsel-selid NO-DISPLAY.

  SELECTION-SCREEN END OF BLOCK b_02.

  SELECTION-SCREEN BEGIN OF BLOCK b_03 WITH FRAME TITLE TEXT-003.
    PARAMETERS: p_cabtx TYPE xfeld AS CHECKBOX DEFAULT abap_true,
                p_cabtc TYPE xfeld AS CHECKBOX DEFAULT abap_false.
  SELECTION-SCREEN END OF BLOCK b_03.

*************************************************************
* At Selection-Screeen P_VIEW                               *
*************************************************************
AT SELECTION-SCREEN ON p_view.

  IF p_rvis IS NOT INITIAL.
    IF p_table NE p_view.
      IF pt_cond IS NOT INITIAL.

        READ TABLE pt_cond ASSIGNING FIELD-SYMBOL(<tab_gwa_cond>) WITH KEY tablename = p_view.
        IF sy-subrc IS INITIAL.
          gt_cond_tab = pt_cond.
        ELSE.
          CLEAR pt_cond.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR p_selid.
  ENDIF.

*************************************************************
* At Selection-Screeen P_TAB                                *
*************************************************************
AT SELECTION-SCREEN ON p_tab.

  IF p_rtab IS NOT INITIAL.
    IF p_table NE p_tab.
      IF pt_cond IS NOT INITIAL.

        READ TABLE pt_cond ASSIGNING FIELD-SYMBOL(<tab_gwa_cond>) WITH KEY tablename = p_tab.
        IF sy-subrc IS INITIAL.
          gt_cond_tab = pt_cond.
        ELSE.
          CLEAR pt_cond.
        ENDIF.
      ENDIF.
      CLEAR p_selid.
    ENDIF.
  ENDIF.

*************************************************************
*AT SELECTION-SCREEN                                        *
*************************************************************
AT SELECTION-SCREEN.

  DATA: lt_trange TYPE rsds_trange.

  DATA: lt_twhere TYPE rsds_twhere ##NEEDED.

  IF sy-ucomm EQ 'FC01'.
    IF p_view IS NOT INITIAL AND
       p_tab  IS NOT INITIAL.
      MESSAGE 'Favor informar apenas Visão ou Tabela, ambos não são permitidos'(006) TYPE 'S' DISPLAY LIKE 'E'.
    ELSEIF p_view IS INITIAL AND
           p_tab  IS INITIAL.
      MESSAGE 'Favor informar Visão ou Tabela!!!'(007) TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.

      CLEAR: gt_table_tab,
             gt_field_tab.

      IF p_view IS NOT INITIAL.
        p_table = p_view.
      ELSE.
        p_table = p_tab.
      ENDIF.

      "Set Key Filter
      PERFORM f_set_key_filter USING p_table.

      IF p_rtab IS NOT INITIAL.
        IF pt_cond IS NOT INITIAL.

          READ TABLE pt_cond ASSIGNING FIELD-SYMBOL(<tab_gwa_cond>) WITH KEY tablename = p_tab.
          IF sy-subrc IS INITIAL.
            gt_cond_tab = pt_cond.
          ELSE.
            CLEAR: pt_cond,
                   gt_cond_tab.
          ENDIF.
        ELSE.
          CLEAR gt_cond_tab.
        ENDIF.
      ELSE.
        IF pt_cond IS NOT INITIAL.

          READ TABLE pt_cond ASSIGNING <tab_gwa_cond> WITH KEY tablename = p_view.
          IF sy-subrc IS INITIAL.
            gt_cond_tab = pt_cond.
          ELSE.
            CLEAR: pt_cond,
                   gt_cond_tab.
          ENDIF.
        ELSE.
          CLEAR: p_selid,
                 gt_cond_tab.
        ENDIF.
      ENDIF.
      IF pt_cond IS NOT INITIAL.
        lt_twhere =  pt_cond.
      ENDIF.

      CALL FUNCTION 'FREE_SELECTIONS_INIT'
        EXPORTING
          kind             = 'T'
          field_ranges_int = pt_free
        IMPORTING
          selection_id     = gv_selid
          where_clauses    = lt_twhere
        TABLES
          tables_tab       = gt_table_tab
          fields_tab       = gt_field_tab
        EXCEPTIONS
          OTHERS           = 4.
      IF sy-subrc <> 0.
        MESSAGE 'Error in initialization'(008) TYPE 'I' DISPLAY LIKE 'E'.
        LEAVE PROGRAM.
      ENDIF.

      CALL FUNCTION 'FREE_SELECTIONS_DIALOG'
        EXPORTING
          selection_id  = gv_selid
          title         = 'Free Selection'(009)
          as_window     = ' '
        IMPORTING
          where_clauses = gt_cond_tab
          field_ranges  = lt_trange
        TABLES
          fields_tab    = gt_field_tab
        EXCEPTIONS
          OTHERS        = 4.

      IF sy-subrc <> 0.
        MESSAGE 'No free selection created'(010) TYPE 'I'.
        LEAVE PROGRAM.
      ELSE.

        pt_cond = gt_cond_tab.
        pt_free = lt_trange.
        p_selid = gv_selid.

        IF gv_name IS INITIAL.
          IF p_view IS NOT INITIAL.
            gv_name = p_view.
          ELSE.
            gv_name = p_tab.
          ENDIF.
        ENDIF.

        ASSIGN gt_cond_tab[ tablename = gv_name ] TO <gwa_cond>.

      ENDIF.
    ENDIF.

  ELSEIF sy-ucomm EQ 'ONLI'.

    IF <gwa_cond> IS NOT ASSIGNED AND
       pt_cond IS NOT INITIAL.

      ASSIGN pt_cond[ tablename = p_table ] TO <gwa_cond>.
    ENDIF.
  ENDIF.

*======================================================================*
* At Selection Screen
*======================================================================*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_arq.

  IF p_hard IS NOT INITIAL.
    PERFORM f_seleciona_arquivo.
  ELSE.
    PERFORM f_seleciona_arquivo_serv.
  ENDIF.

*************************************************************
* AT SELECTION-SCREEN OUTPUT                                *
*************************************************************
AT SELECTION-SCREEN OUTPUT.

  DATA: lt_twhere TYPE rsds_twhere ##NEEDED.

  PERFORM f_trata_campos_tela.

*************************************************************
*Start-Of-Selection                                         *
*************************************************************
START-OF-SELECTION.

*  DO.
*    IF 1 EQ 2.
*      EXIT.
*    ENDIF.
*  ENDDO.

  CLEAR gv_erro.

  IF p_cabtx IS NOT INITIAL AND
     p_cabtc IS NOT INITIAL.
    MESSAGE 'Favor informar apenas Texto Descritivo ou Texto Técnico, ambos não são permitidos'(011) TYPE 'S' DISPLAY LIKE 'E'.
    gv_erro = abap_true.
  ELSE.

    IF p_view IS INITIAL AND
       p_tab IS INITIAL.

      MESSAGE s208(00)
          DISPLAY LIKE 'E'
          WITH 'Preencher visão ou tabela! ambos não possivel'(012).
      gv_erro = abap_true.
    ELSE.

      IF p_arq IS INITIAL.

        MESSAGE 'Favor informar caminho do arquivo'(013) TYPE 'S' DISPLAY LIKE 'E'.
        gv_erro = abap_true.

      ELSE.

        IF p_view IS NOT INITIAL.

          gv_name = p_view.

          SELECT SINGLE viewname ##NEEDED ##WARN_OK
            FROM dd25l
            INTO @DATA(viewname)
            WHERE viewname EQ @p_view.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Não existe Visão ->'(005) && gv_name TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.

        ELSE.

          gv_name = p_tab.

          SELECT SINGLE tabname ##NEEDED ##WARN_OK
  FROM dd02l
  INTO @DATA(tabname)
  WHERE tabname EQ @p_tab.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE 'Não existe Tabela ->'(004) && gv_name TYPE 'S' DISPLAY LIKE 'E'.
            RETURN.
          ENDIF.
        ENDIF.

        "Cria estrutura dinamica
        go_struct_type ?= cl_abap_typedescr=>describe_by_name( gv_name ).

        gt_field_list = go_struct_type->get_ddic_field_list( ).

        "Cria tabela dinamica
        go_itab_type   = cl_abap_tabledescr=>create( go_struct_type ).

        CREATE DATA go_dref TYPE HANDLE go_itab_type.
        ASSIGN go_dref->* TO <gt_outtablocal>.

        DATA(lt_components) = go_struct_type->get_components( ).

        "Retira Campos da tabela
        LOOP AT lt_components ASSIGNING FIELD-SYMBOL(<component>).

          IF <component>-name EQ 'MANDT'.
            DELETE lt_components INDEX sy-tabix.
          ENDIF.

        ENDLOOP.

        "Cria estrutura com novo layout
        go_struct_type = cl_abap_structdescr=>create( p_components = lt_components
                                                      p_strict     = space ).

        "Cria tabela dinamica
        go_itab_type   = cl_abap_tabledescr=>create( go_struct_type ).

        "Cria estrutura com novo Layout
        CREATE DATA go_dref TYPE HANDLE go_struct_type.
        ASSIGN go_dref->* TO <gwa_outline>.

        "Cria tabela com novo Layout
        CREATE DATA go_dref TYPE HANDLE go_itab_type.
        ASSIGN go_dref->* TO <gt_outtab>.

        IF <gwa_cond> IS ASSIGNED.

          SELECT *
            FROM (gv_name)
            INTO TABLE <gt_outtablocal>
            WHERE (<gwa_cond>-where_tab).
        ELSE.

          SELECT *
            FROM (gv_name)
            INTO TABLE <gt_outtablocal>.
        ENDIF.

        IF sy-subrc IS INITIAL.

          LOOP AT <gt_outtablocal> ASSIGNING FIELD-SYMBOL(<lineoutlocal>).

            LOOP AT lt_components ASSIGNING <component>.

              ASSIGN COMPONENT <component>-name OF STRUCTURE <lineoutlocal> TO FIELD-SYMBOL(<value>).
              IF sy-subrc IS INITIAL.

                ASSIGN COMPONENT <component>-name OF STRUCTURE <gwa_outline> TO FIELD-SYMBOL(<line_value>).
                IF sy-subrc IS INITIAL.

                  <line_value> = <value>.

                ENDIF.
              ENDIF.
            ENDLOOP.

            INSERT <gwa_outline> INTO TABLE <gt_outtab>.
            CLEAR <gwa_outline>.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

*************************************************************
*End-Of-Selection                                         *
*************************************************************
END-OF-SELECTION.

  CHECK gv_erro IS INITIAL.

  PERFORM f_gera_arquivo USING <gt_outtab>.

*&---------------------------------------------------------------------*
*&      Form  f_seleciona_arquivo
*&---------------------------------------------------------------------*
*       Caixa de Dialogo do arquivo
*----------------------------------------------------------------------*
FORM f_seleciona_arquivo .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      mask             = '*.*,*.*.'
      mode             = 'O'
      title            = TEXT-f01           "#EC * 'Selecionar Arquivo'
    IMPORTING
      filename         = p_arq
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.

ENDFORM.                    " f_seleciona_arquivo
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_ARQUIVO_SERV
*&---------------------------------------------------------------------*
*      lista diretorios do servidor
*----------------------------------------------------------------------*
FORM f_seleciona_arquivo_serv . " 07/06/2012 (ujusantos)

  CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
    EXPORTING
      directory        = p_arq
      filemask         = '*.*'    " filemask = '*.DIR'
    IMPORTING
      serverfile       = p_arq
    EXCEPTIONS
      canceled_by_user = 1
      OTHERS           = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty
    NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " F_SELECIONA_ARQUIVO_SERV
*************************************************************
*f_trata_campos_tela                                        *
*************************************************************
FORM f_trata_campos_tela.

  LOOP AT SCREEN.

    IF p_rtab IS NOT INITIAL.

      CLEAR p_view.

      IF screen-name EQ 'P_TAB' OR
         screen-name EQ '%_P_TAB_%_APP_%-TEXT'.

        screen-input = '1'.
        screen-invisible = '0'.

      ELSEIF screen-name EQ 'P_VIEW' OR
             screen-name EQ '%_P_VIEW_%_APP_%-TEXT'.

        screen-input = '0'.
        screen-invisible = '1'.

      ENDIF.

    ELSE.

      CLEAR p_tab.

      IF screen-name EQ 'P_TAB' OR
         screen-name EQ '%_P_TAB_%_APP_%-TEXT'.

        screen-input = '0'.
        screen-invisible = '1'.

      ELSEIF screen-name EQ 'P_VIEW' OR
             screen-name EQ '%_P_VIEW_%_APP_%-TEXT'.

        screen-input = '1'.
        screen-invisible = '0'.

      ENDIF.
    ENDIF.

    MODIFY SCREEN.
  ENDLOOP.

ENDFORM.
*************************************************************
*f_set_key_filter                                           *
*************************************************************
FORM f_set_key_filter USING pv_name TYPE any.

  DATA: lo_struct_type TYPE REF TO cl_abap_structdescr.

  gt_table_tab = VALUE #( ( prim_tab = pv_name ) ).

  lo_struct_type ?= cl_abap_typedescr=>describe_by_name( pv_name ).

  DATA(lt_field_list) = lo_struct_type->get_ddic_field_list( ).

  LOOP AT lt_field_list ASSIGNING FIELD-SYMBOL(<field_list>).

    IF <field_list>-fieldname EQ 'MANDT' OR
       <field_list>-keyflag IS INITIAL.
      CONTINUE.
    ENDIF.

    INSERT VALUE #( tablename = pv_name
                    fieldname = <field_list>-fieldname ) INTO TABLE gt_field_tab.

  ENDLOOP.
ENDFORM.
*************************************************************
*F_GERA_ARQUIVO                                             *
*************************************************************
FORM f_gera_arquivo USING pt_tab TYPE STANDARD TABLE.

  DATA: lt_table    TYPE truxs_t_text_data.

  DATA: lwa_cabec LIKE LINE OF lt_table.

  DATA: lv_filename TYPE string.

  lv_filename = p_arq.

  CALL FUNCTION 'SAP_CONVERT_TO_CSV_FORMAT'
    EXPORTING
      i_field_seperator    = ';'
      i_line_header        = 'X'
      i_filename           = CONV localfile( lv_filename )
    TABLES
      i_tab_sap_data       = pt_tab[]
    CHANGING
      i_tab_converted_data = lt_table[].

  "Verifica se deve incluir cabecalho no arquivo
  IF p_cabtx IS NOT INITIAL OR
     p_cabtc IS NOT INITIAL.

    LOOP AT gt_field_list ASSIGNING FIELD-SYMBOL(<field_list>).

      IF <field_list>-fieldname EQ 'MANDT'.
        CONTINUE.
      ENDIF.
      "Incluir Texto Cabecalho
      IF p_cabtx IS NOT INITIAL.
        IF lwa_cabec IS INITIAL.
          lwa_cabec = lwa_cabec = <field_list>-scrtext_l ##NEEDED.
        ELSE.
          lwa_cabec = lwa_cabec && ';' && <field_list>-scrtext_l.
        ENDIF.
      ELSEIF p_cabtc IS NOT INITIAL.
        IF lwa_cabec IS INITIAL.
          lwa_cabec = lwa_cabec = <field_list>-fieldname ##NEEDED.
        ELSE.
          lwa_cabec = lwa_cabec && ';' && <field_list>-fieldname.
        ENDIF.
      ENDIF.
    ENDLOOP.

    INSERT lwa_cabec INTO lt_table INDEX 1.
  ENDIF.

  lv_filename = lv_filename && '.CSV'.

  IF sy-batch IS INITIAL.

    CALL FUNCTION 'GUI_DOWNLOAD'
      EXPORTING
        filename              = lv_filename
        filetype              = 'ASC'
*       append                = 'X'
        write_field_separator = 'X'
        confirm_overwrite     = 'X'
        codepage              = '4102'
      TABLES
        data_tab              = lt_table[] "need to declare and populate
      EXCEPTIONS
        file_write_error      = 1
        OTHERS                = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty
      NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ELSE.

    OPEN DATASET p_arq FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.

    LOOP AT lt_table ASSIGNING FIELD-SYMBOL(<csv>).
      TRANSFER: <csv>   TO p_arq NO END OF LINE.
      TRANSFER: cl_abap_char_utilities=>newline TO p_arq NO END OF LINE.
    ENDLOOP.

    CLOSE DATASET p_arq.

  ENDIF.

ENDFORM.
