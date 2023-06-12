*&---------------------------------------------------------------------*
*& Report ZRACRLERATOR_ALV_TEMPLATE
*&---------------------------------------------------------------------*
*& Template de ALV - Projeto aceleradores
*& Autor: Ana Chaves
*&---------------------------------------------------------------------*


REPORT zracrlerator_alv_template.

DATA: lt_data TYPE TABLE OF spfli,
      ls_data TYPE spfli.

*vars de referencia
DATA: lo_alv       TYPE REF TO cl_salv_table,
      lo_functions TYPE REF TO cl_salv_functions_list.


START-OF-SELECTION.

  SELECT * FROM spfli INTO TABLE lt_data.

*Usando o método estático factory da classe cl_salv_table para criar um objeto ALV.
*Passamos a tabela interna lt_data como parâmetro no campo t_table e recebemos o objeto ALV retornado no campo r_salv_table da variável lo_alv.

  TRY .
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table      = lt_data
      ).

*  Obtendo o objeto lo_functions a partir do objeto ALV lo_alv usando o método get_functions.
*  Em seguida, chamamos o método set_all no objeto lo_functions para definir todas as opções de função do ALV como verdadeiras.

      lo_functions = lo_alv->get_functions( ).
      lo_functions->set_all( abap_true ).

* Chamamos o método display no objeto lo_alv para exibir o ALV na tela.

      lo_alv->display( ).

    CATCH cx_salv_msg.
      MESSAGE 'An error occurred during ALV display.' TYPE 'E'.
  ENDTRY.
