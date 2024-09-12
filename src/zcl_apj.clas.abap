CLASS zcl_apj DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_apj_dt_exec_object.
    INTERFACES if_apj_rt_exec_object.
    INTERFACES if_oo_adt_classrun.

    METHODS api_create_item_text
      IMPORTING
        iv_do   TYPE i_outbounddelivery-outbounddelivery
        iv_item TYPE i_outbounddeliveryitem-outbounddeliveryitem
        iv_text TYPE string.

  PROTECTED SECTION.


  PRIVATE SECTION.
    CONSTANTS : BEGIN OF c_task,
                  create TYPE c VALUE 'C',
                  update TYPE c VALUE 'U',
                  delete TYPE c VALUE 'D',
                END OF c_task.

    METHODS check_do_lock
      IMPORTING i_do TYPE i_outbounddelivery-outbounddelivery
      RAISING   lcx_lock_error.
    METHODS modify_item_text
      IMPORTING
                iv_task TYPE c "Create = 'C' Update = 'U' Delete = 'D'
                iv_do   TYPE i_outbounddelivery-outbounddelivery
                iv_item TYPE i_outbounddeliveryitem-outbounddeliveryitem
                iv_text TYPE string
      RAISING   lcx_handle_error.
ENDCLASS.


CLASS zcl_apj IMPLEMENTATION.

  METHOD if_apj_dt_exec_object~get_parameters.
    " Return the supported selection parameters here
    et_parameter_def = VALUE #(
      ( selname        = 'P_DO'
        kind           = if_apj_dt_exec_object=>parameter
        datatype       = 'C'
        length         = 10
        param_text     = 'Delivery Document'                "#EC NOTEXT
        changeable_ind = abap_true )
    ).
  ENDMETHOD.

  METHOD if_apj_rt_exec_object~execute.
    DATA:
      ls_entity_key    TYPE zcl_outbound_delivery=>tys_a_outb_delivery_item_tex_2,
      ls_business_data TYPE zcl_outbound_delivery=>tys_a_outb_delivery_item_tex_2,
      lo_http_client   TYPE REF TO if_web_http_client,
      lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
      lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy.
    DATA:
      lo_request_read  TYPE REF TO /iwbep/if_cp_request_read,
      lo_response_read TYPE REF TO /iwbep/if_cp_response_read.
    DATA:
      lt_path          TYPE /iwbep/if_cp_runtime_types=>ty_t_property_path.
    DATA:
      lr_cscn          TYPE if_com_scenario_factory=>ty_query-cscn_id_range.
    DATA:
      lt_equipment TYPE RANGE OF i_technicalobject-equipment,
      ls_equipment LIKE LINE OF lt_equipment.
    DATA:
      lv_delivery_doc TYPE i_deliverydocument-deliverydocument,
      lv_serialnumber TYPE string,
      lv_msg          TYPE if_bali_free_text_setter=>ty_text.
    DATA:
      lo_dest_err TYPE REF TO cx_http_dest_provider_error,
      lo_remote   TYPE REF TO /iwbep/cx_cp_remote,
      lo_gateway  TYPE REF TO /iwbep/cx_gateway,
      lo_web_err  TYPE REF TO cx_web_http_client_error,
      lo_runtime  TYPE REF TO cx_bali_runtime.
    DATA:
      lv_dest_err_log TYPE string,
      lv_remote_log   TYPE string,
      lv_gateway_log  TYPE string,
      lv_web_err_log  TYPE string,
      lv_runtime_log  TYPE string,
      lv_retry        TYPE abap_boolean,
      lv_wait_time    TYPE i,
      lv_task         TYPE c.

*-----------*
*"* BEGIN *"*
*-----------*
    TRY.
        " Create a new Application Log
        DATA(l_log) = cl_bali_log=>create( ).

        " Add a header to the log
        l_log->set_header( header = cl_bali_header_setter=>create( object      = 'ZOBJECT_DO'
                                                                   subobject   = 'ZSUBOBJECT_DO'
                                                                   external_id = 'External ID'   ) ). "#EC NOTEXT

        TRY.
            "Add a message as item to the log
            DATA(l_msg_001) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                              id       = 'ZDO'
                                                              number   = '001' ). "Start job
            l_log->add_item( item = l_msg_001 ).
            "Save the log into the database
            cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                       assign_to_current_appl_job = abap_true ).
            COMMIT WORK.

            IF it_parameters IS INITIAL.
              "Add a message as item to the log
              CLEAR lv_msg.
              lv_msg = |it_parameters is initial|.          "#EC NOTEXT
              DATA(l_msg_002) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_error
                                                                  text     = lv_msg ).
              l_log->add_item( item = l_msg_002 ).
              "Save the log into the database
              cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                         assign_to_current_appl_job = abap_true ).
              COMMIT WORK.
            ENDIF.

          CATCH cx_bali_runtime INTO lo_runtime.        "#EC NO_HANDLER
            lv_runtime_log = lo_runtime->get_text( ).
        ENDTRY.

        " Getting the actual parameter values
        READ TABLE it_parameters INTO DATA(ls_parameter) WITH KEY selname = 'P_DO'.
        IF sy-subrc = 0.

          lv_delivery_doc = ls_parameter-low.
          READ ENTITIES OF i_outbounddeliverytp
            ENTITY outbounddelivery BY \_item
            ALL FIELDS WITH VALUE #( ( outbounddelivery = lv_delivery_doc ) )
            RESULT   DATA(lt_do_item)
            REPORTED DATA(ls_msg_err)
            FAILED   DATA(ls_status).

          CHECK lt_do_item[] IS NOT INITIAL.

          DO 2 TIMES.
            CLEAR: lv_retry , lv_web_err_log.
            TRY.
                me->check_do_lock( lv_delivery_doc ).
                CLEAR: lv_web_err_log.
                EXIT.
              CATCH lcx_lock_error INTO DATA(lo_error).
                IF lv_retry = abap_false.
                  WAIT UP TO 20 SECONDS.
                  lv_retry = abap_true.
                  RETRY.
                ELSE.
                  lv_web_err_log = COND #( WHEN lo_error->previous IS BOUND
                                           THEN lo_error->previous->get_text( )
                                           ELSE  lo_error->get_text( ) ).
                ENDIF.
            ENDTRY.
          ENDDO.

          IF lv_web_err_log IS NOT INITIAL.
            "Add a message as item to the log
            CLEAR lv_msg.
            lv_msg = lv_web_err_log.
            DATA(l_msg_003) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_error
                                                                text     = lv_msg ).
            l_log->add_item( item = l_msg_003 ).
            "Save the log into the database
            cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                       assign_to_current_appl_job = abap_true ).
            COMMIT WORK.
            RETURN.
          ENDIF.



*      IF ls_status-outbounddeliveryitem IS INITIAL.
*        MODIFY ENTITIES OF i_outbounddeliverytp
*          ENTITY outbounddeliveryitem
*          UPDATE FIELDS ( yy1_fd_manuserialnum_dli )
*          WITH VALUE #( FOR ls_do_item IN lt_do_item
*                       (   %tky-outbounddelivery     = ls_do_item-%tky-outbounddelivery
*                           %tky-outbounddeliveryitem = ls_do_item-%tky-outbounddeliveryitem
*                           yy1_fd_manuserialnum_dli  = 'SerialJob'
*                           %control-yy1_fd_manuserialnum_dli = '01' )
*                      )
*          REPORTED DATA(ls_msg_upd)
*          FAILED   DATA(ls_status_upd)
*          MAPPED   DATA(ls_mapped_upd).
*
*        IF ls_status_upd-outbounddeliveryitem IS INITIAL.
*          COMMIT ENTITIES.
*        ELSE.
*          RAISE EXCEPTION TYPE cx_apj_rt_content.
*        ENDIF.
*      ELSE.
*        RAISE EXCEPTION TYPE cx_apj_rt_content.
*      ENDIF.


*        " find CA by scenario
*        lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = 'YY1_INT_HTTP' ) ).
*        DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).
*        lo_factory->query_ca(
*          EXPORTING
*            is_query           = VALUE #( cscn_id_range = lr_cscn )
*          IMPORTING
*            et_com_arrangement = DATA(lt_ca) ).
*
*        IF lt_ca IS INITIAL.
*          CONTINUE.
*        ENDIF.
*
*        " take the first one
*        READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
*        DATA(lv_system_id) = lo_ca->get_comm_system_id( ).


          LOOP AT lt_do_item ASSIGNING FIELD-SYMBOL(<ls_item>).
            CLEAR:
              lv_serialnumber.

*-------------*
*"* CONNECT *"*
*-------------*
*            TRY.
*                " Create HTTP client
*                TRY.
*                    DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
*                                             comm_scenario  = 'YY1_INT_HTTP'
*                                             comm_system_id = 'INT_HTTP'
*                                             service_id     = 'YY1_INT_HTTP_REST' ).
*                  CATCH cx_http_dest_provider_error INTO lo_dest_err. "#EC NO_HANDLER
*                    " Handle Exception
*                    lv_dest_err_log = lo_dest_err->get_text( ).
*                ENDTRY.
*                lo_http_client  = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
*                lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
*                  EXPORTING
*                     is_proxy_model_key      = VALUE #( repository_id       = 'DEFAULT'
*                                                        proxy_model_id      = 'ZCL_OUTBOUND_DELIVERY'
*                                                        proxy_model_version = '0001' )
*                    io_http_client           = lo_http_client
*                    iv_relative_service_root = '/sap/opu/odata/SAP/API_OUTBOUND_DELIVERY_SRV;v=2' ).
*
*                ASSERT lo_http_client IS BOUND.

**--------------*
**"* API READ *"*
**--------------*
*                " Set entity key
*                CLEAR ls_entity_key.
*                ls_entity_key = VALUE #(
*                          delivery_document      = <ls_item>-outbounddelivery     "'DeliveryDocument'
*                          delivery_document_item = <ls_item>-outbounddeliveryitem "'DeliveryDocumentItem'
*                          text_element           = 'ZTX2'
*                          language               = 'EN' ).
*
*                " Navigate to the resource
*                lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).
*
*                " Execute the request and retrieve the business data
*                lo_response_read = lo_resource->create_request_for_read( )->execute( ).
*                lo_response_read->get_business_data( IMPORTING es_business_data = ls_business_data ).

*              CATCH /iwbep/cx_cp_remote      INTO lo_remote. "#EC NO_HANDLER
            " Handle remote Exception
            " It contains details about the problems of your http(s) connection
*                lv_remote_log  = lo_remote->get_longtext( ).
*                TRY.
*                    "Add a message as item to the log
*                    CLEAR lv_msg.
*                    lv_msg = lv_remote_log .
*                    DATA(l_msg_003) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_error
*                                                                        text     = lv_msg ).
*                    l_log->add_item( item = l_msg_003 ).
*                    "Save the log into the database
*                    cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                               assign_to_current_appl_job = abap_true ).
*                    COMMIT WORK.
*
*                  CATCH cx_bali_runtime INTO lo_runtime.
*                    lv_runtime_log = lo_runtime->get_text( ).
*                ENDTRY.
*              CATCH /iwbep/cx_gateway        INTO lo_gateway. "#EC NO_HANDLER
            " Handle Exception
*                lv_gateway_log = lo_gateway->get_text( ).
*              CATCH cx_web_http_client_error INTO lo_web_err. "#EC NO_HANDLER
            " Handle Exception
*                RAISE SHORTDUMP lo_web_err.
*            ENDTRY.

*           " Connection
*            TRY.
*                "Add a message as item to the log
*                CLEAR lv_msg.
*                IF lo_web_err IS BOUND.
*                  lv_msg = |API Connection Fail|.           "#EC NOTEXT
*                ELSE.
*                  lv_msg = |API connection was successful|. "#EC NOTEXT
*                ENDIF.
*                DATA(l_msg_004) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_status
*                                                                    text     = lv_msg ).
*                l_log->add_item( item = l_msg_004 ).
*                "Save the log into the database
*                cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                           assign_to_current_appl_job = abap_true ).
*                COMMIT WORK.
*
*              CATCH cx_bali_runtime INTO lo_runtime.    "#EC NO_HANDLER
*                lv_runtime_log = lo_runtime->get_text( ).
*            ENDTRY.

            " Product Plant
            SELECT SINGLE
              product,
              plant,
              serialnumberprofile
              FROM  i_productplantbasic
              WHERE product        = @<ls_item>-material
              AND   plant          = @<ls_item>-plant
              INTO @DATA(ls_snprofile).
            IF ls_snprofile-serialnumberprofile IS INITIAL.

              TRY.
                  "Add a message as item to the log
                  CLEAR lv_msg.
                  lv_msg = |Item | && <ls_item>-outbounddeliveryitem &&
                           |, serial number profile is missing for material | && <ls_item>-material &&
                           | in plant | && <ls_item>-plant. "#EC NOTEXT
                  DATA(l_msg_013) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_status
                                                                      text     = lv_msg ).
                  l_log->add_item( item = l_msg_013 ).
                  "Save the log into the database
                  cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                             assign_to_current_appl_job = abap_true ).
                  COMMIT WORK.

                CATCH cx_bali_runtime INTO lo_runtime.  "#EC NO_HANDLER
                  lv_runtime_log = lo_runtime->get_text( ).
              ENDTRY.

              CONTINUE.

            ELSE.

              " Item texts > Serial Numbers ZTX2
              SELECT *
                FROM  i_outbounddeliveryitemtexttp
                WHERE outbounddelivery     = @<ls_item>-outbounddelivery
                AND   outbounddeliveryitem = @<ls_item>-outbounddeliveryitem
                AND   language             = 'E'
                AND   longtextid           = 'ZTX2'
                INTO TABLE @DATA(lt_itemtext).

              " equipment
              SELECT
                equipment,
                deliverydocument,
                deliverydocumentitem
                FROM i_serialnumberdeliverydocument
                WHERE deliverydocument     = @<ls_item>-outbounddelivery
                AND   deliverydocumentitem = @<ls_item>-outbounddeliveryitem
                INTO TABLE @DATA(lt_equip).

              IF lt_equip[] IS INITIAL.
                TRY.
                    "Add a message as item to the log
                    CLEAR lv_msg.
                    lv_msg = |No serial number was entered for item | && <ls_item>-outbounddeliveryitem. "#EC NOTEXT
                    DATA(l_msg_005) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_status
                                                                        text     = lv_msg ).
                    l_log->add_item( item = l_msg_005 ).
                    "Save the log into the database
                    cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                               assign_to_current_appl_job = abap_true ).
                    COMMIT WORK.

                  CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
                    lv_runtime_log = lo_runtime->get_text( ).
                ENDTRY.
              ELSE.
                CLEAR lt_equipment.
                LOOP AT lt_equip ASSIGNING FIELD-SYMBOL(<ls_equip>).
                  ls_equipment-sign   = 'I'.
                  ls_equipment-option = 'EQ'.
                  ls_equipment-low    = <ls_equip>-equipment.
                  ls_equipment-high   = ' '.
                  APPEND ls_equipment TO lt_equipment.
                  CLEAR ls_equipment.
                ENDLOOP.

                SELECT
                  equipment,
                  manufacturerserialnumber,
                  serialnumber
                  FROM i_technicalobject
                  WHERE equipment IN @lt_equipment
                  INTO TABLE @DATA(lt_technicalobject).

                IF lt_technicalobject[] IS NOT INITIAL.
                  DELETE lt_technicalobject WHERE manufacturerserialnumber IS INITIAL.
                  SORT lt_technicalobject ASCENDING BY manufacturerserialnumber.
                  LOOP AT lt_technicalobject INTO DATA(ls_technicalobject).
                    IF lv_serialnumber IS INITIAL.
                      lv_serialnumber = ls_technicalobject-manufacturerserialnumber.
                    ELSE.
                      lv_serialnumber = lv_serialnumber && |, | && ls_technicalobject-manufacturerserialnumber.
                    ENDIF.
                  ENDLOOP.
                ENDIF. "IF lt_technicalobject[] IS NOT INITIAL.
              ENDIF. "IF lt_equip[] IS INITIAL.

            ENDIF. "IF ls_snprofile-serialnumberprofile IS INITIAL.

            ">>Modify outbound delivery item text
            CLEAR: lv_task , lv_remote_log , lv_retry.

            lv_task = COND #( WHEN lt_itemtext IS INITIAL     AND lv_serialnumber IS NOT INITIAL
                              THEN c_task-create
                              WHEN lt_itemtext IS NOT INITIAL AND lv_serialnumber IS INITIAL
                              THEN c_task-delete
                              WHEN lt_itemtext IS NOT INITIAL AND lv_serialnumber IS NOT INITIAL
                              THEN c_task-update ).

            IF lv_task IS  INITIAL.
              "Do nothing.
              CONTINUE.
            ELSE.
              TRY.
                  me->modify_item_text(
                    iv_task = lv_task
                    iv_do   = <ls_item>-outbounddelivery
                    iv_item = <ls_item>-outbounddeliveryitem
                    iv_text = lv_serialnumber
                  ).
                  CLEAR: lv_remote_log.
                CATCH lcx_handle_error INTO DATA(lx_error).

                  IF lv_retry = abap_false. "try call api again
                    WAIT UP TO 20 SECONDS.
                    lv_retry = abap_true.
                    RETRY.
                  ELSE.
                    lv_remote_log = COND #( WHEN lx_error->previous IS BOUND
                                            THEN lx_error->previous->get_longtext( )
                                            ELSE lx_error->get_longtext(  ) ).
                  ENDIF.

              ENDTRY.
            ENDIF.



            ">>Add Message log
            TRY.
                "Add a message as item to the log
                CLEAR lv_msg.
                IF lv_remote_log IS INITIAL.
                  lv_msg =  SWITCH #( lv_task
                                      WHEN c_task-create
                                      THEN |Serial number for item {   <ls_item>-outbounddeliveryitem } has been created| "#EC NOTEXT
                                      WHEN c_task-delete
                                      THEN |Serial number for item {  <ls_item>-outbounddeliveryitem } has already been deleted|
                                      WHEN c_task-update
                                      THEN |Serial number item {  <ls_item>-outbounddeliveryitem } has already been updated| ).
                ELSE.
                  lv_msg = |Item { <ls_item>-outbounddeliveryitem } Error : { lv_remote_log }|.
                ENDIF.
                DATA(l_msg_007) = cl_bali_free_text_setter=>create( severity = COND #( WHEN lv_remote_log IS INITIAL
                                                                                       THEN if_bali_constants=>c_severity_status
                                                                                       ELSE if_bali_constants=>c_severity_error )
                                                                    text     = lv_msg ).
                l_log->add_item( item = l_msg_007 ).
                "Save the log into the database
                cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                           assign_to_current_appl_job = abap_true ).
                COMMIT WORK.

              CATCH cx_bali_runtime INTO lo_runtime.    "#EC NO_HANDLER
                lv_runtime_log = lo_runtime->get_text( ).
            ENDTRY.

            CLEAR: lt_itemtext , lv_serialnumber.

*            IF lt_itemtext[] IS INITIAL.
*
*              IF lv_serialnumber IS INITIAL.
*                CLEAR: lt_itemtext.
*                CONTINUE.
*              ELSE.
**----------------*
**"* API CREATE *"*
**----------------*
*                DATA:
*                  lo_request_create  TYPE REF TO /iwbep/if_cp_request_create,
*                  lo_response_create TYPE REF TO /iwbep/if_cp_response_create.
*
*                CLEAR: lv_retry ,lv_remote_log.
*
*                TRY.
*                    " Prepare the business data
*                    CLEAR ls_business_data.
*                    ls_business_data = VALUE #(
*                              delivery_document       = <ls_item>-outbounddelivery
*                              delivery_document_item  = <ls_item>-outbounddeliveryitem
*                              text_element            = 'ZTX2'
*                              language                = 'EN'
*                              text_element_text       = lv_serialnumber
*                              ).
*
*                    " Set the business data for the created entity
*                    CLEAR: lt_path.
*                    APPEND 'DELIVERY_DOCUMENT'        TO lt_path.
*                    APPEND 'DELIVERY_DOCUMENT_ITEM'   TO lt_path.
*                    APPEND 'TEXT_ELEMENT'             TO lt_path.
*                    APPEND 'LANGUAGE'                 TO lt_path.
*                    APPEND 'TEXT_ELEMENT_TEXT'        TO lt_path.
*
*                    " Navigate to the resource and create a request for the create operation
*                    lo_request_create = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->create_request_for_create( ).
*
*                    lo_request_create->set_business_data( is_business_data     = ls_business_data
*                                                          it_provided_property = lt_path          ).
*
*                    " Execute the request
*                    lo_response_create = lo_request_create->execute( ).
*
**                    " Get the after image
**                    lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).
*
*                  CATCH /iwbep/cx_cp_remote      INTO lo_remote. "#EC NO_HANDLER
*                    " Handle remote Exception
*                    " It contains details about the problems of your http(s) connection
**                    lv_remote_log   = lo_remote->get_text( ).
**                    TRY.
**                        "Add a message as item to the log
**                        CLEAR lv_msg.
**                        lv_msg = lv_remote_log .
**                        DATA(l_msg_006) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_error
**                                                                            text     = lv_msg ).
**                        l_log->add_item( item = l_msg_006 ).
**                        "Save the log into the database
**                        cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
**                                                                   assign_to_current_appl_job = abap_true ).
**                        COMMIT WORK.
**
**                      CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
**                        lv_runtime_log = lo_runtime->get_text( ).
**                    ENDTRY.
*                    IF lv_retry = abap_false.
*                      WAIT UP TO 10 SECONDS.
*                      lv_retry = abap_true.
*                      RETRY.
*                    ELSE.
*                      lv_remote_log = lo_remote->get_longtext( ).
*                    ENDIF.
*
*                  CATCH /iwbep/cx_gateway        INTO lo_gateway. "#EC NO_HANDLER
*                    " Handle Exception
**                    lv_gateway_log  = lo_gateway->get_text( ).
*                  CATCH cx_web_http_client_error INTO lo_web_err. "#EC NO_HANDLER
*                    " Handle Exception
*                    RAISE SHORTDUMP lo_web_err.
*                ENDTRY.
*
*                " Create Item Text
*                TRY.
*                    "Add a message as item to the log
*                    CLEAR lv_msg.
*                    IF lv_remote_log IS INITIAL.
*                      lv_msg = |Serial number for item | && <ls_item>-outbounddeliveryitem && | has been created|. "#EC NOTEXT
*                    ELSE.
*                      lv_msg = |Item { <ls_item>-outbounddeliveryitem } Error : { lv_remote_log }|.
*                    ENDIF.
*                    DATA(l_msg_007) = cl_bali_free_text_setter=>create( severity = COND #( WHEN lv_remote_log IS INITIAL
*                                                                                           THEN if_bali_constants=>c_severity_status
*                                                                                           ELSE if_bali_constants=>c_severity_error )
*                                                                        text     = lv_msg ).
*                    l_log->add_item( item = l_msg_007 ).
*                    "Save the log into the database
*                    cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                               assign_to_current_appl_job = abap_true ).
*                    COMMIT WORK.
*
*                  CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
*                    lv_runtime_log = lo_runtime->get_text( ).
*                ENDTRY.
*
*              ENDIF. "IF lv_serialnumber IS INITIAL.
*
*            ELSE.
*
*              IF lv_serialnumber IS INITIAL.
**----------------*
**"* API DELETE *"*
**----------------*
*                DATA:
*                    lo_request_delete TYPE REF TO /iwbep/if_cp_request_delete.
*
*                CLEAR: lv_retry , lv_remote_log.
*
*                TRY.
*                    "Set entity key
*                    ls_entity_key = VALUE #(
*                              delivery_document       = <ls_item>-outbounddelivery     "'DeliveryDocument'
*                              delivery_document_item  = <ls_item>-outbounddeliveryitem "'DeliveryDocumentItem'
*                              text_element            = 'ZTX2'
*                              language                = 'EN' ).
*
*                    "Navigate to the resource and create a request for the delete operation
*                    lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).
*                    lo_request_delete = lo_resource->create_request_for_delete( ).
*
*                    " Execute the request
*                    lo_request_delete->execute( ).
*
*                  CATCH /iwbep/cx_cp_remote      INTO lo_remote. "#EC NO_HANDLER
*                    " Handle remote Exception
*                    " It contains details about the problems of your http(s) connection
**                    lv_remote_log  = lo_remote->get_text( ).
**                    TRY.
**                        "Add a message as item to the log
**                        CLEAR lv_msg.
**                        lv_msg = lv_remote_log .
**                        DATA(l_msg_008) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_error
**                                                                            text     = lv_msg ).
**                        l_log->add_item( item = l_msg_008 ).
**                        "Save the log into the database
**                        cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
**                                                                   assign_to_current_appl_job = abap_true ).
**                        COMMIT WORK.
**
**                      CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
**                        lv_runtime_log = lo_runtime->get_text( ).
**                    ENDTRY.
*
*                    IF lv_retry = abap_false.
*                      WAIT UP TO 10 SECONDS.
*                      lv_retry = abap_true.
*                      RETRY.
*                    ELSE.
*                      lv_remote_log = lo_remote->get_longtext( ).
*                    ENDIF.
*
*                  CATCH /iwbep/cx_gateway        INTO lo_gateway. "#EC NO_HANDLER
*                    " Handle Exception
**                    lv_gateway_log = lo_gateway->get_text( ).
*                  CATCH cx_web_http_client_error INTO lo_web_err. "#EC NO_HANDLER
*                    " Handle Exception
*                    RAISE SHORTDUMP lo_web_err.
*                ENDTRY.
*
*                " Delete Item Text
*                TRY.
*                    "Add a message as item to the log
*                    CLEAR lv_msg.
*                    IF lv_remote_log IS INITIAL.
*                      lv_msg = |Serial number for item | && <ls_item>-outbounddeliveryitem && | has already been deleted|. "#EC NOTEXT
*                    ELSE.
*                      lv_msg = |Item { <ls_item>-outbounddeliveryitem } error: { lv_remote_log }|. "#EC NOTEXT
*                    ENDIF.
*                    DATA(l_msg_009) = cl_bali_free_text_setter=>create( severity = COND #( WHEN lv_remote_log IS INITIAL
*                                                                                           THEN if_bali_constants=>c_severity_status
*                                                                                           ELSE if_bali_constants=>c_severity_error )
*                                                                        text     = lv_msg ).
*                    l_log->add_item( item = l_msg_009 ).
*                    "Save the log into the database
*                    cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                               assign_to_current_appl_job = abap_true ).
*                    COMMIT WORK.
*
*                  CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
*                    lv_runtime_log = lo_runtime->get_text( ).
*                ENDTRY.
*
*              ELSE.
**----------------*
**"* API UPDATE *"*
**----------------*
*                DATA:
*                  lo_request_update  TYPE REF TO /iwbep/if_cp_request_update,
*                  lo_response_update TYPE REF TO /iwbep/if_cp_response_update.
*
*                CLEAR: lv_retry , lv_remote_log.
*
*                TRY.
*                    " Set entity key
*                    CLEAR ls_entity_key.
*                    ls_entity_key = VALUE #(
*                              delivery_document       = <ls_item>-outbounddelivery     "'DeliveryDocument'
*                              delivery_document_item  = <ls_item>-outbounddeliveryitem "'DeliveryDocumentItem'
*                              text_element            = 'ZTX2'
*                              language                = 'EN' ).
*
*                    " Prepare the business data
*                    CLEAR ls_business_data.
*                    ls_business_data = VALUE #(
*                              "delivery_document       = <ls_item>-outbounddelivery
*                              "delivery_document_item  = <ls_item>-outbounddeliveryitem
*                              "text_element            = 'ZTX2'
*                              "language                = 'EN'
*                              text_element_text       = lv_serialnumber
*                              ).
*
*                    " Set the business data for the update entity
*                    CLEAR: lt_path.
*                    "APPEND 'DELIVERY_DOCUMENT'        TO lt_path.
*                    "APPEND 'DELIVERY_DOCUMENT_ITEM'   TO lt_path.
*                    "APPEND 'TEXT_ELEMENT'             TO lt_path.
*                    "APPEND 'LANGUAGE'                 TO lt_path.
*                    APPEND 'TEXT_ELEMENT_TEXT'        TO lt_path.
*
*                    " Navigate to the resource and create a request for the update operation
*                    lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).
*                    lo_request_update = lo_resource->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-patch ).
*
*                    lo_request_update->set_business_data( is_business_data     = ls_business_data
*                                                          it_provided_property = lt_path          ).
*
*                    " Execute the request and retrieve the business data
*                    lo_response_update = lo_request_update->execute( ).
*
**                    " Get updated entity
**                    CLEAR ls_business_data.
**                    lo_response->get_business_data( importing es_business_data = ls_business_data ).
*
*                  CATCH /iwbep/cx_cp_remote      INTO lo_remote. "#EC NO_HANDLER
*                    " Handle remote Exception
*                    " It contains details about the problems of your http(s) connection
**                    lv_remote_log  = lo_remote->get_text( ).
**                    TRY.
**                        "Add a message as item to the log
**                        CLEAR lv_msg.
**                        lv_msg = lv_remote_log .
**                        DATA(l_msg_010) = cl_bali_free_text_setter=>create( severity = if_bali_constants=>c_severity_error
**                                                                            text     = lv_msg ).
**                        l_log->add_item( item = l_msg_010 ).
**                        "Save the log into the database
**                        cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
**                                                                   assign_to_current_appl_job = abap_true ).
**                        COMMIT WORK.
**
**                      CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
**                        lv_runtime_log = lo_runtime->get_text( ).
**                    ENDTRY.
*                    IF lv_retry = abap_false.
*                      WAIT UP TO 10 SECONDS.
*                      lv_retry = abap_true.
*                      RETRY.
*                    ELSE.
*                      lv_remote_log = lo_remote->get_longtext(  ).
*                    ENDIF.
*                  CATCH /iwbep/cx_gateway        INTO lo_gateway. "#EC NO_HANDLER
*                    " Handle Exception
**                    lv_gateway_log = lo_gateway->get_text( ).
*                  CATCH cx_web_http_client_error INTO lo_web_err. "#EC NO_HANDLER
*                    " Handle Exception
*                    RAISE SHORTDUMP lo_web_err.
*                ENDTRY.
*
*                " Update Item Text
*                TRY.
*                    "Add a message as item to the log
*                    CLEAR lv_msg.
*                    IF lv_remote_log IS INITIAL.
*                      lv_msg = |Serial number item | && <ls_item>-outbounddeliveryitem && | has already been updated|. "#EC NOTEXT
*                    ELSE.
*                      lv_msg = |Item { <ls_item>-outbounddeliveryitem } error: { lv_remote_log }|. "#EC NOTEXT
*                    ENDIF.
*                    DATA(l_msg_011) = cl_bali_free_text_setter=>create( severity = COND #( WHEN lv_remote_log IS INITIAL
*                                                                                           THEN if_bali_constants=>c_severity_status
*                                                                                           ELSE if_bali_constants=>c_severity_error )
*                                                                        text     = lv_msg ).
*                    l_log->add_item( item = l_msg_011 ).
*                    "Save the log into the database
*                    cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                               assign_to_current_appl_job = abap_true ).
*                    COMMIT WORK.
*
*                  CATCH cx_bali_runtime INTO lo_runtime. "#EC NO_HANDLER
*                    lv_runtime_log = lo_runtime->get_text( ).
*                ENDTRY.
*
*              ENDIF. "IF lv_serialnumber IS INITIAL.
*
*            ENDIF. "IF lt_itemtext[] IS INITIAL.

*            CLEAR: lt_itemtext , lv_serialnumber.

          ENDLOOP. "LOOP AT lt_do_item ASSIGNING FIELD-SYMBOL(<ls_item>).

          "Add a message as item to the log
          DATA(l_msg_012) = cl_bali_message_setter=>create( severity = if_bali_constants=>c_severity_information
                                                            id       = 'ZDO'
                                                            number   = '002' ). "Job finished
          l_log->add_item( item = l_msg_012 ).
          "Save the log into the database
          cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
                                                     assign_to_current_appl_job = abap_true ).
          COMMIT WORK.

        ENDIF. "IF sy-subrc = 0.

*        " CREATE: exception
*        DATA(l_exception_c) = cl_bali_exception_setter=>create( severity  = if_bali_constants=>c_severity_error
*                                                                exception = lx_web_err_c ).
*        l_log->add_item( item = l_exception_c ).
*        "Save the log into the database
*        cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                   assign_to_current_appl_job = abap_true ).
*        COMMIT WORK.
*
*        " UPDATE: exception
*        DATA(l_exception_u) = cl_bali_exception_setter=>create( severity  = if_bali_constants=>c_severity_error
*                                                                exception = lx_web_err_u ).
*        l_log->add_item( item = l_exception_u ).
*        "Save the log into the database
*        cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                   assign_to_current_appl_job = abap_true ).
*        COMMIT WORK.
*
*        " DELETE: exception
*        DATA(l_exception_d) = cl_bali_exception_setter=>create( severity  = if_bali_constants=>c_severity_error
*                                                                exception = lx_web_err_d ).
*        l_log->add_item( item = l_exception_d ).
*        "Save the log into the database
*        cl_bali_log_db=>get_instance( )->save_log( log                        = l_log
*                                                   assign_to_current_appl_job = abap_true ).
*        COMMIT WORK.

      CATCH cx_bali_runtime INTO lo_runtime.            "#EC NO_HANDLER
        lv_runtime_log = lo_runtime->get_text( ).
    ENDTRY.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA(lo_apj) = NEW zcl_apj( ).



*    TRY.
*        lo_apj->api_create_item_text(
*             iv_do   = '0080000365'
*             iv_item = '000010'
*             iv_text = 'Test serial from adt 80000365'      "#EC NOTEXT
*        ).
*
*      CATCH cx_bali_runtime INTO DATA(lx_main1).        "#EC NO_HANDLER
*        DATA(bali_log_main1) = lx_main1->get_text( ).
*    ENDTRY.
*
*    TRY.
*        lo_apj->api_create_item_text(
*             iv_do   = '0080000384'
*             iv_item = '000010'
*             iv_text = 'Test serial from adt 80000384'      "#EC NOTEXT
*        ).
*
*      CATCH cx_bali_runtime INTO DATA(lx_main2).        "#EC NO_HANDLER
*        DATA(bali_log_main2) = lx_main2->get_text( ).
*    ENDTRY.

*    TRY.
*        if_apj_rt_exec_object~execute( it_parameters = et_parameters ).
*        out->write( |Finished| ).
*
*      CATCH cx_root INTO DATA(job_scheduling_exception).
*        out->write( |Exception has occured: { job_scheduling_exception->get_text( ) }| ).
*    ENDTRY.


    DATA(lt_parameter) = VALUE if_apj_dt_exec_object=>tt_templ_val(  ).
    TRY.

        APPEND INITIAL LINE TO lt_parameter ASSIGNING FIELD-SYMBOL(<ls_parameter>).
        <ls_parameter>-selname = 'P_DO'.
        <ls_parameter>-sign = 'I'.
        <ls_parameter>-option = 'EQ'.
        <ls_parameter>-low = 'Do'.

        lo_apj->if_apj_rt_exec_object~execute( it_parameters = lt_parameter  ).
      CATCH cx_apj_rt_content INTO DATA(lo_error).

        DATA(lv_err) = lo_error->get_longtext(  ).

    ENDTRY.


  ENDMETHOD.

  METHOD api_create_item_text.

    DATA: lv_delivery_doc TYPE i_deliverydocument-deliverydocument.

    lv_delivery_doc = |{ iv_do ALPHA = IN }|.

    IF lv_delivery_doc IS NOT INITIAL.

      READ ENTITIES OF i_outbounddeliverytp
        ENTITY outbounddelivery BY \_item
        ALL FIELDS WITH VALUE #( ( outbounddelivery = lv_delivery_doc ) )
        RESULT   DATA(lt_do_item)
        REPORTED DATA(ls_msg_err)
        FAILED   DATA(ls_status).

*      IF ls_status-outbounddeliveryitem IS INITIAL.
*        MODIFY ENTITIES OF i_outbounddeliverytp
*          ENTITY outbounddeliveryitem
*          UPDATE FIELDS ( yy1_fd_manuserialnum_dli )
*          WITH VALUE #( FOR ls_do_item IN lt_do_item
*                       (   %tky-outbounddelivery             = ls_do_item-%tky-outbounddelivery
*                           %tky-outbounddeliveryitem         = ls_do_item-%tky-outbounddeliveryitem
*                           yy1_fd_manuserialnum_dli          = 'SerialJob'
*                           %control-yy1_fd_manuserialnum_dli = '01' )
*                      )
*          REPORTED DATA(ls_msg_upd)
*          FAILED   DATA(ls_status_upd)
*          MAPPED   DATA(ls_mapped_upd).
*
*        IF ls_status_upd-outbounddeliveryitem IS INITIAL.
*          COMMIT ENTITIES.
**        ELSE.
**          RAISE EXCEPTION TYPE cx_apj_rt_content.
*        ENDIF.
**      ELSE.
**        RAISE EXCEPTION TYPE cx_apj_rt_content.
*      ENDIF.

      DATA:
        lt_equip TYPE RANGE OF i_technicalobject-equipment,
        ls_equip LIKE LINE OF lt_equip.
      DATA:
        lv_string TYPE string.

      " begin
      DELETE lt_do_item WHERE outbounddeliveryitem <> iv_item.
      IF lt_do_item[] IS NOT INITIAL.

        LOOP AT lt_do_item ASSIGNING FIELD-SYMBOL(<ls_item>).
          CLEAR lv_string.

          " Product Plant
          SELECT SINGLE
            product,
            plant,
            serialnumberprofile
            FROM  i_productplantbasic
            WHERE product        = @<ls_item>-material
            AND   plant          = @<ls_item>-plant
            INTO @DATA(ls_snprofile).

          SELECT
            equipment,
            deliverydocument,
            deliverydocumentitem
            FROM i_serialnumberdeliverydocument
            WHERE deliverydocument     = @<ls_item>-outbounddelivery
            AND   deliverydocumentitem = @<ls_item>-outbounddeliveryitem
            INTO TABLE @DATA(lt_serialnumber).

          IF lt_serialnumber[] IS INITIAL.
            CONTINUE.
          ENDIF.

          CLEAR lt_equip.
          LOOP AT lt_serialnumber ASSIGNING FIELD-SYMBOL(<ls_serialnumber>).
            ls_equip-sign   = 'I'.
            ls_equip-option = 'EQ'.
            ls_equip-low    = <ls_serialnumber>-equipment.
            ls_equip-high   = ' '.
            APPEND ls_equip TO lt_equip.
            CLEAR ls_equip.
          ENDLOOP.

          SELECT
            equipment,
            manufacturerserialnumber,
            serialnumber
            FROM i_technicalobject
            WHERE equipment IN @lt_equip
            INTO TABLE @DATA(lt_technicalobject).

          IF lt_technicalobject[] IS NOT INITIAL.
            DELETE lt_technicalobject WHERE manufacturerserialnumber IS INITIAL.
            SORT lt_technicalobject ASCENDING BY manufacturerserialnumber.
            LOOP AT lt_technicalobject INTO DATA(ls_technicalobject).
              IF lv_string IS INITIAL.
                lv_string = ls_technicalobject-manufacturerserialnumber.
              ELSE.
                lv_string = lv_string && |, | && ls_technicalobject-manufacturerserialnumber.
              ENDIF.
            ENDLOOP.
          ENDIF.

          IF lv_string IS INITIAL.
            CONTINUE.
          ENDIF.

          DATA:
            ls_entity_key    TYPE zcl_outbound_delivery=>tys_a_outb_delivery_item_tex_2,
            ls_business_data TYPE zcl_outbound_delivery=>tys_a_outb_delivery_item_tex_2,
            lo_http_client   TYPE REF TO if_web_http_client,
            lo_resource      TYPE REF TO /iwbep/if_cp_resource_entity,
            lo_client_proxy  TYPE REF TO /iwbep/if_cp_client_proxy.
          DATA:
            lo_request_read  TYPE REF TO /iwbep/if_cp_request_read,
            lo_response_read TYPE REF TO /iwbep/if_cp_response_read.

          DATA: lt_path TYPE /iwbep/if_cp_runtime_types=>ty_t_property_path.

          DATA: lr_cscn TYPE if_com_scenario_factory=>ty_query-cscn_id_range.

          " find CA by scenario
          lr_cscn = VALUE #( ( sign = 'I' option = 'EQ' low = 'YY1_INT_HTTP' ) ).
          DATA(lo_factory) = cl_com_arrangement_factory=>create_instance( ).
          lo_factory->query_ca(
            EXPORTING
              is_query           = VALUE #( cscn_id_range = lr_cscn )
            IMPORTING
              et_com_arrangement = DATA(lt_ca) ).

          IF lt_ca IS INITIAL.
            CONTINUE.
          ENDIF.

          " take the first one
          READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
          DATA(lv_system_id) = lo_ca->get_comm_system_id( ).

          TRY.
              " Create http client
              TRY.
                  DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                           comm_scenario  = 'YY1_INT_HTTP'
                                           comm_system_id = 'INT_HTTP'
                                           service_id     = 'YY1_INT_HTTP_REST' ).
                CATCH cx_http_dest_provider_error INTO DATA(lx_http_err). "#EC NO_HANDLER
                  " Handle Exception
                  DATA(bali_log_http_err) = lx_http_err->get_text( ).
              ENDTRY.
              lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
              lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
                EXPORTING
                   is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                       proxy_model_id      = 'ZCL_OUTBOUND_DELIVERY'
                                                       proxy_model_version = '0001' )
                  io_http_client            = lo_http_client
                  iv_relative_service_root  = '/sap/opu/odata/sap/API_OUTBOUND_DELIVERY_SRV;v=2' ).

              ASSERT lo_http_client IS BOUND.

              " Set entity key
              CLEAR ls_entity_key.
              ls_entity_key = VALUE #(
                        delivery_document       = <ls_item>-outbounddelivery
                        delivery_document_item  = <ls_item>-outbounddeliveryitem
                        text_element            = 'ZTX2'
                        language                = 'EN' ).

              " Navigate to the resource
              lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).

              " Execute the request and retrieve the business data
              lo_response_read = lo_resource->create_request_for_read( )->execute( ).
              lo_response_read->get_business_data( IMPORTING es_business_data = ls_business_data ).

            CATCH /iwbep/cx_cp_remote      INTO DATA(lx_remote). "#EC NO_HANDLER
              " Handle remote Exception
              " It contains details about the problems of your http(s) connection
              DATA(bali_log_xremote) = lx_remote->get_text( ).
            CATCH /iwbep/cx_gateway        INTO DATA(lx_gateway). "#EC NO_HANDLER
              " Handle Exception
              DATA(bali_log_gateway) = lx_remote->get_text( ).
            CATCH cx_web_http_client_error INTO DATA(lx_web_err_r). "#EC NO_HANDLER
              " Handle Exception
              RAISE SHORTDUMP lx_web_err_r.
          ENDTRY.

          SELECT *
            FROM i_outbounddeliveryitemtexttp
            WHERE outbounddelivery     = @<ls_item>-outbounddelivery
            AND   outbounddeliveryitem = @<ls_item>-outbounddeliveryitem
            AND   language             = 'E'
            AND   longtextid           = 'ZTX2'
            INTO TABLE @DATA(lt_itemtext).

          "IF ls_business_data IS INITIAL. "create
          IF lt_itemtext[] IS INITIAL.

            DATA:
              lo_request_create  TYPE REF TO /iwbep/if_cp_request_create,
              lo_response_create TYPE REF TO /iwbep/if_cp_response_create.

            TRY.

                " Prepare the business data
                CLEAR ls_business_data.
                ls_business_data = VALUE #(
                          delivery_document       = <ls_item>-outbounddelivery
                          delivery_document_item  = <ls_item>-outbounddeliveryitem
                          text_element            = 'ZTX2'
                          language                = 'EN'
                          text_element_text       = lv_string
                          ).

                " Navigate to the resource and create a request for the create operation
                lo_request_create = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->create_request_for_create( ).

                " Set the business data for the created entity
                CLEAR: lt_path.
                APPEND 'DELIVERY_DOCUMENT'      TO lt_path.
                APPEND 'DELIVERY_DOCUMENT_ITEM' TO lt_path.
                APPEND 'TEXT_ELEMENT'           TO lt_path.
                APPEND 'LANGUAGE'               TO lt_path.
                APPEND 'TEXT_ELEMENT_TEXT'      TO lt_path.

                lo_request_create->set_business_data( is_business_data     = ls_business_data
                                                      it_provided_property = lt_path          ).

                " Execute the request
                lo_response_create = lo_request_create->execute( ).

*                " Get the after image
*                lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).

              CATCH /iwbep/cx_cp_remote      INTO DATA(lx_remote_create). "#EC NO_HANDLER
                " Handle remote Exception
                " It contains details about the problems of your http(s) connection
                DATA(bali_log_remote_create) = lx_remote_create->get_text( ).
              CATCH /iwbep/cx_gateway        INTO DATA(lx_gateway_create). "#EC NO_HANDLER
                " Handle Exception
                DATA(bali_log_gateway_create) = lx_gateway_create->get_text( ).
              CATCH cx_web_http_client_error INTO DATA(lx_web_error_create). "#EC NO_HANDLER
                " Handle Exception
                RAISE SHORTDUMP lx_web_error_create.
            ENDTRY.

          ELSE.

            DATA:
              lo_request_update  TYPE REF TO /iwbep/if_cp_request_update,
              lo_response_update TYPE REF TO /iwbep/if_cp_response_update.

            TRY.
                " Set entity key
                CLEAR ls_entity_key.
                ls_entity_key = VALUE #(
                          delivery_document       = <ls_item>-outbounddelivery
                          delivery_document_item  = <ls_item>-outbounddeliveryitem
                          text_element            = 'ZTX2'
                          language                = 'EN' ).

                " Prepare the business data
                CLEAR ls_business_data.
                ls_business_data = VALUE #(
                          delivery_document       = <ls_item>-outbounddelivery
                          delivery_document_item  = <ls_item>-outbounddeliveryitem
                          text_element            = 'ZTX2'
                          language                = 'EN'
                          text_element_text       = lv_string
                          ).

                " Navigate to the resource and create a request for the update operation
                lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).

                lo_request_update = lo_resource->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-patch ). "put

                " Set the business data for the update entity
                CLEAR: lt_path.
                APPEND 'DELIVERY_DOCUMENT'      TO lt_path.
                APPEND 'DELIVERY_DOCUMENT_ITEM' TO lt_path.
                APPEND 'TEXT_ELEMENT'           TO lt_path.
                APPEND 'LANGUAGE'               TO lt_path.
                APPEND 'TEXT_ELEMENT_TEXT'      TO lt_path.

                lo_request_update->set_business_data( is_business_data     = ls_business_data
                                                      it_provided_property = lt_path          ).

                " Execute the request and retrieve the business data
                lo_response_update = lo_request_update->execute( ).

*                " Get updated entity
*                CLEAR ls_business_data.
*                lo_response->get_business_data( importing es_business_data = ls_business_data ).

              CATCH /iwbep/cx_cp_remote      INTO DATA(lx_remote_update). "#EC NO_HANDLER
                " Handle remote Exception
                " It contains details about the problems of your http(s) connection
                DATA(bali_log_remote_update) = lx_remote_update->get_text( ).
              CATCH /iwbep/cx_gateway        INTO DATA(lx_gateway_update). "#EC NO_HANDLER
                " Handle Exception
                DATA(bali_log_gateway_update) = lx_gateway_update->get_text( ).
              CATCH cx_web_http_client_error INTO DATA(lx_web_error_update). "#EC NO_HANDLER
                " Handle Exception
                RAISE SHORTDUMP lx_web_error_update.
            ENDTRY.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD check_do_lock.
*
    SELECT SINGLE FROM i_outbounddeliveryitem
     FIELDS outbounddelivery,
            outbounddeliveryitem,
            deliverydocumentitemtext,
            materialbycustomer,
            actualdeliveredqtyinbaseunit
     WHERE outbounddelivery = @i_do
     INTO @DATA(ls_do).
    IF ls_do IS NOT INITIAL.
      ls_do-actualdeliveredqtyinbaseunit += 1.
      MODIFY ENTITIES OF i_outbounddeliverytp
        ENTITY outbounddeliveryitem
         UPDATE
           FIELDS ( actualdeliveredqtyinorderunit )
           WITH VALUE #( ( actualdeliveredqtyinorderunit = ls_do-actualdeliveredqtyinbaseunit

                           %tky-outbounddelivery         = ls_do-outbounddelivery
                           %tky-outbounddeliveryitem     = ls_do-outbounddeliveryitem ) )
       FAILED   DATA(ls_failed_upd)
       REPORTED DATA(ls_reported_upd).
      IF ls_failed_upd-outbounddeliveryitem IS INITIAL.
        ROLLBACK ENTITIES.
      ELSE.
        ROLLBACK ENTITIES.
*        READ TABLE ls_failed_upd-outbounddeliveryitem INTO DATA(ls_item) INDEX 1.
        READ TABLE ls_reported_upd-outbounddeliveryitem INTO DATA(ls_item) INDEX 1.
        IF sy-subrc = 0.
          "DATA(lv_msg) = ls_item-%msg->if_message~get_text( ).

          RAISE EXCEPTION TYPE lcx_lock_error
            EXPORTING
              previous = CAST #( ls_item-%msg ).
        ELSE.
          RAISE EXCEPTION TYPE lcx_lock_error
           MESSAGE ID 'ZDO' TYPE 'E' NUMBER '003' WITH 'Can not update outbound delivery'.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD modify_item_text.

    DATA:
      lt_field_name          TYPE /iwbep/if_cp_runtime_types=>ty_t_property_path.
    DATA:
      ls_entity_key      TYPE zcl_outbound_delivery=>tys_a_outb_delivery_item_tex_2,
      ls_business_data   TYPE zcl_outbound_delivery=>tys_a_outb_delivery_item_tex_2,
      lo_http_client     TYPE REF TO if_web_http_client,
      lo_resource        TYPE REF TO /iwbep/if_cp_resource_entity,
      lo_client_proxy    TYPE REF TO /iwbep/if_cp_client_proxy,
      lo_request_create  TYPE REF TO /iwbep/if_cp_request_create,
      lo_response_create TYPE REF TO /iwbep/if_cp_response_create,
      lo_request_delete  TYPE REF TO /iwbep/if_cp_request_delete,
      lo_request_update  TYPE REF TO /iwbep/if_cp_request_update,
      lo_response_update TYPE REF TO /iwbep/if_cp_response_update.

*-------------*
*"* CONNECT *"*
*-------------*
    TRY.
        " Create HTTP client
        TRY.
            DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                     comm_scenario  = 'YY1_INT_HTTP'
                                     comm_system_id = 'INT_HTTP'
                                     service_id     = 'YY1_INT_HTTP_REST' ).
          CATCH cx_http_dest_provider_error INTO DATA(lo_dest_err). "#EC NO_HANDLER
            " Handle Exception
            RAISE EXCEPTION TYPE lcx_handle_error
              EXPORTING
                previous = lo_dest_err.
        ENDTRY.
        lo_http_client  = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
        lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
          EXPORTING
             is_proxy_model_key      = VALUE #( repository_id       = 'DEFAULT'
                                                proxy_model_id      = 'ZCL_OUTBOUND_DELIVERY'
                                                proxy_model_version = '0001' )
            io_http_client           = lo_http_client
            iv_relative_service_root = '/sap/opu/odata/SAP/API_OUTBOUND_DELIVERY_SRV;v=2' ).

        ASSERT lo_http_client IS BOUND.

        CLEAR: lt_field_name , ls_business_data , ls_entity_key.
        "prepare data.
        ls_entity_key = VALUE #(
                  delivery_document       = iv_do   "'DeliveryDocument'
                  delivery_document_item  = iv_item "'DeliveryDocumentItem'
                  text_element            = 'ZTX2'
                  language                = 'EN' ).

        ls_business_data = VALUE #(
                  delivery_document       = iv_do
                  delivery_document_item  = iv_item
                  text_element            = 'ZTX2'
                  language                = 'EN'
                  text_element_text       = iv_text
                  ).

        CASE iv_task.
          WHEN c_task-create. "Create Item Text

            " Set the business data for the created entity
            CLEAR: lt_field_name.
            APPEND 'DELIVERY_DOCUMENT'        TO lt_field_name.
            APPEND 'DELIVERY_DOCUMENT_ITEM'   TO lt_field_name.
            APPEND 'TEXT_ELEMENT'             TO lt_field_name.
            APPEND 'LANGUAGE'                 TO lt_field_name.
            APPEND 'TEXT_ELEMENT_TEXT'        TO lt_field_name.

            " Navigate to the resource and create a request for the create operation
            lo_request_create = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->create_request_for_create( ).

            lo_request_create->set_business_data( is_business_data     = ls_business_data
                                                  it_provided_property = lt_field_name  ).

            " Execute the request
            lo_response_create = lo_request_create->execute( ).

          WHEN c_task-update."Update Item text

            " Set the business data for the created entity
            CLEAR: lt_field_name ,
                   ls_business_data-delivery_document ,
                   ls_business_data-delivery_document_item,
                   ls_business_data-text_element,
                   ls_business_data-language.
            APPEND 'TEXT_ELEMENT_TEXT'        TO lt_field_name.

            " Navigate to the resource and create a request for the update operation
            lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).
            lo_request_update = lo_resource->create_request_for_update( /iwbep/if_cp_request_update=>gcs_update_semantic-patch ).

            lo_request_update->set_business_data( is_business_data     = ls_business_data
                                                  it_provided_property = lt_field_name          ).

            " Execute the request and retrieve the business data
            lo_response_update = lo_request_update->execute( ).

          WHEN c_task-delete."Delete Item Text

            "Navigate to the resource and create a request for the delete operation
            lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).
            lo_request_delete = lo_resource->create_request_for_delete( ).

            " Execute the request
            lo_request_delete->execute( ).

          WHEN OTHERS.
            RAISE EXCEPTION TYPE lcx_handle_error
             MESSAGE ID 'ZDO'  TYPE 'E' NUMBER '003'  WITH 'Task is incorrect'.
        ENDCASE.
        TRY.
            lo_http_client->close( ).
          CATCH cx_web_http_client_error INTO DATA(lx_http_error). "#EC NO_HANDLER
        ENDTRY.

      CATCH /iwbep/cx_cp_remote      INTO DATA(lx_remote_update). "#EC NO_HANDLER
        " Handle remote Exception
        " It contains details about the problems of your http(s) connection
        TRY.
            lo_http_client->close( ).
          CATCH cx_web_http_client_error INTO lx_http_error. "#EC NO_HANDLER
        ENDTRY.
        RAISE EXCEPTION TYPE lcx_handle_error
          EXPORTING
            previous = lx_remote_update.
      CATCH /iwbep/cx_gateway        INTO DATA(lx_gateway_update). "#EC NO_HANDLER
        " Handle Exception
        TRY.
            lo_http_client->close( ).
          CATCH cx_web_http_client_error INTO lx_http_error. "#EC NO_HANDLER
        ENDTRY.
        RAISE EXCEPTION TYPE lcx_handle_error
          EXPORTING
            previous = lx_gateway_update.
      CATCH cx_web_http_client_error INTO DATA(lx_web_error_update). "#EC NO_HANDLER
        " Handle Exception
        TRY.
            lo_http_client->close( ).
          CATCH cx_web_http_client_error INTO lx_http_error. "#EC NO_HANDLER
        ENDTRY.
        RAISE EXCEPTION TYPE lcx_handle_error
          EXPORTING
            previous = lx_web_error_update.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
