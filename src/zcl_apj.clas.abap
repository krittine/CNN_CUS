CLASS zcl_apj DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_apj_dt_exec_object.
    INTERFACES if_apj_rt_exec_object.

    INTERFACES if_oo_adt_classrun.
    METHODS api_create_item_text IMPORTING iv_do   TYPE i_outbounddelivery-outbounddelivery
                                           iv_item TYPE i_outbounddeliveryitem-outbounddeliveryitem
                                           iv_text TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APJ IMPLEMENTATION.


  METHOD if_apj_dt_exec_object~get_parameters.
    et_parameter_def = VALUE #(
      ( selname        = 'P_DO'
        kind           = if_apj_dt_exec_object=>parameter
        datatype       = 'C'
        length         = 10
        param_text     = 'Delivery Doucment'
        changeable_ind = abap_true )
    ).
  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.
    DATA: lv_delivery_doc TYPE i_deliverydocument-deliverydocument.

    READ TABLE it_parameters INTO DATA(ls_parameter) WITH KEY selname = 'P_DO'.

    IF sy-subrc = 0.
      lv_delivery_doc = ls_parameter-low.
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

      IF lt_do_item[] IS NOT INITIAL.
        DATA: lv_string TYPE string.

        LOOP AT lt_do_item ASSIGNING FIELD-SYMBOL(<ls_item>).

          DATA:
            ls_entity_key    TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
            ls_business_data TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
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
            EXIT.
          ENDIF.

          " take the first one
          READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
          DATA(lv_system_id) = lo_ca->get_comm_system_id( ).

          TRY.
              " Create http client
              DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                       comm_scenario  = 'YY1_INT_HTTP'
                                       service_id     = 'YY1_INT_HTTP_REST'
                                       comm_system_id = lv_system_id ).
              lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
              lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
                EXPORTING
                   is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                       proxy_model_id      = 'YCL_SCM_OUTBOUNDDELIVERY'
                                                       proxy_model_version = '0001' )
                  io_http_client             = lo_http_client
                  iv_relative_service_root   = '/sap/opu/odata/sap/API_OUTBOUND_DELIVERY_SRV;v=0002/' ).

              ASSERT lo_http_client IS BOUND.

              " Set entity key
              CLEAR ls_entity_key.
              ls_entity_key = VALUE #(
                        delivery_document       = <ls_item>-outbounddelivery
                        delivery_document_item  = <ls_item>-outbounddeliveryitem
                        text_element            = 'Z001'
                        language                = 'EN' ).

              " Navigate to the resource
              lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).

              " Execute the request and retrieve the business data
              lo_response_read = lo_resource->create_request_for_read( )->execute( ).
              lo_response_read->get_business_data( IMPORTING es_business_data = ls_business_data ).

            CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
              " Handle remote Exception
              " It contains details about the problems of your http(s) connection
            CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
              " Handle Exception
            CATCH cx_http_dest_provider_error INTO DATA(lx_http_error_read).
              " Handle Exception
            CATCH cx_web_http_client_error INTO DATA(lx_web_error_read).
              " Handle Exception
              RAISE SHORTDUMP lx_web_error_read.
          ENDTRY.

          SELECT
            equipment,
            deliverydocument,
            deliverydocumentitem
            FROM i_serialnumberdeliverydocument
            WHERE deliverydocument     = @<ls_item>-outbounddelivery
            AND   deliverydocumentitem = @<ls_item>-outbounddeliveryitem
            INTO TABLE @DATA(lt_serialnumber) PRIVILEGED ACCESS.

          DATA: lt_equip TYPE RANGE OF i_technicalobject-equipment,
                ls_equip LIKE LINE OF lt_equip.

          IF lt_serialnumber[] IS NOT INITIAL.
            CLEAR lv_string.

            CLEAR lt_equip.
            LOOP AT lt_serialnumber ASSIGNING FIELD-SYMBOL(<ls_serialnumber>).
              ls_equip-sign   = 'I'.
              ls_equip-option = 'EQ'.
              ls_equip-low    = <ls_serialnumber>-equipment.
              ls_equip-high   = ' '.
              APPEND ls_equip TO lt_equip.
            ENDLOOP.

            SELECT
              equipment,
              manufacturerserialnumber,
              serialnumber
              FROM i_technicalobject
              WHERE equipment IN @lt_equip
              INTO TABLE @DATA(lt_technicalobject) PRIVILEGED ACCESS.

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

            IF ls_business_data IS INITIAL.

              DATA:
                lo_request_create  TYPE REF TO /iwbep/if_cp_request_create,
                lo_response_create TYPE REF TO /iwbep/if_cp_response_create.

              TRY.

                  " Prepare the business data
                  CLEAR ls_business_data.
                  ls_business_data = VALUE #(
                            delivery_document           = <ls_item>-outbounddelivery
                            delivery_document_item      = <ls_item>-outbounddeliveryitem
                            text_element                = 'Z001'
                            language                    = 'EN'
                            "text_element_description    = 'Serial number'
                            text_element_text           = lv_string
                            "delivery_long_text_is_form  = abap_true
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

*                  " Get the after image
*                  lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).

                CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote_create).
                  " Handle remote Exception
                  " It contains details about the problems of your http(s) connection
                CATCH /iwbep/cx_gateway INTO DATA(lx_gateway_create).
                  " Handle Exception
                CATCH cx_http_dest_provider_error INTO DATA(lx_http_error_create).
                  " Handle Exception
                CATCH cx_web_http_client_error INTO DATA(lx_web_error_create).
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
                            text_element            = 'Z001'
                            language                = 'EN' ).

                  " Prepare the business data
                  CLEAR ls_business_data.
                  ls_business_data = VALUE #(
                            delivery_document           = <ls_item>-outbounddelivery
                            delivery_document_item      = <ls_item>-outbounddeliveryitem
                            text_element                = 'Z001'
                            language                    = 'EN'
*                            text_element_description    = 'Serial number'
                            text_element_text           = lv_string
*                            delivery_long_text_is_form  = abap_true
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

*                 " Get updated entity
*                  CLEAR ls_business_data.
*                  lo_response->get_business_data( importing es_business_data = ls_business_data ).

                CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote_update).
                  " Handle remote Exception
                  " It contains details about the problems of your http(s) connection
                CATCH /iwbep/cx_gateway INTO DATA(lx_gateway_update).
                  " Handle Exception
                CATCH cx_http_dest_provider_error INTO DATA(lx_http_error_update).
                  " Handle Exception
                CATCH cx_web_http_client_error INTO DATA(lx_web_error_update).
                  " Handle Exception
                  RAISE SHORTDUMP lx_web_error_update.
              ENDTRY.

            ENDIF.

            CLEAR ls_business_data.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

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

      IF lt_do_item[] IS NOT INITIAL.
        DATA: lv_string TYPE string.

        LOOP AT lt_do_item ASSIGNING FIELD-SYMBOL(<ls_item>).

          DATA:
            ls_entity_key    TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
            ls_business_data TYPE ycl_scm_outbounddelivery=>tys_a_outb_delivery_item_tex_2,
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
            EXIT.
          ENDIF.

          " take the first one
          READ TABLE lt_ca INTO DATA(lo_ca) INDEX 1.
          DATA(lv_system_id) = lo_ca->get_comm_system_id( ).

          TRY.
              " Create http client
              DATA(lo_destination) = cl_http_destination_provider=>create_by_comm_arrangement(
                                       comm_scenario  = 'YY1_INT_HTTP'
                                       service_id     = 'YY1_INT_HTTP_REST'
                                       comm_system_id = lv_system_id ).
              lo_http_client = cl_web_http_client_manager=>create_by_http_destination( lo_destination ).
              lo_client_proxy = /iwbep/cl_cp_factory_remote=>create_v2_remote_proxy(
                EXPORTING
                   is_proxy_model_key       = VALUE #( repository_id       = 'DEFAULT'
                                                       proxy_model_id      = 'YCL_SCM_OUTBOUNDDELIVERY'
                                                       proxy_model_version = '0001' )
                  io_http_client             = lo_http_client
                  iv_relative_service_root   = '/sap/opu/odata/sap/API_OUTBOUND_DELIVERY_SRV;v=0002/' ).

              ASSERT lo_http_client IS BOUND.

              " Set entity key
              CLEAR ls_entity_key.
              ls_entity_key = VALUE #(
                        delivery_document       = <ls_item>-outbounddelivery
                        delivery_document_item  = <ls_item>-outbounddeliveryitem
                        text_element            = 'Z001'
                        language                = 'EN' ).

              " Navigate to the resource
              lo_resource = lo_client_proxy->create_resource_for_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' )->navigate_with_key( ls_entity_key ).

              " Execute the request and retrieve the business data
              lo_response_read = lo_resource->create_request_for_read( )->execute( ).
              lo_response_read->get_business_data( IMPORTING es_business_data = ls_business_data ).

            CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote).
              " Handle remote Exception
              " It contains details about the problems of your http(s) connection
            CATCH /iwbep/cx_gateway INTO DATA(lx_gateway).
              " Handle Exception
            CATCH cx_http_dest_provider_error INTO DATA(lx_http_error_read).
              " Handle Exception
            CATCH cx_web_http_client_error INTO DATA(lx_web_error_read).
              " Handle Exception
              RAISE SHORTDUMP lx_web_error_read.
          ENDTRY.


          SELECT
            equipment,
            deliverydocument,
            deliverydocumentitem
            FROM i_serialnumberdeliverydocument
            WHERE deliverydocument     = @<ls_item>-outbounddelivery
            AND   deliverydocumentitem = @<ls_item>-outbounddeliveryitem
            INTO TABLE @DATA(lt_serialnumber) PRIVILEGED ACCESS.

          DATA: lt_equip TYPE RANGE OF i_technicalobject-equipment,
                ls_equip LIKE LINE OF lt_equip.

          IF lt_serialnumber[] IS NOT INITIAL.
            CLEAR lv_string.

            CLEAR lt_equip.
            LOOP AT lt_serialnumber ASSIGNING FIELD-SYMBOL(<ls_serialnumber>).
              ls_equip-sign   = 'I'.
              ls_equip-option = 'EQ'.
              ls_equip-low    = <ls_serialnumber>-equipment.
              ls_equip-high   = ' '.
              APPEND ls_equip TO lt_equip.
            ENDLOOP.

            SELECT
              equipment,
              manufacturerserialnumber,
              serialnumber
              FROM i_technicalobject
              WHERE equipment IN @lt_equip
              INTO TABLE @DATA(lt_technicalobject) PRIVILEGED ACCESS.

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



            IF ls_business_data IS INITIAL.

              DATA:
                lo_request_create  TYPE REF TO /iwbep/if_cp_request_create,
                lo_response_create TYPE REF TO /iwbep/if_cp_response_create.

              TRY.

                  " Prepare the business data
                  CLEAR ls_business_data.
                  ls_business_data = VALUE #(
                            delivery_document           = <ls_item>-outbounddelivery
                            delivery_document_item      = <ls_item>-outbounddeliveryitem
                            text_element                = 'Z001'
                            language                    = 'EN'
                            "text_element_description    = 'Serial number'
                            text_element_text           = lv_string
                            "delivery_long_text_is_form  = abap_true
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

*                  " Get the after image
*                  lo_response->get_business_data( IMPORTING es_business_data = ls_business_data ).

                CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote_create).
                  " Handle remote Exception
                  " It contains details about the problems of your http(s) connection
                CATCH /iwbep/cx_gateway INTO DATA(lx_gateway_create).
                  " Handle Exception
                CATCH cx_http_dest_provider_error INTO DATA(lx_http_error_create).
                  " Handle Exception
                CATCH cx_web_http_client_error INTO DATA(lx_web_error_create).
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
                            text_element            = 'Z001'
                            language                = 'EN' ).

                  " Prepare the business data
                  CLEAR ls_business_data.
                  ls_business_data = VALUE #(
                            delivery_document           = <ls_item>-outbounddelivery
                            delivery_document_item      = <ls_item>-outbounddeliveryitem
                            text_element                = 'Z001'
                            language                    = 'EN'
*                            text_element_description    = 'Serial number'
                            text_element_text           = lv_string
*                            delivery_long_text_is_form  = abap_true
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

*                 " Get updated entity
*                  CLEAR ls_business_data.
*                  lo_response->get_business_data( importing es_business_data = ls_business_data ).

                CATCH /iwbep/cx_cp_remote INTO DATA(lx_remote_update).
                  " Handle remote Exception
                  " It contains details about the problems of your http(s) connection
                CATCH /iwbep/cx_gateway INTO DATA(lx_gateway_update).
                  " Handle Exception
                CATCH cx_http_dest_provider_error INTO DATA(lx_http_error_update).
                  " Handle Exception
                CATCH cx_web_http_client_error INTO DATA(lx_web_error_update).
                  " Handle Exception
                  RAISE SHORTDUMP lx_web_error_update.
              ENDTRY.

            ENDIF.

            CLEAR ls_business_data.

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
    DATA(lo_apj) = NEW zcl_apj(  ).

*    lo_apj->api_update_item_text(
*      iv_do   = '80000316'
*      iv_item = '10'
*      iv_text = 'Test from adt'
*    ).

    lo_apj->api_create_item_text(
         iv_do   = '0080000302'
         iv_item = '000010'
         iv_text = 'Test serial from adt'
    ).
  ENDMETHOD.
ENDCLASS.
