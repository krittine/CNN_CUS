"! <p class="shorttext synchronized">Consumption model for client proxy - generated</p>
"! This class has been generated based on the metadata with namespace
"! <em>API_OUTBOUND_DELIVERY_SRV</em>
CLASS ztestdo DEFINITION
  PUBLIC
  INHERITING FROM /iwbep/cl_v4_abs_pm_model_prov
  CREATE PUBLIC.

  PUBLIC SECTION.

    TYPES:
      "! <p class="shorttext synchronized">Types for "OData Primitive Types"</p>
      BEGIN OF tys_types_for_prim_types,
        "! Used for primitive type ACTUAL_DELIVERED_QTY_IN_BA
        actual_delivered_qty_in_ba TYPE decfloat16,
        "! Used for primitive type ACTUAL_DELIVERED_QTY_IN__2
        actual_delivered_qty_in__2 TYPE decfloat16,
        "! Used for primitive type ACTUAL_DELIVERY_QUANTITY
        actual_delivery_quantity   TYPE decfloat16,
        "! Used for primitive type ACTUAL_DELIVERY_QUANTITY_2
        actual_delivery_quantity_2 TYPE decfloat16,
        "! Used for primitive type ACTUAL_GOODS_MOVEMENT_DATE
        actual_goods_movement_date TYPE timestamp,
        "! Used for primitive type BASE_UNIT
        base_unit                  TYPE c LENGTH 3,
        "! Used for primitive type BASE_UNIT_2
        base_unit_2                TYPE c LENGTH 3,
        "! Used for primitive type BATCH
        batch                      TYPE c LENGTH 10,
        "! Used for primitive type BATCH_2
        batch_2                    TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT
        delivery_document          TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_10
        delivery_document_10       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_11
        delivery_document_11       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_12
        delivery_document_12       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_13
        delivery_document_13       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_14
        delivery_document_14       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_15
        delivery_document_15       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_16
        delivery_document_16       TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_2
        delivery_document_2        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_3
        delivery_document_3        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_4
        delivery_document_4        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_5
        delivery_document_5        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_6
        delivery_document_6        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_7
        delivery_document_7        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_8
        delivery_document_8        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_9
        delivery_document_9        TYPE c LENGTH 10,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM
        delivery_document_item     TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_10
        delivery_document_item_10  TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_2
        delivery_document_item_2   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_3
        delivery_document_item_3   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_4
        delivery_document_item_4   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_5
        delivery_document_item_5   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_6
        delivery_document_item_6   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_7
        delivery_document_item_7   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_8
        delivery_document_item_8   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_DOCUMENT_ITEM_9
        delivery_document_item_9   TYPE c LENGTH 6,
        "! Used for primitive type DELIVERY_QUANTITY_UNIT
        delivery_quantity_unit     TYPE c LENGTH 3,
        "! Used for primitive type DELIVERY_QUANTITY_UNIT_2
        delivery_quantity_unit_2   TYPE c LENGTH 3,
        "! Used for primitive type HANDLING_UNIT_EXTERNAL_ID
        handling_unit_external_id  TYPE c LENGTH 20,
        "! Used for primitive type PICK_QUANTITY_IN_SALES_UOM
        pick_quantity_in_sales_uom TYPE decfloat16,
        "! Used for primitive type SERIAL_NUMBER
        serial_number              TYPE c LENGTH 18,
        "! Used for primitive type SERIAL_NUMBER_2
        serial_number_2            TYPE c LENGTH 18,
        "! Used for primitive type SPLIT_QUANTITY
        split_quantity             TYPE decfloat16,
        "! Used for primitive type SPLIT_QUANTITY_UNIT
        split_quantity_unit        TYPE c LENGTH 3,
      END OF tys_types_for_prim_types.

    TYPES:
      "! <p class="shorttext synchronized">CreatedDeliveryItem</p>
      BEGIN OF tys_created_delivery_item,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
      END OF tys_created_delivery_item,
      "! <p class="shorttext synchronized">List of CreatedDeliveryItem</p>
      tyt_created_delivery_item TYPE STANDARD TABLE OF tys_created_delivery_item WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">HuReturn</p>
      BEGIN OF tys_hu_return,
        "! SystemMessageIdentification
        system_message_identificat TYPE c LENGTH 20,
        "! SystemMessageNumber
        system_message_number      TYPE c LENGTH 3,
        "! SystemMessageType
        system_message_type        TYPE c LENGTH 1,
        "! SystemMessageVariable1
        system_message_variable_1  TYPE c LENGTH 50,
        "! SystemMessageVariable2
        system_message_variable_2  TYPE c LENGTH 50,
        "! SystemMessageVariable3
        system_message_variable_3  TYPE c LENGTH 50,
        "! SystemMessageVariable4
        system_message_variable_4  TYPE c LENGTH 50,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
      END OF tys_hu_return,
      "! <p class="shorttext synchronized">List of HuReturn</p>
      tyt_hu_return TYPE STANDARD TABLE OF tys_hu_return WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">PickingReport</p>
      BEGIN OF tys_picking_report,
        "! SystemMessageIdentification
        system_message_identificat TYPE c LENGTH 20,
        "! SystemMessageNumber
        system_message_number      TYPE c LENGTH 3,
        "! SystemMessageType
        system_message_type        TYPE c LENGTH 1,
        "! SystemMessageVariable1
        system_message_variable_1  TYPE c LENGTH 50,
        "! SystemMessageVariable2
        system_message_variable_2  TYPE c LENGTH 50,
        "! SystemMessageVariable3
        system_message_variable_3  TYPE c LENGTH 50,
        "! SystemMessageVariable4
        system_message_variable_4  TYPE c LENGTH 50,
        "! Batch
        batch                      TYPE c LENGTH 10,
        "! DeliveryQuantityUnit
        delivery_quantity_unit     TYPE c LENGTH 3,
        "! ActualDeliveryQuantity
        actual_delivery_quantity   TYPE p LENGTH 7 DECIMALS 3,
        "! DeliveryDocumentItemText
        delivery_document_item_tex TYPE c LENGTH 40,
        "! Material
        material                   TYPE c LENGTH 40,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
      END OF tys_picking_report,
      "! <p class="shorttext synchronized">List of PickingReport</p>
      tyt_picking_report TYPE STANDARD TABLE OF tys_picking_report WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Return</p>
      BEGIN OF tys_return,
        "! CollectiveProcessing
        collective_processing      TYPE c LENGTH 10,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! ScheduleLine
        schedule_line              TYPE c LENGTH 4,
        "! CollectiveProcessingMsgCounter
        collective_processing_msg  TYPE c LENGTH 2,
        "! SystemMessageIdentification
        system_message_identificat TYPE c LENGTH 20,
        "! SystemMessageNumber
        system_message_number      TYPE c LENGTH 3,
        "! SystemMessageType
        system_message_type        TYPE c LENGTH 1,
        "! SystemMessageVariable1
        system_message_variable_1  TYPE c LENGTH 50,
        "! SystemMessageVariable2
        system_message_variable_2  TYPE c LENGTH 50,
        "! SystemMessageVariable3
        system_message_variable_3  TYPE c LENGTH 50,
        "! SystemMessageVariable4
        system_message_variable_4  TYPE c LENGTH 50,
        "! CollectiveProcessingType
        collective_processing_type TYPE c LENGTH 1,
      END OF tys_return,
      "! <p class="shorttext synchronized">List of Return</p>
      tyt_return TYPE STANDARD TABLE OF tys_return WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function AddSerialNumberToDeliveryItem</p>
      "! <em>with the internal name</em> ADD_SERIAL_NUMBER_TO_DELIV
      BEGIN OF tys_parameters_1,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
        "! SerialNumber
        serial_number          TYPE c LENGTH 18,
      END OF tys_parameters_1,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_1</p>
      tyt_parameters_1 TYPE STANDARD TABLE OF tys_parameters_1 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function PickAndBatchSplitOneItem</p>
      "! <em>with the internal name</em> PICK_AND_BATCH_SPLIT_ONE_I
      BEGIN OF tys_parameters_2,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
        "! Batch
        batch                  TYPE c LENGTH 10,
        "! SplitQuantity
        split_quantity         TYPE decfloat16,
        "! SplitQuantityUnit
        split_quantity_unit    TYPE c LENGTH 3,
      END OF tys_parameters_2,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_2</p>
      tyt_parameters_2 TYPE STANDARD TABLE OF tys_parameters_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function PickOneItem</p>
      "! <em>with the internal name</em> PICK_ONE_ITEM
      BEGIN OF tys_parameters_3,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
      END OF tys_parameters_3,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_3</p>
      tyt_parameters_3 TYPE STANDARD TABLE OF tys_parameters_3 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function PickOneItemWithBaseQuantity</p>
      "! <em>with the internal name</em> PICK_ONE_ITEM_WITH_BASE_QU
      BEGIN OF tys_parameters_4,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! ActualDeliveredQtyInBaseUnit
        actual_delivered_qty_in_ba TYPE decfloat16,
        "! BaseUnit
        base_unit                  TYPE c LENGTH 3,
      END OF tys_parameters_4,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_4</p>
      tyt_parameters_4 TYPE STANDARD TABLE OF tys_parameters_4 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function PickOneItemWithSalesQuantity</p>
      "! <em>with the internal name</em> PICK_ONE_ITEM_WITH_SALES_Q
      BEGIN OF tys_parameters_5,
        "! ActualDeliveryQuantity
        actual_delivery_quantity TYPE decfloat16,
        "! DeliveryDocument
        delivery_document        TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item   TYPE c LENGTH 6,
        "! DeliveryQuantityUnit
        delivery_quantity_unit   TYPE c LENGTH 3,
      END OF tys_parameters_5,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_5</p>
      tyt_parameters_5 TYPE STANDARD TABLE OF tys_parameters_5 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function PostGoodsIssue</p>
      "! <em>with the internal name</em> POST_GOODS_ISSUE
      BEGIN OF tys_parameters_6,
        "! DeliveryDocument
        delivery_document TYPE c LENGTH 10,
      END OF tys_parameters_6,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_6</p>
      tyt_parameters_6 TYPE STANDARD TABLE OF tys_parameters_6 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function ReverseGoodsIssue</p>
      "! <em>with the internal name</em> REVERSE_GOODS_ISSUE
      BEGIN OF tys_parameters_7,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! ActualGoodsMovementDate
        actual_goods_movement_date TYPE timestamp,
      END OF tys_parameters_7,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_7</p>
      tyt_parameters_7 TYPE STANDARD TABLE OF tys_parameters_7 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function SetPickingQuantityWithBaseQuantity</p>
      "! <em>with the internal name</em> SET_PICKING_QUANTITY_WITH
      BEGIN OF tys_parameters_8,
        "! ActualDeliveredQtyInBaseUnit
        actual_delivered_qty_in_ba TYPE decfloat16,
        "! BaseUnit
        base_unit                  TYPE c LENGTH 3,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
      END OF tys_parameters_8,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_8</p>
      tyt_parameters_8 TYPE STANDARD TABLE OF tys_parameters_8 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function AssignHandlingUnitToDelivery</p>
      "! <em>with the internal name</em> ASSIGN_HANDLING_UNIT_TO_DE
      BEGIN OF tys_parameters_9,
        "! DeliveryDocument
        delivery_document         TYPE c LENGTH 10,
        "! HandlingUnitExternalId
        handling_unit_external_id TYPE c LENGTH 20,
      END OF tys_parameters_9,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_9</p>
      tyt_parameters_9 TYPE STANDARD TABLE OF tys_parameters_9 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function ConfirmPickingAllItems</p>
      "! <em>with the internal name</em> CONFIRM_PICKING_ALL_ITEMS
      BEGIN OF tys_parameters_10,
        "! DeliveryDocument
        delivery_document TYPE c LENGTH 10,
      END OF tys_parameters_10,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_10</p>
      tyt_parameters_10 TYPE STANDARD TABLE OF tys_parameters_10 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function ConfirmPickingOneItem</p>
      "! <em>with the internal name</em> CONFIRM_PICKING_ONE_ITEM
      BEGIN OF tys_parameters_11,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
      END OF tys_parameters_11,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_11</p>
      tyt_parameters_11 TYPE STANDARD TABLE OF tys_parameters_11 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function CreateBatchSplitItem</p>
      "! <em>with the internal name</em> CREATE_BATCH_SPLIT_ITEM
      BEGIN OF tys_parameters_12,
        "! PickQuantityInSalesUOM
        pick_quantity_in_sales_uom TYPE decfloat16,
        "! Batch
        batch                      TYPE c LENGTH 10,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! ActualDeliveryQuantity
        actual_delivery_quantity   TYPE decfloat16,
        "! DeliveryQuantityUnit
        delivery_quantity_unit     TYPE c LENGTH 3,
      END OF tys_parameters_12,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_12</p>
      tyt_parameters_12 TYPE STANDARD TABLE OF tys_parameters_12 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function DeleteAllHandlingUnitsFromDelivery</p>
      "! <em>with the internal name</em> DELETE_ALL_HANDLING_UNITS
      BEGIN OF tys_parameters_13,
        "! DeliveryDocument
        delivery_document TYPE c LENGTH 10,
      END OF tys_parameters_13,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_13</p>
      tyt_parameters_13 TYPE STANDARD TABLE OF tys_parameters_13 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function DeleteAllSerialNumbersFromDeliveryItem</p>
      "! <em>with the internal name</em> DELETE_ALL_SERIAL_NUMBERS
      BEGIN OF tys_parameters_14,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
      END OF tys_parameters_14,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_14</p>
      tyt_parameters_14 TYPE STANDARD TABLE OF tys_parameters_14 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function DeleteSerialNumberFromDeliveryItem</p>
      "! <em>with the internal name</em> DELETE_SERIAL_NUMBER_FROM
      BEGIN OF tys_parameters_15,
        "! DeliveryDocument
        delivery_document      TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item TYPE c LENGTH 6,
        "! SerialNumber
        serial_number          TYPE c LENGTH 18,
      END OF tys_parameters_15,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_15</p>
      tyt_parameters_15 TYPE STANDARD TABLE OF tys_parameters_15 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">Parameters of function PickAllItems</p>
      "! <em>with the internal name</em> PICK_ALL_ITEMS
      BEGIN OF tys_parameters_16,
        "! DeliveryDocument
        delivery_document TYPE c LENGTH 10,
      END OF tys_parameters_16,
      "! <p class="shorttext synchronized">List of TYS_PARAMETERS_16</p>
      tyt_parameters_16 TYPE STANDARD TABLE OF tys_parameters_16 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_HandlingUnitHeaderDeliveryType</p>
      BEGIN OF tys_a_handling_unit_header_d_2,
        "! CreatedByUser
        created_by_user            TYPE c LENGTH 12,
        "! CreationDate
        creation_date              TYPE timestampl,
        "! CreationTime
        creation_time              TYPE timn,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! GrossVolume
        gross_volume               TYPE p LENGTH 8 DECIMALS 3,
        "! GrossWeight
        gross_weight               TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitBaseUnitOfMeasure
        handling_unit_base_unit_of TYPE c LENGTH 3,
        "! HandlingUnitContentDescription
        handling_unit_content_desc TYPE c LENGTH 40,
        "! HandlingUnitExternalId
        handling_unit_external_id  TYPE c LENGTH 20,
        "! HandlingUnitExternalIdType
        handling_unit_external_i_2 TYPE c LENGTH 1,
        "! HandlingUnitHeight
        handling_unit_height       TYPE p LENGTH 7 DECIMALS 3,
        "! <em>Key property</em> HandlingUnitInternalId
        handling_unit_internal_id  TYPE c LENGTH 10,
        "! HandlingUnitInternalStatus
        handling_unit_internal_sta TYPE c LENGTH 4,
        "! HandlingUnitLength
        handling_unit_length       TYPE p LENGTH 7 DECIMALS 3,
        "! HandlingUnitLowerLevelRefer
        handling_unit_lower_level  TYPE c LENGTH 10,
        "! HandlingUnitMaxVolume
        handling_unit_max_volume   TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitMaxWeight
        handling_unit_max_weight   TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitNetVolume
        handling_unit_net_volume   TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitSecondExternalId
        handling_unit_second_exter TYPE c LENGTH 20,
        "! HandlingUnitTareVolume
        handling_unit_tare_volume  TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitTareVolumeUnit
        handling_unit_tare_volum_2 TYPE c LENGTH 3,
        "! HandlingUnitTareWeight
        handling_unit_tare_weight  TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitTareWeightUnit
        handling_unit_tare_weigh_2 TYPE c LENGTH 3,
        "! HandlingUnitUoMDimension
        handling_unit_uo_mdimensio TYPE c LENGTH 3,
        "! HandlingUnitWidth
        handling_unit_width        TYPE p LENGTH 7 DECIMALS 3,
        "! LastChangeDate
        last_change_date           TYPE timestampl,
        "! LastChangedByUser
        last_changed_by_user       TYPE c LENGTH 12,
        "! LastChangeTime
        last_change_time           TYPE timn,
        "! NetWeight
        net_weight                 TYPE p LENGTH 8 DECIMALS 3,
        "! PackagingMaterial
        packaging_material         TYPE c LENGTH 40,
        "! PackagingMaterialCategory
        packaging_material_categor TYPE c LENGTH 1,
        "! PackagingMaterialGroup
        packaging_material_group   TYPE c LENGTH 4,
        "! PackagingMaterialType
        packaging_material_type    TYPE c LENGTH 4,
        "! PackingInstructionNumber
        packing_instruction_number TYPE c LENGTH 20,
        "! ShippingPoint
        shipping_point             TYPE c LENGTH 4,
        "! VolumeUnit
        volume_unit                TYPE c LENGTH 3,
        "! WeightUnit
        weight_unit                TYPE c LENGTH 3,
      END OF tys_a_handling_unit_header_d_2,
      "! <p class="shorttext synchronized">List of A_HandlingUnitHeaderDeliveryType</p>
      tyt_a_handling_unit_header_d_2 TYPE STANDARD TABLE OF tys_a_handling_unit_header_d_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_HandlingUnitItemDeliveryType</p>
      BEGIN OF tys_a_handling_unit_item_del_2,
        "! Batch
        batch                      TYPE c LENGTH 10,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! HandlingUnitExternalId
        handling_unit_external_id  TYPE c LENGTH 20,
        "! <em>Key property</em> HandlingUnitInternalId
        handling_unit_internal_id  TYPE c LENGTH 10,
        "! <em>Key property</em> HandlingUnitItem
        handling_unit_item         TYPE c LENGTH 6,
        "! HandlingUnitNestedInternalId
        handling_unit_nested_inter TYPE c LENGTH 10,
        "! HandlingUnitNrOfAuxPackgMat
        handling_unit_nr_of_aux_pa TYPE int4,
        "! HandlingUnitNumberOfSerialnumb
        handling_unit_number_of_se TYPE int4,
        "! HandlingUnitQuantity
        handling_unit_quantity     TYPE p LENGTH 8 DECIMALS 3,
        "! HandlingUnitQuantityUnit
        handling_unit_quantity_uni TYPE c LENGTH 3,
        "! HandlingUnitTypeOfContent
        handling_unit_type_of_cont TYPE c LENGTH 1,
        "! Material
        material                   TYPE c LENGTH 40,
        "! MaterialByCustomer
        material_by_customer       TYPE c LENGTH 35,
        "! MaterialName
        material_name              TYPE c LENGTH 40,
        "! SerialNumberProfile
        serial_number_profile      TYPE c LENGTH 4,
      END OF tys_a_handling_unit_item_del_2,
      "! <p class="shorttext synchronized">List of A_HandlingUnitItemDeliveryType</p>
      tyt_a_handling_unit_item_del_2 TYPE STANDARD TABLE OF tys_a_handling_unit_item_del_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_MaintenanceItemObjectType</p>
      BEGIN OF tys_a_maintenance_item_objec_2,
        "! Assembly
        assembly                   TYPE c LENGTH 40,
        "! Equipment
        equipment                  TYPE c LENGTH 18,
        "! FunctionalLocation
        functional_location        TYPE c LENGTH 30,
        "! <em>Key property</em> MaintenanceItemObject
        maintenance_item_object    TYPE int4,
        "! <em>Key property</em> MaintenanceItemObjectList
        maintenance_item_object_li TYPE int8,
        "! MaintenanceNotification
        maintenance_notification   TYPE c LENGTH 12,
        "! MaintObjectLocAcctAssgmtNmbr
        maint_object_loc_acct_assg TYPE c LENGTH 12,
        "! Material
        material                   TYPE c LENGTH 40,
        "! SerialNumber
        serial_number              TYPE c LENGTH 18,
      END OF tys_a_maintenance_item_objec_2,
      "! <p class="shorttext synchronized">List of A_MaintenanceItemObjectType</p>
      tyt_a_maintenance_item_objec_2 TYPE STANDARD TABLE OF tys_a_maintenance_item_objec_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryAddressType</p>
      BEGIN OF tys_a_outb_delivery_address_ty,
        "! AdditionalStreetPrefixName
        additional_street_prefix_n TYPE c LENGTH 40,
        "! AdditionalStreetSuffixName
        additional_street_suffix_n TYPE c LENGTH 40,
        "! <em>Key property</em> AddressID
        address_id                 TYPE c LENGTH 10,
        "! AddressTimeZone
        address_time_zone          TYPE c LENGTH 6,
        "! Building
        building                   TYPE c LENGTH 20,
        "! BusinessPartnerName1
        business_partner_name_1    TYPE c LENGTH 40,
        "! BusinessPartnerName2
        business_partner_name_2    TYPE c LENGTH 40,
        "! BusinessPartnerName3
        business_partner_name_3    TYPE c LENGTH 40,
        "! BusinessPartnerName4
        business_partner_name_4    TYPE c LENGTH 40,
        "! CareOfName
        care_of_name               TYPE c LENGTH 40,
        "! CityCode
        city_code                  TYPE c LENGTH 12,
        "! CityName
        city_name                  TYPE c LENGTH 40,
        "! CitySearch
        city_search                TYPE c LENGTH 25,
        "! CompanyPostalCode
        company_postal_code        TYPE c LENGTH 10,
        "! CorrespondenceLanguage
        correspondence_language    TYPE c LENGTH 2,
        "! Country
        country                    TYPE c LENGTH 3,
        "! County
        county                     TYPE c LENGTH 40,
        "! DeliveryServiceNumber
        delivery_service_number    TYPE c LENGTH 10,
        "! DeliveryServiceTypeCode
        delivery_service_type_code TYPE c LENGTH 4,
        "! District
        district                   TYPE c LENGTH 40,
        "! FaxNumber
        fax_number                 TYPE c LENGTH 30,
        "! Floor
        floor                      TYPE c LENGTH 10,
        "! FormOfAddress
        form_of_address            TYPE c LENGTH 4,
        "! FullName
        full_name                  TYPE c LENGTH 80,
        "! HomeCityName
        home_city_name             TYPE c LENGTH 40,
        "! HouseNumber
        house_number               TYPE c LENGTH 10,
        "! HouseNumberSupplementText
        house_number_supplement_te TYPE c LENGTH 10,
        "! Nation
        nation                     TYPE c LENGTH 1,
        "! Person
        person                     TYPE c LENGTH 10,
        "! PhoneNumber
        phone_number               TYPE c LENGTH 30,
        "! <em>Key property</em> POBox
        pobox                      TYPE c LENGTH 10,
        "! POBoxDeviatingCityName
        pobox_deviating_city_name  TYPE c LENGTH 40,
        "! <em>Key property</em> POBoxDeviatingCountry
        pobox_deviating_country    TYPE c LENGTH 3,
        "! POBoxDeviatingRegion
        pobox_deviating_region     TYPE c LENGTH 3,
        "! POBoxIsWithoutNumber
        pobox_is_without_number    TYPE abap_bool,
        "! POBoxLobbyName
        pobox_lobby_name           TYPE c LENGTH 40,
        "! POBoxPostalCode
        pobox_postal_code          TYPE c LENGTH 10,
        "! PostalCode
        postal_code                TYPE c LENGTH 10,
        "! PrfrdCommMediumType
        prfrd_comm_medium_type     TYPE c LENGTH 3,
        "! Region
        region                     TYPE c LENGTH 3,
        "! RoomNumber
        room_number                TYPE c LENGTH 10,
        "! SearchTerm1
        search_term_1              TYPE c LENGTH 20,
        "! StreetName
        street_name                TYPE c LENGTH 60,
        "! StreetPrefixName
        street_prefix_name         TYPE c LENGTH 40,
        "! StreetSearch
        street_search              TYPE c LENGTH 25,
        "! StreetSuffixName
        street_suffix_name         TYPE c LENGTH 40,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! TransportZone
        transport_zone             TYPE c LENGTH 10,
      END OF tys_a_outb_delivery_address_ty,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryAddressType</p>
      tyt_a_outb_delivery_address_ty TYPE STANDARD TABLE OF tys_a_outb_delivery_address_ty WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryAddress2Type</p>
      BEGIN OF tys_a_outb_delivery_address__2,
        "! <em>Key property</em> DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! <em>Key property</em> PartnerFunction
        partner_function           TYPE c LENGTH 2,
        "! DeliveryVersion
        delivery_version           TYPE c LENGTH 4,
        "! AdditionalStreetPrefixName
        additional_street_prefix_n TYPE c LENGTH 40,
        "! AddressID
        address_id                 TYPE c LENGTH 10,
        "! AdditionalStreetSuffixName
        additional_street_suffix_n TYPE c LENGTH 40,
        "! AddressTimeZone
        address_time_zone          TYPE c LENGTH 6,
        "! Building
        building                   TYPE c LENGTH 20,
        "! BusinessPartnerName1
        business_partner_name_1    TYPE c LENGTH 40,
        "! BusinessPartnerName2
        business_partner_name_2    TYPE c LENGTH 40,
        "! BusinessPartnerName3
        business_partner_name_3    TYPE c LENGTH 40,
        "! BusinessPartnerName4
        business_partner_name_4    TYPE c LENGTH 40,
        "! PersonFamilyName
        person_family_name         TYPE c LENGTH 40,
        "! PersonGivenName
        person_given_name          TYPE c LENGTH 40,
        "! CareOfName
        care_of_name               TYPE c LENGTH 40,
        "! CityName
        city_name                  TYPE c LENGTH 40,
        "! CompanyPostalCode
        company_postal_code        TYPE c LENGTH 10,
        "! CorrespondenceLanguage
        correspondence_language    TYPE c LENGTH 2,
        "! Country
        country                    TYPE c LENGTH 3,
        "! County
        county                     TYPE c LENGTH 40,
        "! DeliveryServiceNumber
        delivery_service_number    TYPE c LENGTH 10,
        "! DeliveryServiceTypeCode
        delivery_service_type_code TYPE c LENGTH 4,
        "! District
        district                   TYPE c LENGTH 40,
        "! EmailAddress
        email_address              TYPE c LENGTH 241,
        "! FaxNumber
        fax_number                 TYPE c LENGTH 30,
        "! FaxNumberExtension
        fax_number_extension       TYPE c LENGTH 10,
        "! Floor
        floor                      TYPE c LENGTH 10,
        "! FormOfAddress
        form_of_address            TYPE c LENGTH 4,
        "! HomeCityName
        home_city_name             TYPE c LENGTH 40,
        "! HouseNumber
        house_number               TYPE c LENGTH 10,
        "! HouseNumberSupplementText
        house_number_supplement_te TYPE c LENGTH 10,
        "! MobilePhoneNumber
        mobile_phone_number        TYPE c LENGTH 30,
        "! PhoneNumber
        phone_number               TYPE c LENGTH 30,
        "! PhoneNumberExtension
        phone_number_extension     TYPE c LENGTH 10,
        "! POBox
        pobox                      TYPE c LENGTH 10,
        "! POBoxDeviatingCityName
        pobox_deviating_city_name  TYPE c LENGTH 40,
        "! POBoxDeviatingCountry
        pobox_deviating_country    TYPE c LENGTH 3,
        "! POBoxDeviatingRegion
        pobox_deviating_region     TYPE c LENGTH 3,
        "! POBoxIsWithoutNumber
        pobox_is_without_number    TYPE abap_bool,
        "! POBoxLobbyName
        pobox_lobby_name           TYPE c LENGTH 40,
        "! POBoxPostalCode
        pobox_postal_code          TYPE c LENGTH 10,
        "! PostalCode
        postal_code                TYPE c LENGTH 10,
        "! PrfrdCommMediumType
        prfrd_comm_medium_type     TYPE c LENGTH 3,
        "! Region
        region                     TYPE c LENGTH 3,
        "! RoomNumber
        room_number                TYPE c LENGTH 10,
        "! StreetName
        street_name                TYPE c LENGTH 60,
        "! StreetPrefixName
        street_prefix_name         TYPE c LENGTH 40,
        "! StreetSuffixName
        street_suffix_name         TYPE c LENGTH 40,
        "! TaxJurisdiction
        tax_jurisdiction           TYPE c LENGTH 15,
        "! TransportZone
        transport_zone             TYPE c LENGTH 10,
        "! odata.etag
        etag                       TYPE string,
      END OF tys_a_outb_delivery_address__2,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryAddress2Type</p>
      tyt_a_outb_delivery_address__2 TYPE STANDARD TABLE OF tys_a_outb_delivery_address__2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryDocFlowType</p>
      BEGIN OF tys_a_outb_delivery_doc_flow_t,
        "! Deliveryversion
        deliveryversion            TYPE c LENGTH 4,
        "! <em>Key property</em> PrecedingDocument
        preceding_document         TYPE c LENGTH 10,
        "! PrecedingDocumentCategory
        preceding_document_categor TYPE c LENGTH 4,
        "! <em>Key property</em> PrecedingDocumentItem
        preceding_document_item    TYPE c LENGTH 6,
        "! Subsequentdocument
        subsequentdocument         TYPE c LENGTH 10,
        "! QuantityInBaseUnit
        quantity_in_base_unit      TYPE p LENGTH 8 DECIMALS 3,
        "! SubsequentDocumentItem
        subsequent_document_item   TYPE c LENGTH 6,
        "! SDFulfillmentCalculationRule
        sdfulfillment_calculation  TYPE c LENGTH 1,
        "! <em>Key property</em> SubsequentDocumentCategory
        subsequent_document_catego TYPE c LENGTH 4,
        "! TransferOrderInWrhsMgmtIsConfd
        transfer_order_in_wrhs_mgm TYPE abap_bool,
        "! odata.etag
        etag                       TYPE string,
      END OF tys_a_outb_delivery_doc_flow_t,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryDocFlowType</p>
      tyt_a_outb_delivery_doc_flow_t TYPE STANDARD TABLE OF tys_a_outb_delivery_doc_flow_t WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryHeaderType</p>
      BEGIN OF tys_a_outb_delivery_header_typ,
        "! ActualDeliveryRoute
        actual_delivery_route      TYPE c LENGTH 6,
        "! Shippinglocationtimezone
        shippinglocationtimezone   TYPE c LENGTH 6,
        "! ActualGoodsMovementDate
        actual_goods_movement_date TYPE timestampl,
        "! Receivinglocationtimezone
        receivinglocationtimezone  TYPE c LENGTH 6,
        "! ActualGoodsMovementTime
        actual_goods_movement_time TYPE timn,
        "! BillingDocumentDate
        billing_document_date      TYPE timestampl,
        "! BillOfLading
        bill_of_lading             TYPE c LENGTH 35,
        "! CompleteDeliveryIsDefined
        complete_delivery_is_defin TYPE abap_bool,
        "! ConfirmationTime
        confirmation_time          TYPE timn,
        "! CreatedByUser
        created_by_user            TYPE c LENGTH 12,
        "! CreationDate
        creation_date              TYPE timestampl,
        "! CreationTime
        creation_time              TYPE timn,
        "! CustomerGroup
        customer_group             TYPE c LENGTH 2,
        "! DeliveryBlockReason
        delivery_block_reason      TYPE c LENGTH 2,
        "! DeliveryDate
        delivery_date              TYPE timestampl,
        "! <em>Key property</em> DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentBySupplier
        delivery_document_by_suppl TYPE c LENGTH 35,
        "! DeliveryDocumentType
        delivery_document_type     TYPE c LENGTH 4,
        "! DeliveryIsInPlant
        delivery_is_in_plant       TYPE abap_bool,
        "! DeliveryPriority
        delivery_priority          TYPE c LENGTH 2,
        "! DeliveryTime
        delivery_time              TYPE timn,
        "! DeliveryVersion
        delivery_version           TYPE c LENGTH 4,
        "! DepreciationPercentage
        depreciation_percentage    TYPE p LENGTH 3 DECIMALS 2,
        "! DistrStatusByDecentralizedWrhs
        distr_status_by_decentrali TYPE c LENGTH 1,
        "! DocumentDate
        document_date              TYPE timestampl,
        "! ExternalIdentificationType
        external_identification_ty TYPE c LENGTH 1,
        "! ExternalTransportSystem
        external_transport_system  TYPE c LENGTH 5,
        "! FactoryCalendarByCustomer
        factory_calendar_by_custom TYPE c LENGTH 2,
        "! GoodsIssueOrReceiptSlipNumber
        goods_issue_or_receipt_sli TYPE c LENGTH 10,
        "! GoodsIssueTime
        goods_issue_time           TYPE timn,
        "! HandlingUnitInStock
        handling_unit_in_stock     TYPE c LENGTH 1,
        "! HdrGeneralIncompletionStatus
        hdr_general_incompletion_s TYPE c LENGTH 1,
        "! HdrGoodsMvtIncompletionStatus
        hdr_goods_mvt_incompletion TYPE c LENGTH 1,
        "! HeaderBillgIncompletionStatus
        header_billg_incompletion  TYPE c LENGTH 1,
        "! HeaderBillingBlockReason
        header_billing_block_reaso TYPE c LENGTH 2,
        "! HeaderDelivIncompletionStatus
        header_deliv_incompletion  TYPE c LENGTH 1,
        "! HeaderGrossWeight
        header_gross_weight        TYPE p LENGTH 8 DECIMALS 3,
        "! HeaderNetWeight
        header_net_weight          TYPE p LENGTH 8 DECIMALS 3,
        "! HeaderPackingIncompletionSts
        header_packing_incompletio TYPE c LENGTH 1,
        "! HeaderPickgIncompletionStatus
        header_pickg_incompletion  TYPE c LENGTH 1,
        "! HeaderVolume
        header_volume              TYPE p LENGTH 8 DECIMALS 3,
        "! HeaderVolumeUnit
        header_volume_unit         TYPE c LENGTH 3,
        "! HeaderWeightUnit
        header_weight_unit         TYPE c LENGTH 3,
        "! IncotermsClassification
        incoterms_classification   TYPE c LENGTH 3,
        "! IncotermsTransferLocation
        incoterms_transfer_locatio TYPE c LENGTH 28,
        "! IntercompanyBillingDate
        intercompany_billing_date  TYPE timestampl,
        "! InternalFinancialDocument
        internal_financial_documen TYPE c LENGTH 10,
        "! IsDeliveryForSingleWarehouse
        is_delivery_for_single_war TYPE c LENGTH 1,
        "! IsExportDelivery
        is_export_delivery         TYPE c LENGTH 1,
        "! LastChangeDate
        last_change_date           TYPE timestampl,
        "! LastChangedByUser
        last_changed_by_user       TYPE c LENGTH 12,
        "! LoadingDate
        loading_date               TYPE timestampl,
        "! LoadingPoint
        loading_point              TYPE c LENGTH 2,
        "! LoadingTime
        loading_time               TYPE timn,
        "! MeansOfTransport
        means_of_transport         TYPE c LENGTH 20,
        "! MeansOfTransportRefMaterial
        means_of_transport_ref_mat TYPE c LENGTH 40,
        "! MeansOfTransportType
        means_of_transport_type    TYPE c LENGTH 4,
        "! OrderCombinationIsAllowed
        order_combination_is_allow TYPE abap_bool,
        "! OrderID
        order_id                   TYPE c LENGTH 12,
        "! OverallDelivConfStatus
        overall_deliv_conf_status  TYPE c LENGTH 1,
        "! OverallDelivReltdBillgStatus
        overall_deliv_reltd_billg  TYPE c LENGTH 1,
        "! OverallGoodsMovementStatus
        overall_goods_movement_sta TYPE c LENGTH 1,
        "! OverallIntcoBillingStatus
        overall_intco_billing_stat TYPE c LENGTH 1,
        "! OverallPackingStatus
        overall_packing_status     TYPE c LENGTH 1,
        "! OverallPickingConfStatus
        overall_picking_conf_statu TYPE c LENGTH 1,
        "! OverallPickingStatus
        overall_picking_status     TYPE c LENGTH 1,
        "! OverallProofOfDeliveryStatus
        overall_proof_of_delivery  TYPE c LENGTH 1,
        "! OverallSDProcessStatus
        overall_sdprocess_status   TYPE c LENGTH 1,
        "! OverallWarehouseActivityStatus
        overall_warehouse_activity TYPE c LENGTH 1,
        "! OvrlItmDelivIncompletionSts
        ovrl_itm_deliv_incompletio TYPE c LENGTH 1,
        "! OvrlItmGdsMvtIncompletionSts
        ovrl_itm_gds_mvt_incomplet TYPE c LENGTH 1,
        "! OvrlItmGeneralIncompletionSts
        ovrl_itm_general_incomplet TYPE c LENGTH 1,
        "! OvrlItmPackingIncompletionSts
        ovrl_itm_packing_incomplet TYPE c LENGTH 1,
        "! OvrlItmPickingIncompletionSts
        ovrl_itm_picking_incomplet TYPE c LENGTH 1,
        "! PaymentGuaranteeProcedure
        payment_guarantee_procedur TYPE c LENGTH 6,
        "! PickedItemsLocation
        picked_items_location      TYPE c LENGTH 20,
        "! PickingDate
        picking_date               TYPE timestampl,
        "! PickingTime
        picking_time               TYPE timn,
        "! PlannedGoodsIssueDate
        planned_goods_issue_date   TYPE timestampl,
        "! ProofOfDeliveryDate
        proof_of_delivery_date     TYPE timestampl,
        "! ProposedDeliveryRoute
        proposed_delivery_route    TYPE c LENGTH 6,
        "! ReceivingPlant
        receiving_plant            TYPE c LENGTH 4,
        "! RouteSchedule
        route_schedule             TYPE c LENGTH 10,
        "! SalesDistrict
        sales_district             TYPE c LENGTH 6,
        "! SalesOffice
        sales_office               TYPE c LENGTH 4,
        "! SalesOrganization
        sales_organization         TYPE c LENGTH 4,
        "! SDDocumentCategory
        sddocument_category        TYPE c LENGTH 4,
        "! ShipmentBlockReason
        shipment_block_reason      TYPE c LENGTH 2,
        "! ShippingCondition
        shipping_condition         TYPE c LENGTH 2,
        "! ShippingPoint
        shipping_point             TYPE c LENGTH 4,
        "! ShippingType
        shipping_type              TYPE c LENGTH 2,
        "! ShipToParty
        ship_to_party              TYPE c LENGTH 10,
        "! SoldToParty
        sold_to_party              TYPE c LENGTH 10,
        "! SpecialProcessingCode
        special_processing_code    TYPE c LENGTH 4,
        "! StatisticsCurrency
        statistics_currency        TYPE c LENGTH 5,
        "! Supplier
        supplier                   TYPE c LENGTH 10,
        "! TotalBlockStatus
        total_block_status         TYPE c LENGTH 1,
        "! TotalCreditCheckStatus
        total_credit_check_status  TYPE c LENGTH 1,
        "! TotalNumberOfPackage
        total_number_of_package    TYPE c LENGTH 5,
        "! TransactionCurrency
        transaction_currency       TYPE c LENGTH 5,
        "! TransportationGroup
        transportation_group       TYPE c LENGTH 4,
        "! TransportationPlanningDate
        transportation_planning_da TYPE timestampl,
        "! TransportationPlanningStatus
        transportation_planning_st TYPE c LENGTH 1,
        "! TransportationPlanningTime
        transportation_planning_ti TYPE timn,
        "! UnloadingPointName
        unloading_point_name       TYPE c LENGTH 25,
        "! Warehouse
        warehouse                  TYPE c LENGTH 3,
        "! WarehouseGate
        warehouse_gate             TYPE c LENGTH 3,
        "! WarehouseStagingArea
        warehouse_staging_area     TYPE c LENGTH 10,
        "! odata.etag
        etag                       TYPE string,
      END OF tys_a_outb_delivery_header_typ,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryHeaderType</p>
      tyt_a_outb_delivery_header_typ TYPE STANDARD TABLE OF tys_a_outb_delivery_header_typ WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryHeaderTextType</p>
      BEGIN OF tys_a_outb_delivery_header_t_2,
        "! <em>Key property</em> DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! <em>Key property</em> TextElement
        text_element               TYPE c LENGTH 4,
        "! <em>Key property</em> Language
        language                   TYPE c LENGTH 2,
        "! TextElementDescription
        text_element_description   TYPE c LENGTH 30,
        "! TextElementText
        text_element_text          TYPE string,
        "! DeliveryLongTextIsFormatted
        delivery_long_text_is_form TYPE abap_bool,
      END OF tys_a_outb_delivery_header_t_2,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryHeaderTextType</p>
      tyt_a_outb_delivery_header_t_2 TYPE STANDARD TABLE OF tys_a_outb_delivery_header_t_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryItemTextType</p>
      BEGIN OF tys_a_outb_delivery_item_tex_2,
        "! <em>Key property</em> DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! <em>Key property</em> DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! <em>Key property</em> TextElement
        text_element               TYPE c LENGTH 4,
        "! <em>Key property</em> Language
        language                   TYPE c LENGTH 2,
        "! TextElementDescription
        text_element_description   TYPE c LENGTH 30,
        "! TextElementText
        text_element_text          TYPE string,
        "! DeliveryLongTextIsFormatted
        delivery_long_text_is_form TYPE abap_bool,
      END OF tys_a_outb_delivery_item_tex_2,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryItemTextType</p>
      tyt_a_outb_delivery_item_tex_2 TYPE STANDARD TABLE OF tys_a_outb_delivery_item_tex_2 WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryItemType</p>
      BEGIN OF tys_a_outb_delivery_item_type,
        "! ActualDeliveredQtyInBaseUnit
        actual_delivered_qty_in_ba TYPE p LENGTH 7 DECIMALS 3,
        "! ActualDeliveryQuantity
        actual_delivery_quantity   TYPE p LENGTH 7 DECIMALS 3,
        "! AdditionalCustomerGroup1
        additional_customer_group  TYPE c LENGTH 3,
        "! AdditionalCustomerGroup2
        additional_customer_grou_2 TYPE c LENGTH 3,
        "! AdditionalCustomerGroup3
        additional_customer_grou_3 TYPE c LENGTH 3,
        "! AdditionalCustomerGroup4
        additional_customer_grou_4 TYPE c LENGTH 3,
        "! AdditionalCustomerGroup5
        additional_customer_grou_5 TYPE c LENGTH 3,
        "! AdditionalMaterialGroup1
        additional_material_group  TYPE c LENGTH 3,
        "! AdditionalMaterialGroup2
        additional_material_grou_2 TYPE c LENGTH 3,
        "! AdditionalMaterialGroup3
        additional_material_grou_3 TYPE c LENGTH 3,
        "! AdditionalMaterialGroup4
        additional_material_grou_4 TYPE c LENGTH 3,
        "! AdditionalMaterialGroup5
        additional_material_grou_5 TYPE c LENGTH 3,
        "! AlternateProductNumber
        alternate_product_number   TYPE c LENGTH 40,
        "! BaseUnit
        base_unit                  TYPE c LENGTH 3,
        "! Batch
        batch                      TYPE c LENGTH 10,
        "! BatchBySupplier
        batch_by_supplier          TYPE c LENGTH 15,
        "! BatchClassification
        batch_classification       TYPE c LENGTH 18,
        "! BOMExplosion
        bomexplosion               TYPE c LENGTH 8,
        "! BusinessArea
        business_area              TYPE c LENGTH 4,
        "! ConsumptionPosting
        consumption_posting        TYPE c LENGTH 1,
        "! ControllingArea
        controlling_area           TYPE c LENGTH 4,
        "! CostCenter
        cost_center                TYPE c LENGTH 10,
        "! CreatedByUser
        created_by_user            TYPE c LENGTH 12,
        "! CreationDate
        creation_date              TYPE timestampl,
        "! CreationTime
        creation_time              TYPE timn,
        "! CustEngineeringChgStatus
        cust_engineering_chg_statu TYPE c LENGTH 17,
        "! <em>Key property</em> DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! <em>Key property</em> DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! DeliveryDocumentItemCategory
        delivery_document_item_cat TYPE c LENGTH 4,
        "! DeliveryDocumentItemText
        delivery_document_item_tex TYPE c LENGTH 40,
        "! DeliveryGroup
        delivery_group             TYPE c LENGTH 3,
        "! DeliveryQuantityUnit
        delivery_quantity_unit     TYPE c LENGTH 3,
        "! DeliveryRelatedBillingStatus
        delivery_related_billing_s TYPE c LENGTH 1,
        "! DeliveryToBaseQuantityDnmntr
        delivery_to_base_quantity  TYPE p LENGTH 3 DECIMALS 0,
        "! DeliveryToBaseQuantityNmrtr
        delivery_to_base_quantit_2 TYPE p LENGTH 3 DECIMALS 0,
        "! DeliveryVersion
        delivery_version           TYPE c LENGTH 4,
        "! DepartmentClassificationByCust
        department_classification  TYPE c LENGTH 4,
        "! DistributionChannel
        distribution_channel       TYPE c LENGTH 2,
        "! Division
        division                   TYPE c LENGTH 2,
        "! EUDeliveryItemARCStatus
        eudelivery_item_arcstatus  TYPE c LENGTH 1,
        "! FixedShipgProcgDurationInDays
        fixed_shipg_procg_duration TYPE p LENGTH 3 DECIMALS 2,
        "! GLAccount
        glaccount                  TYPE c LENGTH 10,
        "! GoodsMovementReasonCode
        goods_movement_reason_code TYPE c LENGTH 4,
        "! GoodsMovementStatus
        goods_movement_status      TYPE c LENGTH 1,
        "! GoodsMovementType
        goods_movement_type        TYPE c LENGTH 3,
        "! HigherLvlItmOfBatSpltItm
        higher_lvl_itm_of_bat_splt TYPE c LENGTH 6,
        "! HigherLevelItem
        higher_level_item          TYPE c LENGTH 6,
        "! InspectionLot
        inspection_lot             TYPE c LENGTH 12,
        "! InspectionPartialLot
        inspection_partial_lot     TYPE c LENGTH 6,
        "! IntercompanyBillingStatus
        intercompany_billing_statu TYPE c LENGTH 1,
        "! InternationalArticleNumber
        international_article_numb TYPE c LENGTH 18,
        "! InventorySpecialStockType
        inventory_special_stock_ty TYPE c LENGTH 1,
        "! InventoryValuationType
        inventory_valuation_type   TYPE c LENGTH 10,
        "! IsCompletelyDelivered
        is_completely_delivered    TYPE abap_bool,
        "! IsNotGoodsMovementsRelevant
        is_not_goods_movements_rel TYPE c LENGTH 1,
        "! IsSeparateValuation
        is_separate_valuation      TYPE abap_bool,
        "! IssgOrRcvgBatch
        issg_or_rcvg_batch         TYPE c LENGTH 10,
        "! IssgOrRcvgMaterial
        issg_or_rcvg_material      TYPE c LENGTH 40,
        "! IssgOrRcvgSpclStockInd
        issg_or_rcvg_spcl_stock_in TYPE c LENGTH 1,
        "! IssgOrRcvgStockCategory
        issg_or_rcvg_stock_categor TYPE c LENGTH 1,
        "! IssgOrRcvgValuationType
        issg_or_rcvg_valuation_typ TYPE c LENGTH 10,
        "! IssuingOrReceivingPlant
        issuing_or_receiving_plant TYPE c LENGTH 4,
        "! IssuingOrReceivingStorageLoc
        issuing_or_receiving_stora TYPE c LENGTH 4,
        "! ItemBillingBlockReason
        item_billing_block_reason  TYPE c LENGTH 2,
        "! ItemBillingIncompletionStatus
        item_billing_incompletion  TYPE c LENGTH 1,
        "! ItemDeliveryIncompletionStatus
        item_delivery_incompletion TYPE c LENGTH 1,
        "! ItemGdsMvtIncompletionSts
        item_gds_mvt_incompletion  TYPE c LENGTH 1,
        "! ItemGeneralIncompletionStatus
        item_general_incompletion  TYPE c LENGTH 1,
        "! ItemGrossWeight
        item_gross_weight          TYPE p LENGTH 8 DECIMALS 3,
        "! ItemIsBillingRelevant
        item_is_billing_relevant   TYPE c LENGTH 1,
        "! ItemNetWeight
        item_net_weight            TYPE p LENGTH 8 DECIMALS 3,
        "! ItemPackingIncompletionStatus
        item_packing_incompletion  TYPE c LENGTH 1,
        "! ItemPickingIncompletionStatus
        item_picking_incompletion  TYPE c LENGTH 1,
        "! ItemVolume
        item_volume                TYPE p LENGTH 8 DECIMALS 3,
        "! ItemVolumeUnit
        item_volume_unit           TYPE c LENGTH 3,
        "! ItemWeightUnit
        item_weight_unit           TYPE c LENGTH 3,
        "! LastChangeDate
        last_change_date           TYPE timestampl,
        "! LoadingGroup
        loading_group              TYPE c LENGTH 4,
        "! ManufactureDate
        manufacture_date           TYPE timestampl,
        "! Material
        material                   TYPE c LENGTH 40,
        "! MaterialByCustomer
        material_by_customer       TYPE c LENGTH 35,
        "! MaterialFreightGroup
        material_freight_group     TYPE c LENGTH 8,
        "! MaterialGroup
        material_group             TYPE c LENGTH 9,
        "! MaterialIsBatchManaged
        material_is_batch_managed  TYPE abap_bool,
        "! MaterialIsIntBatchManaged
        material_is_int_batch_mana TYPE abap_bool,
        "! NumberOfSerialNumbers
        number_of_serial_numbers   TYPE int4,
        "! OrderID
        order_id                   TYPE c LENGTH 12,
        "! OrderItem
        order_item                 TYPE c LENGTH 4,
        "! OriginalDeliveryQuantity
        original_delivery_quantity TYPE p LENGTH 7 DECIMALS 3,
        "! OriginallyRequestedMaterial
        originally_requested_mater TYPE c LENGTH 40,
        "! OverdelivTolrtdLmtRatioInPct
        overdeliv_tolrtd_lmt_ratio TYPE p LENGTH 2 DECIMALS 1,
        "! PackingStatus
        packing_status             TYPE c LENGTH 1,
        "! PartialDeliveryIsAllowed
        partial_delivery_is_allowe TYPE c LENGTH 1,
        "! PaymentGuaranteeForm
        payment_guarantee_form     TYPE c LENGTH 2,
        "! PickingConfirmationStatus
        picking_confirmation_statu TYPE c LENGTH 1,
        "! PickingControl
        picking_control            TYPE c LENGTH 1,
        "! PickingStatus
        picking_status             TYPE c LENGTH 1,
        "! Plant
        plant                      TYPE c LENGTH 4,
        "! PrimaryPostingSwitch
        primary_posting_switch     TYPE c LENGTH 1,
        "! ProductAvailabilityDate
        product_availability_date  TYPE timestampl,
        "! ProductAvailabilityTime
        product_availability_time  TYPE timn,
        "! ProductConfiguration
        product_configuration      TYPE c LENGTH 18,
        "! ProductHierarchyNode
        product_hierarchy_node     TYPE c LENGTH 18,
        "! ProfitabilitySegment
        profitability_segment      TYPE c LENGTH 10,
        "! ProfitCenter
        profit_center              TYPE c LENGTH 10,
        "! ProofOfDeliveryRelevanceCode
        proof_of_delivery_relevanc TYPE c LENGTH 1,
        "! ProofOfDeliveryStatus
        proof_of_delivery_status   TYPE c LENGTH 1,
        "! QuantityIsFixed
        quantity_is_fixed          TYPE abap_bool,
        "! ReceivingPoint
        receiving_point            TYPE c LENGTH 25,
        "! ReferenceDocumentLogicalSystem
        reference_document_logical TYPE c LENGTH 10,
        "! ReferenceSDDocument
        reference_sddocument       TYPE c LENGTH 10,
        "! ReferenceSDDocumentCategory
        reference_sddocument_categ TYPE c LENGTH 4,
        "! ReferenceSDDocumentItem
        reference_sddocument_item  TYPE c LENGTH 6,
        "! RetailPromotion
        retail_promotion           TYPE c LENGTH 10,
        "! SalesDocumentItemType
        sales_document_item_type   TYPE c LENGTH 1,
        "! SalesGroup
        sales_group                TYPE c LENGTH 3,
        "! SalesOffice
        sales_office               TYPE c LENGTH 4,
        "! SDDocumentCategory
        sddocument_category        TYPE c LENGTH 4,
        "! SDProcessStatus
        sdprocess_status           TYPE c LENGTH 1,
        "! ShelfLifeExpirationDate
        shelf_life_expiration_date TYPE timestampl,
        "! StatisticsDate
        statistics_date            TYPE timestampl,
        "! StockType
        stock_type                 TYPE c LENGTH 1,
        "! StorageBin
        storage_bin                TYPE c LENGTH 10,
        "! StorageLocation
        storage_location           TYPE c LENGTH 4,
        "! StorageType
        storage_type               TYPE c LENGTH 3,
        "! SubsequentMovementType
        subsequent_movement_type   TYPE c LENGTH 3,
        "! TransportationGroup
        transportation_group       TYPE c LENGTH 4,
        "! UnderdelivTolrtdLmtRatioInPct
        underdeliv_tolrtd_lmt_rati TYPE p LENGTH 2 DECIMALS 1,
        "! UnlimitedOverdeliveryIsAllowed
        unlimited_overdelivery_is  TYPE abap_bool,
        "! VarblShipgProcgDurationInDays
        varbl_shipg_procg_duration TYPE p LENGTH 3 DECIMALS 2,
        "! Warehouse
        warehouse                  TYPE c LENGTH 3,
        "! WarehouseActivityStatus
        warehouse_activity_status  TYPE c LENGTH 1,
        "! WarehouseStagingArea
        warehouse_staging_area     TYPE c LENGTH 10,
        "! WarehouseStockCategory
        warehouse_stock_category   TYPE c LENGTH 1,
        "! WarehouseStorageBin
        warehouse_storage_bin      TYPE c LENGTH 10,
        "! odata.etag
        etag                       TYPE string,
      END OF tys_a_outb_delivery_item_type,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryItemType</p>
      tyt_a_outb_delivery_item_type TYPE STANDARD TABLE OF tys_a_outb_delivery_item_type WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_OutbDeliveryPartnerType</p>
      BEGIN OF tys_a_outb_delivery_partner_ty,
        "! AddressID
        address_id                 TYPE c LENGTH 10,
        "! BusinessPartnerAddressUUID
        business_partner_address_u TYPE sysuuid_x16,
        "! RefBusinessPartnerAddressUUID
        ref_business_partner_addre TYPE sysuuid_x16,
        "! ContactPerson
        contact_person             TYPE c LENGTH 10,
        "! Customer
        customer                   TYPE c LENGTH 10,
        "! <em>Key property</em> PartnerFunction
        partner_function           TYPE c LENGTH 2,
        "! Personnel
        personnel                  TYPE c LENGTH 8,
        "! <em>Key property</em> SDDocument
        sddocument                 TYPE c LENGTH 10,
        "! SDDocumentItem
        sddocument_item            TYPE c LENGTH 6,
        "! Supplier
        supplier                   TYPE c LENGTH 10,
      END OF tys_a_outb_delivery_partner_ty,
      "! <p class="shorttext synchronized">List of A_OutbDeliveryPartnerType</p>
      tyt_a_outb_delivery_partner_ty TYPE STANDARD TABLE OF tys_a_outb_delivery_partner_ty WITH DEFAULT KEY.

    TYPES:
      "! <p class="shorttext synchronized">A_SerialNmbrDeliveryType</p>
      BEGIN OF tys_a_serial_nmbr_delivery_typ,
        "! DeliveryDate
        delivery_date              TYPE timestampl,
        "! DeliveryDocument
        delivery_document          TYPE c LENGTH 10,
        "! DeliveryDocumentItem
        delivery_document_item     TYPE c LENGTH 6,
        "! <em>Key property</em> MaintenanceItemObjectList
        maintenance_item_object_li TYPE int8,
        "! SDDocumentCategory
        sddocument_category        TYPE c LENGTH 4,
      END OF tys_a_serial_nmbr_delivery_typ,
      "! <p class="shorttext synchronized">List of A_SerialNmbrDeliveryType</p>
      tyt_a_serial_nmbr_delivery_typ TYPE STANDARD TABLE OF tys_a_serial_nmbr_delivery_typ WITH DEFAULT KEY.


    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the entity sets</p>
      BEGIN OF gcs_entity_set,
        "! A_HandlingUnitHeaderDelivery
        "! <br/> Collection of type 'A_HandlingUnitHeaderDeliveryType'
        a_handling_unit_header_del TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_HANDLING_UNIT_HEADER_DEL',
        "! A_HandlingUnitItemDelivery
        "! <br/> Collection of type 'A_HandlingUnitItemDeliveryType'
        a_handling_unit_item_deliv TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_HANDLING_UNIT_ITEM_DELIV',
        "! A_MaintenanceItemObject
        "! <br/> Collection of type 'A_MaintenanceItemObjectType'
        a_maintenance_item_object  TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_MAINTENANCE_ITEM_OBJECT',
        "! A_OutbDeliveryAddress
        "! <br/> Collection of type 'A_OutbDeliveryAddressType'
        a_outb_delivery_address    TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_ADDRESS',
        "! A_OutbDeliveryAddress2
        "! <br/> Collection of type 'A_OutbDeliveryAddress2Type'
        a_outb_delivery_address_2  TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_ADDRESS_2',
        "! A_OutbDeliveryDocFlow
        "! <br/> Collection of type 'A_OutbDeliveryDocFlowType'
        a_outb_delivery_doc_flow   TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_DOC_FLOW',
        "! A_OutbDeliveryHeader
        "! <br/> Collection of type 'A_OutbDeliveryHeaderType'
        a_outb_delivery_header     TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_HEADER',
        "! A_OutbDeliveryHeaderText
        "! <br/> Collection of type 'A_OutbDeliveryHeaderTextType'
        a_outb_delivery_header_tex TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_HEADER_TEX',
        "! A_OutbDeliveryItem
        "! <br/> Collection of type 'A_OutbDeliveryItemType'
        a_outb_delivery_item       TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_ITEM',
        "! A_OutbDeliveryItemText
        "! <br/> Collection of type 'A_OutbDeliveryItemTextType'
        a_outb_delivery_item_text  TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_ITEM_TEXT',
        "! A_OutbDeliveryPartner
        "! <br/> Collection of type 'A_OutbDeliveryPartnerType'
        a_outb_delivery_partner    TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_OUTB_DELIVERY_PARTNER',
        "! A_SerialNmbrDelivery
        "! <br/> Collection of type 'A_SerialNmbrDeliveryType'
        a_serial_nmbr_delivery     TYPE /iwbep/if_cp_runtime_types=>ty_entity_set_name VALUE 'A_SERIAL_NMBR_DELIVERY',
      END OF gcs_entity_set .

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the function imports</p>
      BEGIN OF gcs_function_import,
        "! AddSerialNumberToDeliveryItem
        "! <br/> See structure type {@link ..tys_parameters_1} for the parameters
        add_serial_number_to_deliv TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'ADD_SERIAL_NUMBER_TO_DELIV',
        "! AssignHandlingUnitToDelivery
        "! <br/> See structure type {@link ..tys_parameters_9} for the parameters
        assign_handling_unit_to_de TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'ASSIGN_HANDLING_UNIT_TO_DE',
        "! ConfirmPickingAllItems
        "! <br/> See structure type {@link ..tys_parameters_10} for the parameters
        confirm_picking_all_items  TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'CONFIRM_PICKING_ALL_ITEMS',
        "! ConfirmPickingOneItem
        "! <br/> See structure type {@link ..tys_parameters_11} for the parameters
        confirm_picking_one_item   TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'CONFIRM_PICKING_ONE_ITEM',
        "! CreateBatchSplitItem
        "! <br/> See structure type {@link ..tys_parameters_12} for the parameters
        create_batch_split_item    TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'CREATE_BATCH_SPLIT_ITEM',
        "! DeleteAllHandlingUnitsFromDelivery
        "! <br/> See structure type {@link ..tys_parameters_13} for the parameters
        delete_all_handling_units  TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'DELETE_ALL_HANDLING_UNITS',
        "! DeleteAllSerialNumbersFromDeliveryItem
        "! <br/> See structure type {@link ..tys_parameters_14} for the parameters
        delete_all_serial_numbers  TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'DELETE_ALL_SERIAL_NUMBERS',
        "! DeleteSerialNumberFromDeliveryItem
        "! <br/> See structure type {@link ..tys_parameters_15} for the parameters
        delete_serial_number_from  TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'DELETE_SERIAL_NUMBER_FROM',
        "! PickAllItems
        "! <br/> See structure type {@link ..tys_parameters_16} for the parameters
        pick_all_items             TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'PICK_ALL_ITEMS',
        "! PickAndBatchSplitOneItem
        "! <br/> See structure type {@link ..tys_parameters_2} for the parameters
        pick_and_batch_split_one_i TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'PICK_AND_BATCH_SPLIT_ONE_I',
        "! PickOneItem
        "! <br/> See structure type {@link ..tys_parameters_3} for the parameters
        pick_one_item              TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'PICK_ONE_ITEM',
        "! PickOneItemWithBaseQuantity
        "! <br/> See structure type {@link ..tys_parameters_4} for the parameters
        pick_one_item_with_base_qu TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'PICK_ONE_ITEM_WITH_BASE_QU',
        "! PickOneItemWithSalesQuantity
        "! <br/> See structure type {@link ..tys_parameters_5} for the parameters
        pick_one_item_with_sales_q TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'PICK_ONE_ITEM_WITH_SALES_Q',
        "! PostGoodsIssue
        "! <br/> See structure type {@link ..tys_parameters_6} for the parameters
        post_goods_issue           TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'POST_GOODS_ISSUE',
        "! ReverseGoodsIssue
        "! <br/> See structure type {@link ..tys_parameters_7} for the parameters
        reverse_goods_issue        TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'REVERSE_GOODS_ISSUE',
        "! SetPickingQuantityWithBaseQuantity
        "! <br/> See structure type {@link ..tys_parameters_8} for the parameters
        set_picking_quantity_with  TYPE /iwbep/if_cp_runtime_types=>ty_operation_name VALUE 'SET_PICKING_QUANTITY_WITH',
      END OF gcs_function_import.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal Names of the bound functions</p>
      BEGIN OF gcs_bound_function,
         "! Dummy field - Structure must not be empty
         dummy TYPE int1 VALUE 0,
      END OF gcs_bound_function.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal names for complex types</p>
      BEGIN OF gcs_complex_type,
        "! <p class="shorttext synchronized">Internal names for CreatedDeliveryItem</p>
        "! See also structure type {@link ..tys_created_delivery_item}
        BEGIN OF created_delivery_item,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF created_delivery_item,
        "! <p class="shorttext synchronized">Internal names for HuReturn</p>
        "! See also structure type {@link ..tys_hu_return}
        BEGIN OF hu_return,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF hu_return,
        "! <p class="shorttext synchronized">Internal names for PickingReport</p>
        "! See also structure type {@link ..tys_picking_report}
        BEGIN OF picking_report,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF picking_report,
        "! <p class="shorttext synchronized">Internal names for Return</p>
        "! See also structure type {@link ..tys_return}
        BEGIN OF return,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF return,
      END OF gcs_complex_type.

    CONSTANTS:
      "! <p class="shorttext synchronized">Internal names for entity types</p>
      BEGIN OF gcs_entity_type,
        "! <p class="shorttext synchronized">Internal names for A_HandlingUnitHeaderDeliveryType</p>
        "! See also structure type {@link ..tys_a_handling_unit_header_d_2}
        BEGIN OF a_handling_unit_header_d_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_HandlingUnitItemDelivery
            to_handling_unit_item_deli TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_HANDLING_UNIT_ITEM_DELI',
          END OF navigation,
        END OF a_handling_unit_header_d_2,
        "! <p class="shorttext synchronized">Internal names for A_HandlingUnitItemDeliveryType</p>
        "! See also structure type {@link ..tys_a_handling_unit_item_del_2}
        BEGIN OF a_handling_unit_item_del_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_handling_unit_item_del_2,
        "! <p class="shorttext synchronized">Internal names for A_MaintenanceItemObjectType</p>
        "! See also structure type {@link ..tys_a_maintenance_item_objec_2}
        BEGIN OF a_maintenance_item_objec_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_maintenance_item_objec_2,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryAddressType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_address_ty}
        BEGIN OF a_outb_delivery_address_ty,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_outb_delivery_address_ty,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryAddress2Type</p>
        "! See also structure type {@link ..tys_a_outb_delivery_address__2}
        BEGIN OF a_outb_delivery_address__2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_outb_delivery_address__2,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryDocFlowType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_doc_flow_t}
        BEGIN OF a_outb_delivery_doc_flow_t,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_outb_delivery_doc_flow_t,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryHeaderType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_header_typ}
        BEGIN OF a_outb_delivery_header_typ,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_DeliveryDocumentItem
            to_delivery_document_item  TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_DELIVERY_DOCUMENT_ITEM',
            "! to_DeliveryDocumentPartner
            to_delivery_document_partn TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_DELIVERY_DOCUMENT_PARTN',
            "! to_DeliveryDocumentText
            to_delivery_document_text  TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_DELIVERY_DOCUMENT_TEXT',
            "! to_HandlingUnitHeaderDelivery
            to_handling_unit_header_de TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_HANDLING_UNIT_HEADER_DE',
          END OF navigation,
        END OF a_outb_delivery_header_typ,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryHeaderTextType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_header_t_2}
        BEGIN OF a_outb_delivery_header_t_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_outb_delivery_header_t_2,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryItemTextType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_item_tex_2}
        BEGIN OF a_outb_delivery_item_tex_2,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! Dummy field - Structure must not be empty
            dummy TYPE int1 VALUE 0,
          END OF navigation,
        END OF a_outb_delivery_item_tex_2,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryItemType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_item_type}
        BEGIN OF a_outb_delivery_item_type,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_DeliveryDocumentItemText
            to_delivery_document_item  TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_DELIVERY_DOCUMENT_ITEM',
            "! to_DocumentFlow
            to_document_flow           TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_DOCUMENT_FLOW',
            "! to_HandlingUnitItemDelivery
            to_handling_unit_item_deli TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_HANDLING_UNIT_ITEM_DELI',
            "! to_SerialDeliveryItem
            to_serial_delivery_item    TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_SERIAL_DELIVERY_ITEM',
          END OF navigation,
        END OF a_outb_delivery_item_type,
        "! <p class="shorttext synchronized">Internal names for A_OutbDeliveryPartnerType</p>
        "! See also structure type {@link ..tys_a_outb_delivery_partner_ty}
        BEGIN OF a_outb_delivery_partner_ty,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_Address
            to_address   TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_ADDRESS',
            "! to_Address2
            to_address_2 TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_ADDRESS_2',
          END OF navigation,
        END OF a_outb_delivery_partner_ty,
        "! <p class="shorttext synchronized">Internal names for A_SerialNmbrDeliveryType</p>
        "! See also structure type {@link ..tys_a_serial_nmbr_delivery_typ}
        BEGIN OF a_serial_nmbr_delivery_typ,
          "! <p class="shorttext synchronized">Navigation properties</p>
          BEGIN OF navigation,
            "! to_MaintenanceItemObject
            to_maintenance_item_object TYPE /iwbep/if_v4_pm_types=>ty_internal_name VALUE 'TO_MAINTENANCE_ITEM_OBJECT',
          END OF navigation,
        END OF a_serial_nmbr_delivery_typ,
      END OF gcs_entity_type.


    METHODS /iwbep/if_v4_mp_basic_pm~define REDEFINITION.


  PRIVATE SECTION.

    "! <p class="shorttext synchronized">Model</p>
    DATA mo_model TYPE REF TO /iwbep/if_v4_pm_model.


    "! <p class="shorttext synchronized">Define CreatedDeliveryItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_created_delivery_item RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define HuReturn</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_hu_return RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PickingReport</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_picking_report RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define Return</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_return RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_HandlingUnitHeaderDeliveryType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_handling_unit_header_d_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_HandlingUnitItemDeliveryType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_handling_unit_item_del_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_MaintenanceItemObjectType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_maintenance_item_objec_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryAddressType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_address_ty RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryAddress2Type</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_address__2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryDocFlowType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_doc_flow_t RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryHeaderType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_header_typ RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryHeaderTextType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_header_t_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryItemTextType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_item_tex_2 RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryItemType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_item_type RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_OutbDeliveryPartnerType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_outb_delivery_partner_ty RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define A_SerialNmbrDeliveryType</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_a_serial_nmbr_delivery_typ RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define AddSerialNumberToDeliveryItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_add_serial_number_to_deliv RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define AssignHandlingUnitToDelivery</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_assign_handling_unit_to_de RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define ConfirmPickingAllItems</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_confirm_picking_all_items RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define ConfirmPickingOneItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_confirm_picking_one_item RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define CreateBatchSplitItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_create_batch_split_item RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define DeleteAllHandlingUnitsFromDelivery</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_delete_all_handling_units RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define DeleteAllSerialNumbersFromDeliveryItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_delete_all_serial_numbers RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define DeleteSerialNumberFromDeliveryItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_delete_serial_number_from RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PickAllItems</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_pick_all_items RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PickAndBatchSplitOneItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_pick_and_batch_split_one_i RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PickOneItem</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_pick_one_item RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PickOneItemWithBaseQuantity</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_pick_one_item_with_base_qu RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PickOneItemWithSalesQuantity</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_pick_one_item_with_sales_q RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define PostGoodsIssue</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_post_goods_issue RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define ReverseGoodsIssue</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_reverse_goods_issue RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define SetPickingQuantityWithBaseQuantity</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS def_set_picking_quantity_with RAISING /iwbep/cx_gateway.

    "! <p class="shorttext synchronized">Define all primitive types</p>
    "! @raising /iwbep/cx_gateway | <p class="shorttext synchronized">Gateway Exception</p>
    METHODS define_primitive_types RAISING /iwbep/cx_gateway.

ENDCLASS.


CLASS ztestdo IMPLEMENTATION.

  METHOD /iwbep/if_v4_mp_basic_pm~define.

    mo_model = io_model.
    mo_model->set_schema_namespace( 'API_OUTBOUND_DELIVERY_SRV' ) ##NO_TEXT.

    def_created_delivery_item( ).
    def_hu_return( ).
    def_picking_report( ).
    def_return( ).
    def_a_handling_unit_header_d_2( ).
    def_a_handling_unit_item_del_2( ).
    def_a_maintenance_item_objec_2( ).
    def_a_outb_delivery_address_ty( ).
    def_a_outb_delivery_address__2( ).
    def_a_outb_delivery_doc_flow_t( ).
    def_a_outb_delivery_header_typ( ).
    def_a_outb_delivery_header_t_2( ).
    def_a_outb_delivery_item_tex_2( ).
    def_a_outb_delivery_item_type( ).
    def_a_outb_delivery_partner_ty( ).
    def_a_serial_nmbr_delivery_typ( ).
    def_add_serial_number_to_deliv( ).
    def_assign_handling_unit_to_de( ).
    def_confirm_picking_all_items( ).
    def_confirm_picking_one_item( ).
    def_create_batch_split_item( ).
    def_delete_all_handling_units( ).
    def_delete_all_serial_numbers( ).
    def_delete_serial_number_from( ).
    def_pick_all_items( ).
    def_pick_and_batch_split_one_i( ).
    def_pick_one_item( ).
    def_pick_one_item_with_base_qu( ).
    def_pick_one_item_with_sales_q( ).
    def_post_goods_issue( ).
    def_reverse_goods_issue( ).
    def_set_picking_quantity_with( ).
    define_primitive_types( ).

  ENDMETHOD.


  METHOD def_created_delivery_item.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_complex_type        TYPE REF TO /iwbep/if_v4_pm_cplx_type,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_complex_type = mo_model->create_complex_type_by_struct(
                                    iv_complex_type_name      = 'CREATED_DELIVERY_ITEM'
                                    is_structure              = VALUE tys_created_delivery_item( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_complex_type->set_edm_name( 'CreatedDeliveryItem' ) ##NO_TEXT.


    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

  ENDMETHOD.


  METHOD def_hu_return.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_complex_type        TYPE REF TO /iwbep/if_v4_pm_cplx_type,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_complex_type = mo_model->create_complex_type_by_struct(
                                    iv_complex_type_name      = 'HU_RETURN'
                                    is_structure              = VALUE tys_hu_return( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_complex_type->set_edm_name( 'HuReturn' ) ##NO_TEXT.


    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_IDENTIFICAT' ).
    lo_primitive_property->set_edm_name( 'SystemMessageIdentification' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'SystemMessageNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_TYPE' ).
    lo_primitive_property->set_edm_name( 'SystemMessageType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_1' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_2' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_3' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_4' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

  ENDMETHOD.


  METHOD def_picking_report.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_complex_type        TYPE REF TO /iwbep/if_v4_pm_cplx_type,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_complex_type = mo_model->create_complex_type_by_struct(
                                    iv_complex_type_name      = 'PICKING_REPORT'
                                    is_structure              = VALUE tys_picking_report( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_complex_type->set_edm_name( 'PickingReport' ) ##NO_TEXT.


    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_IDENTIFICAT' ).
    lo_primitive_property->set_edm_name( 'SystemMessageIdentification' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'SystemMessageNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_TYPE' ).
    lo_primitive_property->set_edm_name( 'SystemMessageType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_1' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_2' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_3' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_4' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'BATCH' ).
    lo_primitive_property->set_edm_name( 'Batch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_QUANTITY_UNIT' ).
    lo_primitive_property->set_edm_name( 'DeliveryQuantityUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'ACTUAL_DELIVERY_QUANTITY' ).
    lo_primitive_property->set_edm_name( 'ActualDeliveryQuantity' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM_TEX' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItemText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'MATERIAL' ).
    lo_primitive_property->set_edm_name( 'Material' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

  ENDMETHOD.


  METHOD def_return.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_complex_type        TYPE REF TO /iwbep/if_v4_pm_cplx_type,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_complex_type = mo_model->create_complex_type_by_struct(
                                    iv_complex_type_name      = 'RETURN'
                                    is_structure              = VALUE tys_return( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_complex_type->set_edm_name( 'Return' ) ##NO_TEXT.


    lo_primitive_property = lo_complex_type->get_primitive_property( 'COLLECTIVE_PROCESSING' ).
    lo_primitive_property->set_edm_name( 'CollectiveProcessing' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SCHEDULE_LINE' ).
    lo_primitive_property->set_edm_name( 'ScheduleLine' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'COLLECTIVE_PROCESSING_MSG' ).
    lo_primitive_property->set_edm_name( 'CollectiveProcessingMsgCounter' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_IDENTIFICAT' ).
    lo_primitive_property->set_edm_name( 'SystemMessageIdentification' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'SystemMessageNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_TYPE' ).
    lo_primitive_property->set_edm_name( 'SystemMessageType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_1' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_2' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_3' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'SYSTEM_MESSAGE_VARIABLE_4' ).
    lo_primitive_property->set_edm_name( 'SystemMessageVariable4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 50 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_complex_type->get_primitive_property( 'COLLECTIVE_PROCESSING_TYPE' ).
    lo_primitive_property->set_edm_name( 'CollectiveProcessingType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

  ENDMETHOD.


  METHOD def_a_handling_unit_header_d_2.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_HANDLING_UNIT_HEADER_D_2'
                                    is_structure              = VALUE tys_a_handling_unit_header_d_2( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_HandlingUnitHeaderDeliveryType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_HANDLING_UNIT_HEADER_DEL' ).
    lo_entity_set->set_edm_name( 'A_HandlingUnitHeaderDelivery' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATED_BY_USER' ).
    lo_primitive_property->set_edm_name( 'CreatedByUser' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATION_DATE' ).
    lo_primitive_property->set_edm_name( 'CreationDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATION_TIME' ).
    lo_primitive_property->set_edm_name( 'CreationTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GROSS_VOLUME' ).
    lo_primitive_property->set_edm_name( 'GrossVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GROSS_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'GrossWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_BASE_UNIT_OF' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitBaseUnitOfMeasure' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_CONTENT_DESC' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitContentDescription' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_EXTERNAL_ID' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitExternalId' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_EXTERNAL_I_2' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitExternalIdType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_HEIGHT' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitHeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_INTERNAL_ID' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitInternalId' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_INTERNAL_STA' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitInternalStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_LENGTH' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitLength' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_LOWER_LEVEL' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitLowerLevelRefer' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_MAX_VOLUME' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitMaxVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_MAX_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitMaxWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_NET_VOLUME' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitNetVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_SECOND_EXTER' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitSecondExternalId' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_TARE_VOLUME' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitTareVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_TARE_VOLUM_2' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitTareVolumeUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_TARE_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitTareWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_TARE_WEIGH_2' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitTareWeightUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_UO_MDIMENSIO' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitUoMDimension' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_WIDTH' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitWidth' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LAST_CHANGE_DATE' ).
    lo_primitive_property->set_edm_name( 'LastChangeDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LAST_CHANGED_BY_USER' ).
    lo_primitive_property->set_edm_name( 'LastChangedByUser' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LAST_CHANGE_TIME' ).
    lo_primitive_property->set_edm_name( 'LastChangeTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'NET_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'NetWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PACKAGING_MATERIAL' ).
    lo_primitive_property->set_edm_name( 'PackagingMaterial' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PACKAGING_MATERIAL_CATEGOR' ).
    lo_primitive_property->set_edm_name( 'PackagingMaterialCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PACKAGING_MATERIAL_GROUP' ).
    lo_primitive_property->set_edm_name( 'PackagingMaterialGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PACKAGING_MATERIAL_TYPE' ).
    lo_primitive_property->set_edm_name( 'PackagingMaterialType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PACKING_INSTRUCTION_NUMBER' ).
    lo_primitive_property->set_edm_name( 'PackingInstructionNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIPPING_POINT' ).
    lo_primitive_property->set_edm_name( 'ShippingPoint' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'VOLUME_UNIT' ).
    lo_primitive_property->set_edm_name( 'VolumeUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WEIGHT_UNIT' ).
    lo_primitive_property->set_edm_name( 'WeightUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_HANDLING_UNIT_ITEM_DELI' ).
    lo_navigation_property->set_edm_name( 'to_HandlingUnitItemDelivery' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_HANDLING_UNIT_ITEM_DEL_2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

  ENDMETHOD.


  METHOD def_a_handling_unit_item_del_2.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_HANDLING_UNIT_ITEM_DEL_2'
                                    is_structure              = VALUE tys_a_handling_unit_item_del_2( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_HandlingUnitItemDeliveryType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_HANDLING_UNIT_ITEM_DELIV' ).
    lo_entity_set->set_edm_name( 'A_HandlingUnitItemDelivery' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'BATCH' ).
    lo_primitive_property->set_edm_name( 'Batch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_EXTERNAL_ID' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitExternalId' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_INTERNAL_ID' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitInternalId' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_ITEM' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_NESTED_INTER' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitNestedInternalId' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_NR_OF_AUX_PA' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitNrOfAuxPackgMat' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Int32' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_NUMBER_OF_SE' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitNumberOfSerialnumb' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Int32' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_QUANTITY' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitQuantity' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_QUANTITY_UNI' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitQuantityUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_TYPE_OF_CONT' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitTypeOfContent' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL' ).
    lo_primitive_property->set_edm_name( 'Material' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_BY_CUSTOMER' ).
    lo_primitive_property->set_edm_name( 'MaterialByCustomer' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 35 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_NAME' ).
    lo_primitive_property->set_edm_name( 'MaterialName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SERIAL_NUMBER_PROFILE' ).
    lo_primitive_property->set_edm_name( 'SerialNumberProfile' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

  ENDMETHOD.


  METHOD def_a_maintenance_item_objec_2.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_MAINTENANCE_ITEM_OBJEC_2'
                                    is_structure              = VALUE tys_a_maintenance_item_objec_2( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_MaintenanceItemObjectType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_MAINTENANCE_ITEM_OBJECT' ).
    lo_entity_set->set_edm_name( 'A_MaintenanceItemObject' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'ASSEMBLY' ).
    lo_primitive_property->set_edm_name( 'Assembly' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EQUIPMENT' ).
    lo_primitive_property->set_edm_name( 'Equipment' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FUNCTIONAL_LOCATION' ).
    lo_primitive_property->set_edm_name( 'FunctionalLocation' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MAINTENANCE_ITEM_OBJECT' ).
    lo_primitive_property->set_edm_name( 'MaintenanceItemObject' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Int32' ) ##NO_TEXT.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MAINTENANCE_ITEM_OBJECT_LI' ).
    lo_primitive_property->set_edm_name( 'MaintenanceItemObjectList' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Int64' ) ##NO_TEXT.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MAINTENANCE_NOTIFICATION' ).
    lo_primitive_property->set_edm_name( 'MaintenanceNotification' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MAINT_OBJECT_LOC_ACCT_ASSG' ).
    lo_primitive_property->set_edm_name( 'MaintObjectLocAcctAssgmtNmbr' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL' ).
    lo_primitive_property->set_edm_name( 'Material' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SERIAL_NUMBER' ).
    lo_primitive_property->set_edm_name( 'SerialNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.

  ENDMETHOD.


  METHOD def_a_outb_delivery_address_ty.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_ADDRESS_TY'
                                    is_structure              = VALUE tys_a_outb_delivery_address_ty( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryAddressType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_ADDRESS' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryAddress' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_STREET_PREFIX_N' ).
    lo_primitive_property->set_edm_name( 'AdditionalStreetPrefixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_STREET_SUFFIX_N' ).
    lo_primitive_property->set_edm_name( 'AdditionalStreetSuffixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDRESS_ID' ).
    lo_primitive_property->set_edm_name( 'AddressID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDRESS_TIME_ZONE' ).
    lo_primitive_property->set_edm_name( 'AddressTimeZone' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUILDING' ).
    lo_primitive_property->set_edm_name( 'Building' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_1' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_2' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_3' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_4' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CARE_OF_NAME' ).
    lo_primitive_property->set_edm_name( 'CareOfName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CITY_CODE' ).
    lo_primitive_property->set_edm_name( 'CityCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CITY_NAME' ).
    lo_primitive_property->set_edm_name( 'CityName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CITY_SEARCH' ).
    lo_primitive_property->set_edm_name( 'CitySearch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 25 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COMPANY_POSTAL_CODE' ).
    lo_primitive_property->set_edm_name( 'CompanyPostalCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CORRESPONDENCE_LANGUAGE' ).
    lo_primitive_property->set_edm_name( 'CorrespondenceLanguage' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COUNTRY' ).
    lo_primitive_property->set_edm_name( 'Country' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COUNTY' ).
    lo_primitive_property->set_edm_name( 'County' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_SERVICE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'DeliveryServiceNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_SERVICE_TYPE_CODE' ).
    lo_primitive_property->set_edm_name( 'DeliveryServiceTypeCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DISTRICT' ).
    lo_primitive_property->set_edm_name( 'District' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FAX_NUMBER' ).
    lo_primitive_property->set_edm_name( 'FaxNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FLOOR' ).
    lo_primitive_property->set_edm_name( 'Floor' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FORM_OF_ADDRESS' ).
    lo_primitive_property->set_edm_name( 'FormOfAddress' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FULL_NAME' ).
    lo_primitive_property->set_edm_name( 'FullName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 80 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HOME_CITY_NAME' ).
    lo_primitive_property->set_edm_name( 'HomeCityName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HOUSE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'HouseNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HOUSE_NUMBER_SUPPLEMENT_TE' ).
    lo_primitive_property->set_edm_name( 'HouseNumberSupplementText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'NATION' ).
    lo_primitive_property->set_edm_name( 'Nation' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PERSON' ).
    lo_primitive_property->set_edm_name( 'Person' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PHONE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'PhoneNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX' ).
    lo_primitive_property->set_edm_name( 'POBox' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_DEVIATING_CITY_NAME' ).
    lo_primitive_property->set_edm_name( 'POBoxDeviatingCityName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_DEVIATING_COUNTRY' ).
    lo_primitive_property->set_edm_name( 'POBoxDeviatingCountry' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_DEVIATING_REGION' ).
    lo_primitive_property->set_edm_name( 'POBoxDeviatingRegion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_IS_WITHOUT_NUMBER' ).
    lo_primitive_property->set_edm_name( 'POBoxIsWithoutNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_LOBBY_NAME' ).
    lo_primitive_property->set_edm_name( 'POBoxLobbyName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_POSTAL_CODE' ).
    lo_primitive_property->set_edm_name( 'POBoxPostalCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POSTAL_CODE' ).
    lo_primitive_property->set_edm_name( 'PostalCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRFRD_COMM_MEDIUM_TYPE' ).
    lo_primitive_property->set_edm_name( 'PrfrdCommMediumType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REGION' ).
    lo_primitive_property->set_edm_name( 'Region' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ROOM_NUMBER' ).
    lo_primitive_property->set_edm_name( 'RoomNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SEARCH_TERM_1' ).
    lo_primitive_property->set_edm_name( 'SearchTerm1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_NAME' ).
    lo_primitive_property->set_edm_name( 'StreetName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 60 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_PREFIX_NAME' ).
    lo_primitive_property->set_edm_name( 'StreetPrefixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_SEARCH' ).
    lo_primitive_property->set_edm_name( 'StreetSearch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 25 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_SUFFIX_NAME' ).
    lo_primitive_property->set_edm_name( 'StreetSuffixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TAX_JURISDICTION' ).
    lo_primitive_property->set_edm_name( 'TaxJurisdiction' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 15 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORT_ZONE' ).
    lo_primitive_property->set_edm_name( 'TransportZone' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

  ENDMETHOD.


  METHOD def_a_outb_delivery_address__2.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_ADDRESS__2'
                                    is_structure              = VALUE tys_a_outb_delivery_address__2( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryAddress2Type' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_ADDRESS_2' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryAddress2' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PARTNER_FUNCTION' ).
    lo_primitive_property->set_edm_name( 'PartnerFunction' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_VERSION' ).
    lo_primitive_property->set_edm_name( 'DeliveryVersion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_STREET_PREFIX_N' ).
    lo_primitive_property->set_edm_name( 'AdditionalStreetPrefixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDRESS_ID' ).
    lo_primitive_property->set_edm_name( 'AddressID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_STREET_SUFFIX_N' ).
    lo_primitive_property->set_edm_name( 'AdditionalStreetSuffixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDRESS_TIME_ZONE' ).
    lo_primitive_property->set_edm_name( 'AddressTimeZone' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUILDING' ).
    lo_primitive_property->set_edm_name( 'Building' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_1' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_2' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_3' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_NAME_4' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerName4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PERSON_FAMILY_NAME' ).
    lo_primitive_property->set_edm_name( 'PersonFamilyName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PERSON_GIVEN_NAME' ).
    lo_primitive_property->set_edm_name( 'PersonGivenName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CARE_OF_NAME' ).
    lo_primitive_property->set_edm_name( 'CareOfName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CITY_NAME' ).
    lo_primitive_property->set_edm_name( 'CityName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COMPANY_POSTAL_CODE' ).
    lo_primitive_property->set_edm_name( 'CompanyPostalCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CORRESPONDENCE_LANGUAGE' ).
    lo_primitive_property->set_edm_name( 'CorrespondenceLanguage' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COUNTRY' ).
    lo_primitive_property->set_edm_name( 'Country' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COUNTY' ).
    lo_primitive_property->set_edm_name( 'County' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_SERVICE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'DeliveryServiceNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_SERVICE_TYPE_CODE' ).
    lo_primitive_property->set_edm_name( 'DeliveryServiceTypeCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DISTRICT' ).
    lo_primitive_property->set_edm_name( 'District' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EMAIL_ADDRESS' ).
    lo_primitive_property->set_edm_name( 'EmailAddress' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 241 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FAX_NUMBER' ).
    lo_primitive_property->set_edm_name( 'FaxNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FAX_NUMBER_EXTENSION' ).
    lo_primitive_property->set_edm_name( 'FaxNumberExtension' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FLOOR' ).
    lo_primitive_property->set_edm_name( 'Floor' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FORM_OF_ADDRESS' ).
    lo_primitive_property->set_edm_name( 'FormOfAddress' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HOME_CITY_NAME' ).
    lo_primitive_property->set_edm_name( 'HomeCityName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HOUSE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'HouseNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HOUSE_NUMBER_SUPPLEMENT_TE' ).
    lo_primitive_property->set_edm_name( 'HouseNumberSupplementText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MOBILE_PHONE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'MobilePhoneNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PHONE_NUMBER' ).
    lo_primitive_property->set_edm_name( 'PhoneNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PHONE_NUMBER_EXTENSION' ).
    lo_primitive_property->set_edm_name( 'PhoneNumberExtension' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX' ).
    lo_primitive_property->set_edm_name( 'POBox' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_DEVIATING_CITY_NAME' ).
    lo_primitive_property->set_edm_name( 'POBoxDeviatingCityName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_DEVIATING_COUNTRY' ).
    lo_primitive_property->set_edm_name( 'POBoxDeviatingCountry' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_DEVIATING_REGION' ).
    lo_primitive_property->set_edm_name( 'POBoxDeviatingRegion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_IS_WITHOUT_NUMBER' ).
    lo_primitive_property->set_edm_name( 'POBoxIsWithoutNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_LOBBY_NAME' ).
    lo_primitive_property->set_edm_name( 'POBoxLobbyName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POBOX_POSTAL_CODE' ).
    lo_primitive_property->set_edm_name( 'POBoxPostalCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'POSTAL_CODE' ).
    lo_primitive_property->set_edm_name( 'PostalCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRFRD_COMM_MEDIUM_TYPE' ).
    lo_primitive_property->set_edm_name( 'PrfrdCommMediumType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REGION' ).
    lo_primitive_property->set_edm_name( 'Region' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ROOM_NUMBER' ).
    lo_primitive_property->set_edm_name( 'RoomNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_NAME' ).
    lo_primitive_property->set_edm_name( 'StreetName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 60 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_PREFIX_NAME' ).
    lo_primitive_property->set_edm_name( 'StreetPrefixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STREET_SUFFIX_NAME' ).
    lo_primitive_property->set_edm_name( 'StreetSuffixName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TAX_JURISDICTION' ).
    lo_primitive_property->set_edm_name( 'TaxJurisdiction' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 15 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORT_ZONE' ).
    lo_primitive_property->set_edm_name( 'TransportZone' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ETAG' ).
    lo_primitive_property->set_edm_name( 'ETAG' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->use_as_etag( ).
    lo_primitive_property->set_is_technical( ).

  ENDMETHOD.


  METHOD def_a_outb_delivery_doc_flow_t.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_DOC_FLOW_T'
                                    is_structure              = VALUE tys_a_outb_delivery_doc_flow_t( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryDocFlowType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_DOC_FLOW' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryDocFlow' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERYVERSION' ).
    lo_primitive_property->set_edm_name( 'Deliveryversion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRECEDING_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'PrecedingDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRECEDING_DOCUMENT_CATEGOR' ).
    lo_primitive_property->set_edm_name( 'PrecedingDocumentCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRECEDING_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'PrecedingDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SUBSEQUENTDOCUMENT' ).
    lo_primitive_property->set_edm_name( 'Subsequentdocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'QUANTITY_IN_BASE_UNIT' ).
    lo_primitive_property->set_edm_name( 'QuantityInBaseUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SUBSEQUENT_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'SubsequentDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDFULFILLMENT_CALCULATION' ).
    lo_primitive_property->set_edm_name( 'SDFulfillmentCalculationRule' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SUBSEQUENT_DOCUMENT_CATEGO' ).
    lo_primitive_property->set_edm_name( 'SubsequentDocumentCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSFER_ORDER_IN_WRHS_MGM' ).
    lo_primitive_property->set_edm_name( 'TransferOrderInWrhsMgmtIsConfd' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ETAG' ).
    lo_primitive_property->set_edm_name( 'ETAG' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->use_as_etag( ).
    lo_primitive_property->set_is_technical( ).

  ENDMETHOD.


  METHOD def_a_outb_delivery_header_typ.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_HEADER_TYP'
                                    is_structure              = VALUE tys_a_outb_delivery_header_typ( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryHeaderType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_HEADER' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryHeader' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'ACTUAL_DELIVERY_ROUTE' ).
    lo_primitive_property->set_edm_name( 'ActualDeliveryRoute' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIPPINGLOCATIONTIMEZONE' ).
    lo_primitive_property->set_edm_name( 'Shippinglocationtimezone' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ACTUAL_GOODS_MOVEMENT_DATE' ).
    lo_primitive_property->set_edm_name( 'ActualGoodsMovementDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'RECEIVINGLOCATIONTIMEZONE' ).
    lo_primitive_property->set_edm_name( 'Receivinglocationtimezone' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ACTUAL_GOODS_MOVEMENT_TIME' ).
    lo_primitive_property->set_edm_name( 'ActualGoodsMovementTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BILLING_DOCUMENT_DATE' ).
    lo_primitive_property->set_edm_name( 'BillingDocumentDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BILL_OF_LADING' ).
    lo_primitive_property->set_edm_name( 'BillOfLading' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 35 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COMPLETE_DELIVERY_IS_DEFIN' ).
    lo_primitive_property->set_edm_name( 'CompleteDeliveryIsDefined' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CONFIRMATION_TIME' ).
    lo_primitive_property->set_edm_name( 'ConfirmationTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATED_BY_USER' ).
    lo_primitive_property->set_edm_name( 'CreatedByUser' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATION_DATE' ).
    lo_primitive_property->set_edm_name( 'CreationDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATION_TIME' ).
    lo_primitive_property->set_edm_name( 'CreationTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CUSTOMER_GROUP' ).
    lo_primitive_property->set_edm_name( 'CustomerGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_BLOCK_REASON' ).
    lo_primitive_property->set_edm_name( 'DeliveryBlockReason' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DATE' ).
    lo_primitive_property->set_edm_name( 'DeliveryDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_BY_SUPPL' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentBySupplier' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 35 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_TYPE' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_IS_IN_PLANT' ).
    lo_primitive_property->set_edm_name( 'DeliveryIsInPlant' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_PRIORITY' ).
    lo_primitive_property->set_edm_name( 'DeliveryPriority' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_TIME' ).
    lo_primitive_property->set_edm_name( 'DeliveryTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_VERSION' ).
    lo_primitive_property->set_edm_name( 'DeliveryVersion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DEPRECIATION_PERCENTAGE' ).
    lo_primitive_property->set_edm_name( 'DepreciationPercentage' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DISTR_STATUS_BY_DECENTRALI' ).
    lo_primitive_property->set_edm_name( 'DistrStatusByDecentralizedWrhs' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DOCUMENT_DATE' ).
    lo_primitive_property->set_edm_name( 'DocumentDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EXTERNAL_IDENTIFICATION_TY' ).
    lo_primitive_property->set_edm_name( 'ExternalIdentificationType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EXTERNAL_TRANSPORT_SYSTEM' ).
    lo_primitive_property->set_edm_name( 'ExternalTransportSystem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FACTORY_CALENDAR_BY_CUSTOM' ).
    lo_primitive_property->set_edm_name( 'FactoryCalendarByCustomer' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GOODS_ISSUE_OR_RECEIPT_SLI' ).
    lo_primitive_property->set_edm_name( 'GoodsIssueOrReceiptSlipNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GOODS_ISSUE_TIME' ).
    lo_primitive_property->set_edm_name( 'GoodsIssueTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HANDLING_UNIT_IN_STOCK' ).
    lo_primitive_property->set_edm_name( 'HandlingUnitInStock' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HDR_GENERAL_INCOMPLETION_S' ).
    lo_primitive_property->set_edm_name( 'HdrGeneralIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HDR_GOODS_MVT_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'HdrGoodsMvtIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_BILLG_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'HeaderBillgIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_BILLING_BLOCK_REASO' ).
    lo_primitive_property->set_edm_name( 'HeaderBillingBlockReason' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_DELIV_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'HeaderDelivIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_GROSS_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'HeaderGrossWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_NET_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'HeaderNetWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_PACKING_INCOMPLETIO' ).
    lo_primitive_property->set_edm_name( 'HeaderPackingIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_PICKG_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'HeaderPickgIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_VOLUME' ).
    lo_primitive_property->set_edm_name( 'HeaderVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_VOLUME_UNIT' ).
    lo_primitive_property->set_edm_name( 'HeaderVolumeUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HEADER_WEIGHT_UNIT' ).
    lo_primitive_property->set_edm_name( 'HeaderWeightUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INCOTERMS_CLASSIFICATION' ).
    lo_primitive_property->set_edm_name( 'IncotermsClassification' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INCOTERMS_TRANSFER_LOCATIO' ).
    lo_primitive_property->set_edm_name( 'IncotermsTransferLocation' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 28 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INTERCOMPANY_BILLING_DATE' ).
    lo_primitive_property->set_edm_name( 'IntercompanyBillingDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INTERNAL_FINANCIAL_DOCUMEN' ).
    lo_primitive_property->set_edm_name( 'InternalFinancialDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'IS_DELIVERY_FOR_SINGLE_WAR' ).
    lo_primitive_property->set_edm_name( 'IsDeliveryForSingleWarehouse' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'IS_EXPORT_DELIVERY' ).
    lo_primitive_property->set_edm_name( 'IsExportDelivery' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LAST_CHANGE_DATE' ).
    lo_primitive_property->set_edm_name( 'LastChangeDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LAST_CHANGED_BY_USER' ).
    lo_primitive_property->set_edm_name( 'LastChangedByUser' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LOADING_DATE' ).
    lo_primitive_property->set_edm_name( 'LoadingDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LOADING_POINT' ).
    lo_primitive_property->set_edm_name( 'LoadingPoint' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LOADING_TIME' ).
    lo_primitive_property->set_edm_name( 'LoadingTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MEANS_OF_TRANSPORT' ).
    lo_primitive_property->set_edm_name( 'MeansOfTransport' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MEANS_OF_TRANSPORT_REF_MAT' ).
    lo_primitive_property->set_edm_name( 'MeansOfTransportRefMaterial' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MEANS_OF_TRANSPORT_TYPE' ).
    lo_primitive_property->set_edm_name( 'MeansOfTransportType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ORDER_COMBINATION_IS_ALLOW' ).
    lo_primitive_property->set_edm_name( 'OrderCombinationIsAllowed' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ORDER_ID' ).
    lo_primitive_property->set_edm_name( 'OrderID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_DELIV_CONF_STATUS' ).
    lo_primitive_property->set_edm_name( 'OverallDelivConfStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_DELIV_RELTD_BILLG' ).
    lo_primitive_property->set_edm_name( 'OverallDelivReltdBillgStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_GOODS_MOVEMENT_STA' ).
    lo_primitive_property->set_edm_name( 'OverallGoodsMovementStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_INTCO_BILLING_STAT' ).
    lo_primitive_property->set_edm_name( 'OverallIntcoBillingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_PACKING_STATUS' ).
    lo_primitive_property->set_edm_name( 'OverallPackingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_PICKING_CONF_STATU' ).
    lo_primitive_property->set_edm_name( 'OverallPickingConfStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_PICKING_STATUS' ).
    lo_primitive_property->set_edm_name( 'OverallPickingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_PROOF_OF_DELIVERY' ).
    lo_primitive_property->set_edm_name( 'OverallProofOfDeliveryStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_SDPROCESS_STATUS' ).
    lo_primitive_property->set_edm_name( 'OverallSDProcessStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERALL_WAREHOUSE_ACTIVITY' ).
    lo_primitive_property->set_edm_name( 'OverallWarehouseActivityStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVRL_ITM_DELIV_INCOMPLETIO' ).
    lo_primitive_property->set_edm_name( 'OvrlItmDelivIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVRL_ITM_GDS_MVT_INCOMPLET' ).
    lo_primitive_property->set_edm_name( 'OvrlItmGdsMvtIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVRL_ITM_GENERAL_INCOMPLET' ).
    lo_primitive_property->set_edm_name( 'OvrlItmGeneralIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVRL_ITM_PACKING_INCOMPLET' ).
    lo_primitive_property->set_edm_name( 'OvrlItmPackingIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVRL_ITM_PICKING_INCOMPLET' ).
    lo_primitive_property->set_edm_name( 'OvrlItmPickingIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PAYMENT_GUARANTEE_PROCEDUR' ).
    lo_primitive_property->set_edm_name( 'PaymentGuaranteeProcedure' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PICKED_ITEMS_LOCATION' ).
    lo_primitive_property->set_edm_name( 'PickedItemsLocation' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 20 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PICKING_DATE' ).
    lo_primitive_property->set_edm_name( 'PickingDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PICKING_TIME' ).
    lo_primitive_property->set_edm_name( 'PickingTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PLANNED_GOODS_ISSUE_DATE' ).
    lo_primitive_property->set_edm_name( 'PlannedGoodsIssueDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PROOF_OF_DELIVERY_DATE' ).
    lo_primitive_property->set_edm_name( 'ProofOfDeliveryDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PROPOSED_DELIVERY_ROUTE' ).
    lo_primitive_property->set_edm_name( 'ProposedDeliveryRoute' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'RECEIVING_PLANT' ).
    lo_primitive_property->set_edm_name( 'ReceivingPlant' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ROUTE_SCHEDULE' ).
    lo_primitive_property->set_edm_name( 'RouteSchedule' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SALES_DISTRICT' ).
    lo_primitive_property->set_edm_name( 'SalesDistrict' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SALES_OFFICE' ).
    lo_primitive_property->set_edm_name( 'SalesOffice' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SALES_ORGANIZATION' ).
    lo_primitive_property->set_edm_name( 'SalesOrganization' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDDOCUMENT_CATEGORY' ).
    lo_primitive_property->set_edm_name( 'SDDocumentCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIPMENT_BLOCK_REASON' ).
    lo_primitive_property->set_edm_name( 'ShipmentBlockReason' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIPPING_CONDITION' ).
    lo_primitive_property->set_edm_name( 'ShippingCondition' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIPPING_POINT' ).
    lo_primitive_property->set_edm_name( 'ShippingPoint' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIPPING_TYPE' ).
    lo_primitive_property->set_edm_name( 'ShippingType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHIP_TO_PARTY' ).
    lo_primitive_property->set_edm_name( 'ShipToParty' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SOLD_TO_PARTY' ).
    lo_primitive_property->set_edm_name( 'SoldToParty' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SPECIAL_PROCESSING_CODE' ).
    lo_primitive_property->set_edm_name( 'SpecialProcessingCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STATISTICS_CURRENCY' ).
    lo_primitive_property->set_edm_name( 'StatisticsCurrency' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SUPPLIER' ).
    lo_primitive_property->set_edm_name( 'Supplier' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TOTAL_BLOCK_STATUS' ).
    lo_primitive_property->set_edm_name( 'TotalBlockStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TOTAL_CREDIT_CHECK_STATUS' ).
    lo_primitive_property->set_edm_name( 'TotalCreditCheckStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TOTAL_NUMBER_OF_PACKAGE' ).
    lo_primitive_property->set_edm_name( 'TotalNumberOfPackage' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSACTION_CURRENCY' ).
    lo_primitive_property->set_edm_name( 'TransactionCurrency' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORTATION_GROUP' ).
    lo_primitive_property->set_edm_name( 'TransportationGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORTATION_PLANNING_DA' ).
    lo_primitive_property->set_edm_name( 'TransportationPlanningDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORTATION_PLANNING_ST' ).
    lo_primitive_property->set_edm_name( 'TransportationPlanningStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORTATION_PLANNING_TI' ).
    lo_primitive_property->set_edm_name( 'TransportationPlanningTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'UNLOADING_POINT_NAME' ).
    lo_primitive_property->set_edm_name( 'UnloadingPointName' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 25 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE' ).
    lo_primitive_property->set_edm_name( 'Warehouse' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE_GATE' ).
    lo_primitive_property->set_edm_name( 'WarehouseGate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE_STAGING_AREA' ).
    lo_primitive_property->set_edm_name( 'WarehouseStagingArea' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ETAG' ).
    lo_primitive_property->set_edm_name( 'ETAG' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->use_as_etag( ).
    lo_primitive_property->set_is_technical( ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_DELIVERY_DOCUMENT_ITEM' ).
    lo_navigation_property->set_edm_name( 'to_DeliveryDocumentItem' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_ITEM_TYPE' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_DELIVERY_DOCUMENT_PARTN' ).
    lo_navigation_property->set_edm_name( 'to_DeliveryDocumentPartner' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_PARTNER_TY' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_DELIVERY_DOCUMENT_TEXT' ).
    lo_navigation_property->set_edm_name( 'to_DeliveryDocumentText' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_HEADER_T_2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_HANDLING_UNIT_HEADER_DE' ).
    lo_navigation_property->set_edm_name( 'to_HandlingUnitHeaderDelivery' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_HANDLING_UNIT_HEADER_D_2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

  ENDMETHOD.


  METHOD def_a_outb_delivery_header_t_2.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_HEADER_T_2'
                                    is_structure              = VALUE tys_a_outb_delivery_header_t_2( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryHeaderTextType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_HEADER_TEX' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryHeaderText' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TEXT_ELEMENT' ).
    lo_primitive_property->set_edm_name( 'TextElement' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LANGUAGE' ).
    lo_primitive_property->set_edm_name( 'Language' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TEXT_ELEMENT_DESCRIPTION' ).
    lo_primitive_property->set_edm_name( 'TextElementDescription' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TEXT_ELEMENT_TEXT' ).
    lo_primitive_property->set_edm_name( 'TextElementText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_LONG_TEXT_IS_FORM' ).
    lo_primitive_property->set_edm_name( 'DeliveryLongTextIsFormatted' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

  ENDMETHOD.


  METHOD def_a_outb_delivery_item_tex_2.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_ITEM_TEX_2'
                                    is_structure              = VALUE tys_a_outb_delivery_item_tex_2( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryItemTextType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_ITEM_TEXT' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryItemText' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TEXT_ELEMENT' ).
    lo_primitive_property->set_edm_name( 'TextElement' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LANGUAGE' ).
    lo_primitive_property->set_edm_name( 'Language' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TEXT_ELEMENT_DESCRIPTION' ).
    lo_primitive_property->set_edm_name( 'TextElementDescription' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 30 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TEXT_ELEMENT_TEXT' ).
    lo_primitive_property->set_edm_name( 'TextElementText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_LONG_TEXT_IS_FORM' ).
    lo_primitive_property->set_edm_name( 'DeliveryLongTextIsFormatted' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.

  ENDMETHOD.


  METHOD def_a_outb_delivery_item_type.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_ITEM_TYPE'
                                    is_structure              = VALUE tys_a_outb_delivery_item_type( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryItemType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_ITEM' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryItem' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'ACTUAL_DELIVERED_QTY_IN_BA' ).
    lo_primitive_property->set_edm_name( 'ActualDeliveredQtyInBaseUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ACTUAL_DELIVERY_QUANTITY' ).
    lo_primitive_property->set_edm_name( 'ActualDeliveryQuantity' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_CUSTOMER_GROUP' ).
    lo_primitive_property->set_edm_name( 'AdditionalCustomerGroup1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_CUSTOMER_GROU_2' ).
    lo_primitive_property->set_edm_name( 'AdditionalCustomerGroup2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_CUSTOMER_GROU_3' ).
    lo_primitive_property->set_edm_name( 'AdditionalCustomerGroup3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_CUSTOMER_GROU_4' ).
    lo_primitive_property->set_edm_name( 'AdditionalCustomerGroup4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_CUSTOMER_GROU_5' ).
    lo_primitive_property->set_edm_name( 'AdditionalCustomerGroup5' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_MATERIAL_GROUP' ).
    lo_primitive_property->set_edm_name( 'AdditionalMaterialGroup1' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_MATERIAL_GROU_2' ).
    lo_primitive_property->set_edm_name( 'AdditionalMaterialGroup2' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_MATERIAL_GROU_3' ).
    lo_primitive_property->set_edm_name( 'AdditionalMaterialGroup3' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_MATERIAL_GROU_4' ).
    lo_primitive_property->set_edm_name( 'AdditionalMaterialGroup4' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDITIONAL_MATERIAL_GROU_5' ).
    lo_primitive_property->set_edm_name( 'AdditionalMaterialGroup5' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ALTERNATE_PRODUCT_NUMBER' ).
    lo_primitive_property->set_edm_name( 'AlternateProductNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BASE_UNIT' ).
    lo_primitive_property->set_edm_name( 'BaseUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BATCH' ).
    lo_primitive_property->set_edm_name( 'Batch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BATCH_BY_SUPPLIER' ).
    lo_primitive_property->set_edm_name( 'BatchBySupplier' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BATCH_CLASSIFICATION' ).
    lo_primitive_property->set_edm_name( 'BatchClassification' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BOMEXPLOSION' ).
    lo_primitive_property->set_edm_name( 'BOMExplosion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 8 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_AREA' ).
    lo_primitive_property->set_edm_name( 'BusinessArea' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CONSUMPTION_POSTING' ).
    lo_primitive_property->set_edm_name( 'ConsumptionPosting' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CONTROLLING_AREA' ).
    lo_primitive_property->set_edm_name( 'ControllingArea' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'COST_CENTER' ).
    lo_primitive_property->set_edm_name( 'CostCenter' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATED_BY_USER' ).
    lo_primitive_property->set_edm_name( 'CreatedByUser' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATION_DATE' ).
    lo_primitive_property->set_edm_name( 'CreationDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CREATION_TIME' ).
    lo_primitive_property->set_edm_name( 'CreationTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CUST_ENGINEERING_CHG_STATU' ).
    lo_primitive_property->set_edm_name( 'CustEngineeringChgStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 17 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM_CAT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItemCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM_TEX' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItemText' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_GROUP' ).
    lo_primitive_property->set_edm_name( 'DeliveryGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_QUANTITY_UNIT' ).
    lo_primitive_property->set_edm_name( 'DeliveryQuantityUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_RELATED_BILLING_S' ).
    lo_primitive_property->set_edm_name( 'DeliveryRelatedBillingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_TO_BASE_QUANTITY' ).
    lo_primitive_property->set_edm_name( 'DeliveryToBaseQuantityDnmntr' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_TO_BASE_QUANTIT_2' ).
    lo_primitive_property->set_edm_name( 'DeliveryToBaseQuantityNmrtr' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_VERSION' ).
    lo_primitive_property->set_edm_name( 'DeliveryVersion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DEPARTMENT_CLASSIFICATION' ).
    lo_primitive_property->set_edm_name( 'DepartmentClassificationByCust' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DISTRIBUTION_CHANNEL' ).
    lo_primitive_property->set_edm_name( 'DistributionChannel' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DIVISION' ).
    lo_primitive_property->set_edm_name( 'Division' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'EUDELIVERY_ITEM_ARCSTATUS' ).
    lo_primitive_property->set_edm_name( 'EUDeliveryItemARCStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'FIXED_SHIPG_PROCG_DURATION' ).
    lo_primitive_property->set_edm_name( 'FixedShipgProcgDurationInDays' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GLACCOUNT' ).
    lo_primitive_property->set_edm_name( 'GLAccount' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GOODS_MOVEMENT_REASON_CODE' ).
    lo_primitive_property->set_edm_name( 'GoodsMovementReasonCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GOODS_MOVEMENT_STATUS' ).
    lo_primitive_property->set_edm_name( 'GoodsMovementStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'GOODS_MOVEMENT_TYPE' ).
    lo_primitive_property->set_edm_name( 'GoodsMovementType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HIGHER_LVL_ITM_OF_BAT_SPLT' ).
    lo_primitive_property->set_edm_name( 'HigherLvlItmOfBatSpltItm' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'HIGHER_LEVEL_ITEM' ).
    lo_primitive_property->set_edm_name( 'HigherLevelItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INSPECTION_LOT' ).
    lo_primitive_property->set_edm_name( 'InspectionLot' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INSPECTION_PARTIAL_LOT' ).
    lo_primitive_property->set_edm_name( 'InspectionPartialLot' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INTERCOMPANY_BILLING_STATU' ).
    lo_primitive_property->set_edm_name( 'IntercompanyBillingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INTERNATIONAL_ARTICLE_NUMB' ).
    lo_primitive_property->set_edm_name( 'InternationalArticleNumber' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INVENTORY_SPECIAL_STOCK_TY' ).
    lo_primitive_property->set_edm_name( 'InventorySpecialStockType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'INVENTORY_VALUATION_TYPE' ).
    lo_primitive_property->set_edm_name( 'InventoryValuationType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'IS_COMPLETELY_DELIVERED' ).
    lo_primitive_property->set_edm_name( 'IsCompletelyDelivered' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'IS_NOT_GOODS_MOVEMENTS_REL' ).
    lo_primitive_property->set_edm_name( 'IsNotGoodsMovementsRelevant' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'IS_SEPARATE_VALUATION' ).
    lo_primitive_property->set_edm_name( 'IsSeparateValuation' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSG_OR_RCVG_BATCH' ).
    lo_primitive_property->set_edm_name( 'IssgOrRcvgBatch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSG_OR_RCVG_MATERIAL' ).
    lo_primitive_property->set_edm_name( 'IssgOrRcvgMaterial' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSG_OR_RCVG_SPCL_STOCK_IN' ).
    lo_primitive_property->set_edm_name( 'IssgOrRcvgSpclStockInd' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSG_OR_RCVG_STOCK_CATEGOR' ).
    lo_primitive_property->set_edm_name( 'IssgOrRcvgStockCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSG_OR_RCVG_VALUATION_TYP' ).
    lo_primitive_property->set_edm_name( 'IssgOrRcvgValuationType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSUING_OR_RECEIVING_PLANT' ).
    lo_primitive_property->set_edm_name( 'IssuingOrReceivingPlant' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ISSUING_OR_RECEIVING_STORA' ).
    lo_primitive_property->set_edm_name( 'IssuingOrReceivingStorageLoc' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_BILLING_BLOCK_REASON' ).
    lo_primitive_property->set_edm_name( 'ItemBillingBlockReason' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_BILLING_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'ItemBillingIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_DELIVERY_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'ItemDeliveryIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_GDS_MVT_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'ItemGdsMvtIncompletionSts' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_GENERAL_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'ItemGeneralIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_GROSS_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'ItemGrossWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_IS_BILLING_RELEVANT' ).
    lo_primitive_property->set_edm_name( 'ItemIsBillingRelevant' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_NET_WEIGHT' ).
    lo_primitive_property->set_edm_name( 'ItemNetWeight' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_PACKING_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'ItemPackingIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_PICKING_INCOMPLETION' ).
    lo_primitive_property->set_edm_name( 'ItemPickingIncompletionStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_VOLUME' ).
    lo_primitive_property->set_edm_name( 'ItemVolume' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 15 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_VOLUME_UNIT' ).
    lo_primitive_property->set_edm_name( 'ItemVolumeUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ITEM_WEIGHT_UNIT' ).
    lo_primitive_property->set_edm_name( 'ItemWeightUnit' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LAST_CHANGE_DATE' ).
    lo_primitive_property->set_edm_name( 'LastChangeDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'LOADING_GROUP' ).
    lo_primitive_property->set_edm_name( 'LoadingGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MANUFACTURE_DATE' ).
    lo_primitive_property->set_edm_name( 'ManufactureDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL' ).
    lo_primitive_property->set_edm_name( 'Material' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_BY_CUSTOMER' ).
    lo_primitive_property->set_edm_name( 'MaterialByCustomer' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 35 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_FREIGHT_GROUP' ).
    lo_primitive_property->set_edm_name( 'MaterialFreightGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 8 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_GROUP' ).
    lo_primitive_property->set_edm_name( 'MaterialGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 9 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_IS_BATCH_MANAGED' ).
    lo_primitive_property->set_edm_name( 'MaterialIsBatchManaged' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MATERIAL_IS_INT_BATCH_MANA' ).
    lo_primitive_property->set_edm_name( 'MaterialIsIntBatchManaged' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'NUMBER_OF_SERIAL_NUMBERS' ).
    lo_primitive_property->set_edm_name( 'NumberOfSerialNumbers' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Int32' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ORDER_ID' ).
    lo_primitive_property->set_edm_name( 'OrderID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 12 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ORDER_ITEM' ).
    lo_primitive_property->set_edm_name( 'OrderItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ORIGINAL_DELIVERY_QUANTITY' ).
    lo_primitive_property->set_edm_name( 'OriginalDeliveryQuantity' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 13 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ORIGINALLY_REQUESTED_MATER' ).
    lo_primitive_property->set_edm_name( 'OriginallyRequestedMaterial' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 40 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'OVERDELIV_TOLRTD_LMT_RATIO' ).
    lo_primitive_property->set_edm_name( 'OverdelivTolrtdLmtRatioInPct' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PACKING_STATUS' ).
    lo_primitive_property->set_edm_name( 'PackingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PARTIAL_DELIVERY_IS_ALLOWE' ).
    lo_primitive_property->set_edm_name( 'PartialDeliveryIsAllowed' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PAYMENT_GUARANTEE_FORM' ).
    lo_primitive_property->set_edm_name( 'PaymentGuaranteeForm' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PICKING_CONFIRMATION_STATU' ).
    lo_primitive_property->set_edm_name( 'PickingConfirmationStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PICKING_CONTROL' ).
    lo_primitive_property->set_edm_name( 'PickingControl' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PICKING_STATUS' ).
    lo_primitive_property->set_edm_name( 'PickingStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PLANT' ).
    lo_primitive_property->set_edm_name( 'Plant' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRIMARY_POSTING_SWITCH' ).
    lo_primitive_property->set_edm_name( 'PrimaryPostingSwitch' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRODUCT_AVAILABILITY_DATE' ).
    lo_primitive_property->set_edm_name( 'ProductAvailabilityDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRODUCT_AVAILABILITY_TIME' ).
    lo_primitive_property->set_edm_name( 'ProductAvailabilityTime' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'TimeOfDay' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'Time' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRODUCT_CONFIGURATION' ).
    lo_primitive_property->set_edm_name( 'ProductConfiguration' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PRODUCT_HIERARCHY_NODE' ).
    lo_primitive_property->set_edm_name( 'ProductHierarchyNode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 18 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PROFITABILITY_SEGMENT' ).
    lo_primitive_property->set_edm_name( 'ProfitabilitySegment' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PROFIT_CENTER' ).
    lo_primitive_property->set_edm_name( 'ProfitCenter' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PROOF_OF_DELIVERY_RELEVANC' ).
    lo_primitive_property->set_edm_name( 'ProofOfDeliveryRelevanceCode' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PROOF_OF_DELIVERY_STATUS' ).
    lo_primitive_property->set_edm_name( 'ProofOfDeliveryStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'QUANTITY_IS_FIXED' ).
    lo_primitive_property->set_edm_name( 'QuantityIsFixed' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'RECEIVING_POINT' ).
    lo_primitive_property->set_edm_name( 'ReceivingPoint' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 25 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REFERENCE_DOCUMENT_LOGICAL' ).
    lo_primitive_property->set_edm_name( 'ReferenceDocumentLogicalSystem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REFERENCE_SDDOCUMENT' ).
    lo_primitive_property->set_edm_name( 'ReferenceSDDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REFERENCE_SDDOCUMENT_CATEG' ).
    lo_primitive_property->set_edm_name( 'ReferenceSDDocumentCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REFERENCE_SDDOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'ReferenceSDDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'RETAIL_PROMOTION' ).
    lo_primitive_property->set_edm_name( 'RetailPromotion' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SALES_DOCUMENT_ITEM_TYPE' ).
    lo_primitive_property->set_edm_name( 'SalesDocumentItemType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SALES_GROUP' ).
    lo_primitive_property->set_edm_name( 'SalesGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SALES_OFFICE' ).
    lo_primitive_property->set_edm_name( 'SalesOffice' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDDOCUMENT_CATEGORY' ).
    lo_primitive_property->set_edm_name( 'SDDocumentCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDPROCESS_STATUS' ).
    lo_primitive_property->set_edm_name( 'SDProcessStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SHELF_LIFE_EXPIRATION_DATE' ).
    lo_primitive_property->set_edm_name( 'ShelfLifeExpirationDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STATISTICS_DATE' ).
    lo_primitive_property->set_edm_name( 'StatisticsDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STOCK_TYPE' ).
    lo_primitive_property->set_edm_name( 'StockType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STORAGE_BIN' ).
    lo_primitive_property->set_edm_name( 'StorageBin' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STORAGE_LOCATION' ).
    lo_primitive_property->set_edm_name( 'StorageLocation' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'STORAGE_TYPE' ).
    lo_primitive_property->set_edm_name( 'StorageType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SUBSEQUENT_MOVEMENT_TYPE' ).
    lo_primitive_property->set_edm_name( 'SubsequentMovementType' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'TRANSPORTATION_GROUP' ).
    lo_primitive_property->set_edm_name( 'TransportationGroup' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'UNDERDELIV_TOLRTD_LMT_RATI' ).
    lo_primitive_property->set_edm_name( 'UnderdelivTolrtdLmtRatioInPct' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'UNLIMITED_OVERDELIVERY_IS' ).
    lo_primitive_property->set_edm_name( 'UnlimitedOverdeliveryIsAllowed' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Boolean' ) ##NO_TEXT.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'VARBL_SHIPG_PROCG_DURATION' ).
    lo_primitive_property->set_edm_name( 'VarblShipgProcgDurationInDays' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 5 ) ##NUMBER_OK.
    lo_primitive_property->set_scale( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE' ).
    lo_primitive_property->set_edm_name( 'Warehouse' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 3 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE_ACTIVITY_STATUS' ).
    lo_primitive_property->set_edm_name( 'WarehouseActivityStatus' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE_STAGING_AREA' ).
    lo_primitive_property->set_edm_name( 'WarehouseStagingArea' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE_STOCK_CATEGORY' ).
    lo_primitive_property->set_edm_name( 'WarehouseStockCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 1 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'WAREHOUSE_STORAGE_BIN' ).
    lo_primitive_property->set_edm_name( 'WarehouseStorageBin' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_nullable( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'ETAG' ).
    lo_primitive_property->set_edm_name( 'ETAG' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->use_as_etag( ).
    lo_primitive_property->set_is_technical( ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_DELIVERY_DOCUMENT_ITEM' ).
    lo_navigation_property->set_edm_name( 'to_DeliveryDocumentItemText' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_ITEM_TEX_2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_DOCUMENT_FLOW' ).
    lo_navigation_property->set_edm_name( 'to_DocumentFlow' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_DOC_FLOW_T' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_HANDLING_UNIT_ITEM_DELI' ).
    lo_navigation_property->set_edm_name( 'to_HandlingUnitItemDelivery' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_HANDLING_UNIT_ITEM_DEL_2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_SERIAL_DELIVERY_ITEM' ).
    lo_navigation_property->set_edm_name( 'to_SerialDeliveryItem' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_SERIAL_NMBR_DELIVERY_TYP' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_one_optional ).

  ENDMETHOD.


  METHOD def_a_outb_delivery_partner_ty.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_OUTB_DELIVERY_PARTNER_TY'
                                    is_structure              = VALUE tys_a_outb_delivery_partner_ty( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_OutbDeliveryPartnerType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_OUTB_DELIVERY_PARTNER' ).
    lo_entity_set->set_edm_name( 'A_OutbDeliveryPartner' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'ADDRESS_ID' ).
    lo_primitive_property->set_edm_name( 'AddressID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'BUSINESS_PARTNER_ADDRESS_U' ).
    lo_primitive_property->set_edm_name( 'BusinessPartnerAddressUUID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Guid' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'REF_BUSINESS_PARTNER_ADDRE' ).
    lo_primitive_property->set_edm_name( 'RefBusinessPartnerAddressUUID' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Guid' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CONTACT_PERSON' ).
    lo_primitive_property->set_edm_name( 'ContactPerson' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'CUSTOMER' ).
    lo_primitive_property->set_edm_name( 'Customer' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PARTNER_FUNCTION' ).
    lo_primitive_property->set_edm_name( 'PartnerFunction' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 2 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'PERSONNEL' ).
    lo_primitive_property->set_edm_name( 'Personnel' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 8 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDDOCUMENT' ).
    lo_primitive_property->set_edm_name( 'SDDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDDOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'SDDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SUPPLIER' ).
    lo_primitive_property->set_edm_name( 'Supplier' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_ADDRESS' ).
    lo_navigation_property->set_edm_name( 'to_Address' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_ADDRESS_TY' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_one ).

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_ADDRESS_2' ).
    lo_navigation_property->set_edm_name( 'to_Address2' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_OUTB_DELIVERY_ADDRESS__2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_one ).

  ENDMETHOD.


  METHOD def_a_serial_nmbr_delivery_typ.

    DATA:
      lo_complex_property    TYPE REF TO /iwbep/if_v4_pm_cplx_prop,
      lo_entity_type         TYPE REF TO /iwbep/if_v4_pm_entity_type,
      lo_entity_set          TYPE REF TO /iwbep/if_v4_pm_entity_set,
      lo_navigation_property TYPE REF TO /iwbep/if_v4_pm_nav_prop,
      lo_primitive_property  TYPE REF TO /iwbep/if_v4_pm_prim_prop.


    lo_entity_type = mo_model->create_entity_type_by_struct(
                                    iv_entity_type_name       = 'A_SERIAL_NMBR_DELIVERY_TYP'
                                    is_structure              = VALUE tys_a_serial_nmbr_delivery_typ( )
                                    iv_do_gen_prim_props         = abap_true
                                    iv_do_gen_prim_prop_colls    = abap_true
                                    iv_do_add_conv_to_prim_props = abap_true ).

    lo_entity_type->set_edm_name( 'A_SerialNmbrDeliveryType' ) ##NO_TEXT.


    lo_entity_set = lo_entity_type->create_entity_set( 'A_SERIAL_NMBR_DELIVERY' ).
    lo_entity_set->set_edm_name( 'A_SerialNmbrDelivery' ) ##NO_TEXT.


    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DATE' ).
    lo_primitive_property->set_edm_name( 'DeliveryDate' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_property->set_precision( 7 ) ##NUMBER_OK.
    lo_primitive_property->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 10 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_primitive_property->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 6 ) ##NUMBER_OK.

    lo_primitive_property = lo_entity_type->get_primitive_property( 'MAINTENANCE_ITEM_OBJECT_LI' ).
    lo_primitive_property->set_edm_name( 'MaintenanceItemObjectList' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'Int64' ) ##NO_TEXT.
    lo_primitive_property->set_is_key( ).

    lo_primitive_property = lo_entity_type->get_primitive_property( 'SDDOCUMENT_CATEGORY' ).
    lo_primitive_property->set_edm_name( 'SDDocumentCategory' ) ##NO_TEXT.
    lo_primitive_property->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_property->set_max_length( 4 ) ##NUMBER_OK.

    lo_navigation_property = lo_entity_type->create_navigation_property( 'TO_MAINTENANCE_ITEM_OBJECT' ).
    lo_navigation_property->set_edm_name( 'to_MaintenanceItemObject' ) ##NO_TEXT.
    lo_navigation_property->set_target_entity_type_name( 'A_MAINTENANCE_ITEM_OBJEC_2' ).
    lo_navigation_property->set_target_multiplicity( /iwbep/if_v4_pm_types=>gcs_nav_multiplicity-to_many_optional ).

  ENDMETHOD.


  METHOD def_add_serial_number_to_deliv.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'ADD_SERIAL_NUMBER_TO_DELIV' ).
    lo_function->set_edm_name( 'AddSerialNumberToDeliveryItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'ADD_SERIAL_NUMBER_TO_DELIV' ).
    lo_function_import->set_edm_name( 'AddSerialNumberToDeliveryItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_6' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_4' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'SERIAL_NUMBER' ).
    lo_parameter->set_edm_name( 'SerialNumber' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SERIAL_NUMBER' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_assign_handling_unit_to_de.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'ASSIGN_HANDLING_UNIT_TO_DE' ).
    lo_function->set_edm_name( 'AssignHandlingUnitToDelivery' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'ASSIGN_HANDLING_UNIT_TO_DE' ).
    lo_function_import->set_edm_name( 'AssignHandlingUnitToDelivery' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_4' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'HANDLING_UNIT_EXTERNAL_ID' ).
    lo_parameter->set_edm_name( 'HandlingUnitExternalId' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'HANDLING_UNIT_EXTERNAL_ID' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'HU_RETURN' ).

  ENDMETHOD.


  METHOD def_confirm_picking_all_items.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'CONFIRM_PICKING_ALL_ITEMS' ).
    lo_function->set_edm_name( 'ConfirmPickingAllItems' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'CONFIRM_PICKING_ALL_ITEMS' ).
    lo_function_import->set_edm_name( 'ConfirmPickingAllItems' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_11' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_confirm_picking_one_item.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'CONFIRM_PICKING_ONE_ITEM' ).
    lo_function->set_edm_name( 'ConfirmPickingOneItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'CONFIRM_PICKING_ONE_ITEM' ).
    lo_function_import->set_edm_name( 'ConfirmPickingOneItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_7' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_12' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_create_batch_split_item.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'CREATE_BATCH_SPLIT_ITEM' ).
    lo_function->set_edm_name( 'CreateBatchSplitItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'CREATE_BATCH_SPLIT_ITEM' ).
    lo_function_import->set_edm_name( 'CreateBatchSplitItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'PICK_QUANTITY_IN_SALES_UOM' ).
    lo_parameter->set_edm_name( 'PickQuantityInSalesUOM' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'PICK_QUANTITY_IN_SALES_UOM' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'BATCH' ).
    lo_parameter->set_edm_name( 'Batch' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'BATCH' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'ACTUAL_DELIVERY_QUANTITY' ).
    lo_parameter->set_edm_name( 'ActualDeliveryQuantity' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'ACTUAL_DELIVERY_QUANTITY' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_QUANTITY_UNIT' ).
    lo_parameter->set_edm_name( 'DeliveryQuantityUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_QUANTITY_UNIT' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'CREATED_DELIVERY_ITEM' ).

  ENDMETHOD.


  METHOD def_delete_all_handling_units.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'DELETE_ALL_HANDLING_UNITS' ).
    lo_function->set_edm_name( 'DeleteAllHandlingUnitsFromDelivery' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'DELETE_ALL_HANDLING_UNITS' ).
    lo_function_import->set_edm_name( 'DeleteAllHandlingUnitsFromDelivery' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'HU_RETURN' ).

  ENDMETHOD.


  METHOD def_delete_all_serial_numbers.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'DELETE_ALL_SERIAL_NUMBERS' ).
    lo_function->set_edm_name( 'DeleteAllSerialNumbersFromDeliveryItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'DELETE_ALL_SERIAL_NUMBERS' ).
    lo_function_import->set_edm_name( 'DeleteAllSerialNumbersFromDeliveryItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_8' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_6' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_delete_serial_number_from.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'DELETE_SERIAL_NUMBER_FROM' ).
    lo_function->set_edm_name( 'DeleteSerialNumberFromDeliveryItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'DELETE_SERIAL_NUMBER_FROM' ).
    lo_function_import->set_edm_name( 'DeleteSerialNumberFromDeliveryItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_7' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_5' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'SERIAL_NUMBER' ).
    lo_parameter->set_edm_name( 'SerialNumber' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SERIAL_NUMBER_2' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_pick_all_items.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'PICK_ALL_ITEMS' ).
    lo_function->set_edm_name( 'PickAllItems' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'PICK_ALL_ITEMS' ).
    lo_function_import->set_edm_name( 'PickAllItems' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_13' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_pick_and_batch_split_one_i.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'PICK_AND_BATCH_SPLIT_ONE_I' ).
    lo_function->set_edm_name( 'PickAndBatchSplitOneItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'PICK_AND_BATCH_SPLIT_ONE_I' ).
    lo_function_import->set_edm_name( 'PickAndBatchSplitOneItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_3' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'BATCH' ).
    lo_parameter->set_edm_name( 'Batch' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'BATCH_2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'SPLIT_QUANTITY' ).
    lo_parameter->set_edm_name( 'SplitQuantity' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SPLIT_QUANTITY' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'SPLIT_QUANTITY_UNIT' ).
    lo_parameter->set_edm_name( 'SplitQuantityUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'SPLIT_QUANTITY_UNIT' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_pick_one_item.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'PICK_ONE_ITEM' ).
    lo_function->set_edm_name( 'PickOneItem' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'PICK_ONE_ITEM' ).
    lo_function_import->set_edm_name( 'PickOneItem' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_14' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_8' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_pick_one_item_with_base_qu.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'PICK_ONE_ITEM_WITH_BASE_QU' ).
    lo_function->set_edm_name( 'PickOneItemWithBaseQuantity' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'PICK_ONE_ITEM_WITH_BASE_QU' ).
    lo_function_import->set_edm_name( 'PickOneItemWithBaseQuantity' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_16' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_10' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'ACTUAL_DELIVERED_QTY_IN_BA' ).
    lo_parameter->set_edm_name( 'ActualDeliveredQtyInBaseUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'ACTUAL_DELIVERED_QTY_IN__2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'BASE_UNIT' ).
    lo_parameter->set_edm_name( 'BaseUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'BASE_UNIT_2' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_pick_one_item_with_sales_q.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'PICK_ONE_ITEM_WITH_SALES_Q' ).
    lo_function->set_edm_name( 'PickOneItemWithSalesQuantity' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'PICK_ONE_ITEM_WITH_SALES_Q' ).
    lo_function_import->set_edm_name( 'PickOneItemWithSalesQuantity' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'ACTUAL_DELIVERY_QUANTITY' ).
    lo_parameter->set_edm_name( 'ActualDeliveryQuantity' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'ACTUAL_DELIVERY_QUANTITY_2' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_15' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_9' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_QUANTITY_UNIT' ).
    lo_parameter->set_edm_name( 'DeliveryQuantityUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_QUANTITY_UNIT_2' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_post_goods_issue.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'POST_GOODS_ISSUE' ).
    lo_function->set_edm_name( 'PostGoodsIssue' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'POST_GOODS_ISSUE' ).
    lo_function_import->set_edm_name( 'PostGoodsIssue' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_9' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_reverse_goods_issue.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'REVERSE_GOODS_ISSUE' ).
    lo_function->set_edm_name( 'ReverseGoodsIssue' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'REVERSE_GOODS_ISSUE' ).
    lo_function_import->set_edm_name( 'ReverseGoodsIssue' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_10' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'ACTUAL_GOODS_MOVEMENT_DATE' ).
    lo_parameter->set_edm_name( 'ActualGoodsMovementDate' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'ACTUAL_GOODS_MOVEMENT_DATE' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'RETURN' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD def_set_picking_quantity_with.

    DATA:
      lo_function        TYPE REF TO /iwbep/if_v4_pm_function,
      lo_function_import TYPE REF TO /iwbep/if_v4_pm_func_imp,
      lo_parameter       TYPE REF TO /iwbep/if_v4_pm_func_param,
      lo_return          TYPE REF TO /iwbep/if_v4_pm_func_return.


    lo_function = mo_model->create_function( 'SET_PICKING_QUANTITY_WITH' ).
    lo_function->set_edm_name( 'SetPickingQuantityWithBaseQuantity' ) ##NO_TEXT.

    lo_function_import = lo_function->create_function_import( 'SET_PICKING_QUANTITY_WITH' ).
    lo_function_import->set_edm_name( 'SetPickingQuantityWithBaseQuantity' ) ##NO_TEXT.


    lo_parameter = lo_function->create_parameter( 'ACTUAL_DELIVERED_QTY_IN_BA' ).
    lo_parameter->set_edm_name( 'ActualDeliveredQtyInBaseUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'ACTUAL_DELIVERED_QTY_IN_BA' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'BASE_UNIT' ).
    lo_parameter->set_edm_name( 'BaseUnit' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'BASE_UNIT' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT' ).
    lo_parameter->set_edm_name( 'DeliveryDocument' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_5' ).
    lo_parameter->set_is_nullable( ).

    lo_parameter = lo_function->create_parameter( 'DELIVERY_DOCUMENT_ITEM' ).
    lo_parameter->set_edm_name( 'DeliveryDocumentItem' ) ##NO_TEXT.
    lo_parameter->set_primitive_type( 'DELIVERY_DOCUMENT_ITEM_3' ).
    lo_parameter->set_is_nullable( ).

    lo_return = lo_function->create_return( ).
    lo_return->set_complex_type( 'PICKING_REPORT' ).
    lo_return->set_is_collection( ).

  ENDMETHOD.


  METHOD define_primitive_types.

    DATA lo_primitive_type TYPE REF TO /iwbep/if_v4_pm_prim_type.


    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'ACTUAL_DELIVERED_QTY_IN_BA'
                            iv_element             = VALUE tys_types_for_prim_types-actual_delivered_qty_in_ba( ) ).
    lo_primitive_type->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_type->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'ACTUAL_DELIVERED_QTY_IN__2'
                            iv_element             = VALUE tys_types_for_prim_types-actual_delivered_qty_in__2( ) ).
    lo_primitive_type->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_type->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'ACTUAL_DELIVERY_QUANTITY'
                            iv_element             = VALUE tys_types_for_prim_types-actual_delivery_quantity( ) ).
    lo_primitive_type->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_type->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'ACTUAL_DELIVERY_QUANTITY_2'
                            iv_element             = VALUE tys_types_for_prim_types-actual_delivery_quantity_2( ) ).
    lo_primitive_type->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_type->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'ACTUAL_GOODS_MOVEMENT_DATE'
                            iv_element             = VALUE tys_types_for_prim_types-actual_goods_movement_date( ) ).
    lo_primitive_type->set_edm_type( 'DateTimeOffset' ) ##NO_TEXT.
    lo_primitive_type->set_edm_type_v2( 'DateTime' ) ##NO_TEXT.

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'BASE_UNIT'
                            iv_element             = VALUE tys_types_for_prim_types-base_unit( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'BASE_UNIT_2'
                            iv_element             = VALUE tys_types_for_prim_types-base_unit_2( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'BATCH'
                            iv_element             = VALUE tys_types_for_prim_types-batch( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'BATCH_2'
                            iv_element             = VALUE tys_types_for_prim_types-batch_2( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_10'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_10( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_11'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_11( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_12'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_12( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_13'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_13( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_14'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_14( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_15'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_15( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_16'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_16( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_2'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_2( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_3'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_3( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_4'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_4( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_5'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_5( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_6'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_6( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_7'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_7( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_8'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_8( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_9'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_9( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_10'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_10( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_2'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_2( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_3'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_3( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_4'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_4( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_5'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_5( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_6'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_6( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_7'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_7( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_8'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_8( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_DOCUMENT_ITEM_9'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_document_item_9( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_QUANTITY_UNIT'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_quantity_unit( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'DELIVERY_QUANTITY_UNIT_2'
                            iv_element             = VALUE tys_types_for_prim_types-delivery_quantity_unit_2( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'HANDLING_UNIT_EXTERNAL_ID'
                            iv_element             = VALUE tys_types_for_prim_types-handling_unit_external_id( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'PICK_QUANTITY_IN_SALES_UOM'
                            iv_element             = VALUE tys_types_for_prim_types-pick_quantity_in_sales_uom( ) ).
    lo_primitive_type->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_type->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'SERIAL_NUMBER'
                            iv_element             = VALUE tys_types_for_prim_types-serial_number( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'SERIAL_NUMBER_2'
                            iv_element             = VALUE tys_types_for_prim_types-serial_number_2( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'SPLIT_QUANTITY'
                            iv_element             = VALUE tys_types_for_prim_types-split_quantity( ) ).
    lo_primitive_type->set_edm_type( 'Decimal' ) ##NO_TEXT.
    lo_primitive_type->set_precision( 3 ) ##NUMBER_OK.
    lo_primitive_type->set_scale_variable( ).

    lo_primitive_type = mo_model->create_primitive_type_by_elem(
                            iv_primitive_type_name = 'SPLIT_QUANTITY_UNIT'
                            iv_element             = VALUE tys_types_for_prim_types-split_quantity_unit( ) ).
    lo_primitive_type->set_edm_type( 'String' ) ##NO_TEXT.
    lo_primitive_type->set_scale_variable( ).

  ENDMETHOD.

ENDCLASS.
