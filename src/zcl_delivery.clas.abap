CLASS zcl_delivery DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_badi_interface .
    INTERFACES if_le_shp_save_doc_prepare .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.


CLASS ZCL_DELIVERY IMPLEMENTATION.

  METHOD if_le_shp_save_doc_prepare~modify_fields.


    IF documentprocessingmode = 'CHANGE' AND sy-cprog <> 'SAPMHTTP'.
      MOVE-CORRESPONDING delivery_document_in TO delivery_document_out.
      delivery_document_items_out[] = delivery_document_items_in[].

      IF delivery_document_in-deliverydocument IS NOT INITIAL AND
         ( delivery_document_in-deliverydocument+0(1) <> '$'  AND
           delivery_document_in-deliverydocument+0(3) <> '$ 1' ) AND
           delivery_document_in-overallgoodsmovementstatus <> 'C'.
        zcl_apj_obj_gen=>create_job( delivery_document_in-deliverydocument ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
