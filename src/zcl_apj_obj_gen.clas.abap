CLASS zcl_apj_obj_gen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-METHODS create_job IMPORTING iv_do TYPE vbeln_vl.

    CLASS-DATA: gv_onetime TYPE c VALUE 'X'.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_APJ_OBJ_GEN IMPLEMENTATION.


  METHOD create_job.
    DATA: lt_job_parameter TYPE cl_apj_rt_api=>tt_job_parameter_value.

    DATA: lv_job_text      TYPE cl_apj_rt_api=>ty_job_text.

    DATA: lv_template_name TYPE cl_apj_rt_api=>ty_template_name.

    DATA: ls_start_info    TYPE cl_apj_rt_api=>ty_start_info.

    DATA: ls_end_info      TYPE cl_apj_rt_api=>ty_end_info.

    DATA: lv_jobname       TYPE cl_apj_rt_api=>ty_jobname.
    DATA: lv_jobcount      TYPE cl_apj_rt_api=>ty_jobcount.

    DATA: lv_job_info      TYPE cl_apj_rt_api=>ty_job_info.

    DATA: lv_status        TYPE cl_apj_rt_api=>ty_job_status.
    DATA: lv_statustext    TYPE cl_apj_rt_api=>ty_job_status_text.

    DATA: txt TYPE string.
    DATA: tz  TYPE timezone.

    DATA dat TYPE d.
    DATA tim TYPE t.

    CHECK gv_onetime IS NOT INITIAL.
    CLEAR gv_onetime.

    lv_template_name = 'ZBJ_DO_TMP'.   "#EC NOTEXT
    lv_jobname       = |APP_JOB_DO_| && iv_do && |_| && cl_abap_context_info=>get_system_date( ) && cl_abap_context_info=>get_system_time( ). "#EC NOTEXT
    lv_job_text      = |DO | && iv_do. "#EC NOTEXT

    " the immediate start would look like this:
*    ls_start_info-start_immediately = abap_true.

    dat = cl_abap_context_info=>get_system_date( ).
    tim = cl_abap_context_info=>get_system_time( ) + 10.

    tz = cl_abap_tstmp=>get_system_timezone( ).

    CONVERT DATE dat TIME tim
            INTO TIME STAMP DATA(ts) TIME ZONE tz.

    ls_start_info-timestamp = ts.

    lt_job_parameter = VALUE #( ( name = 'P_DO'
                                  t_value = VALUE #( ( sign = 'I' option = 'EQ' low = iv_do ) )
                              ) ).
    TRY.
        " schedule job
        cl_apj_rt_api=>schedule_job(
          EXPORTING
            iv_job_template_name   = lv_template_name
            iv_job_text            = lv_job_text
            is_start_info          = ls_start_info
            it_job_parameter_value = lt_job_parameter
          IMPORTING
            ev_jobname             = lv_jobname
            ev_jobcount            = lv_jobcount ).

        " job status
        cl_apj_rt_api=>get_job_status(
          EXPORTING
            iv_jobname         = lv_jobname
            iv_jobcount        = lv_jobcount
          IMPORTING
            ev_job_status      = lv_status
            ev_job_status_text = lv_statustext ).


      CATCH cx_apj_rt INTO DATA(lx_exc). "#EC NO_HANDLER
        " Handle Exception
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
