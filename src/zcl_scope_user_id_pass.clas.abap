CLASS zcl_scope_user_id_pass DEFINITION
PUBLIC
FINAL
CREATE PUBLIC .

PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SCOPE_USER_ID_PASS IMPLEMENTATION.


METHOD if_oo_adt_classrun~main.


    DATA(lo_scope_api) = cl_aps_bc_scope_change_api=>create_instance( ).

    lo_scope_api->scope(
    EXPORTING it_object_scope = VALUE #(
    pgmid = if_aps_bc_scope_change_api=>gc_tadir_pgmid-R3TR
    scope_state = if_aps_bc_scope_change_api=>gc_scope_state-ON

* Space template
   ( object = if_aps_bc_scope_change_api=>gc_tadir_object-UIST obj_name = 'ZUSER_ID_PASS_LPT' )

* Page template
    ( object = if_aps_bc_scope_change_api=>gc_tadir_object-UIPG obj_name = 'ZUSER_ID_PASS_LPT' )
    )

            iv_simulate = abap_false
            iv_force = abap_false
    IMPORTING et_object_result = DATA(lt_results)
            et_message = DATA(lt_messages) ).
ENDMETHOD.
ENDCLASS.
