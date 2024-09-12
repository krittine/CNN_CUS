*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section
class lcx_lock_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC section.
   INTERFACES if_t100_dyn_msg.
   INTERFACES if_t100_message.

ENDCLASS.
class lcx_handle_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC section.
   INTERFACES if_t100_dyn_msg.
   INTERFACES if_t100_message.

ENDCLASS.
