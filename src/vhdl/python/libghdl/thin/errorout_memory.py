from libghdl import libghdl
from ctypes import c_int32, c_char_p
import libghdl.thin.errorout as errorout

Install_Handler = libghdl.errorout__memory__install_handler

Get_Nbr_Messages = libghdl.errorout__memory__get_nbr_messages

Get_Error_Record = libghdl.errorout__memory__get_error_record
Get_Error_Record.argstypes = [c_int32]
Get_Error_Record.restype = errorout.Error_Record

Get_Error_Message = libghdl.errorout__memory__get_error_message_addr
Get_Error_Message.argstype = [c_int32]
Get_Error_Message.restype = c_char_p

Clear_Errors = libghdl.errorout__memory__clear_errors
