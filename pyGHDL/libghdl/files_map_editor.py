from ctypes import c_int32, c_char_p, c_bool
from libghdl import libghdl

Replace_Text = libghdl.files_map__editor__replace_text_ptr
Replace_Text.argstype = [c_int32, c_int32, c_int32, c_int32, c_char_p, c_int32]
Replace_Text.restype = c_bool

Fill_Text = libghdl.files_map__editor__fill_text_ptr

Check_Buffer_Content = libghdl.files_map__editor__check_buffer_content

Copy_Source_File = libghdl.files_map__editor__copy_source_file
