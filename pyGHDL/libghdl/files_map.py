from libghdl import libghdl
from ctypes import c_void_p

EOT = b"\x04"

No_Source_File_Entry = 0

No_Location = 0

Location_To_File = libghdl.files_map__location_to_file

Location_File_To_Pos = libghdl.files_map__location_file_to_pos

Location_File_To_Line = libghdl.files_map__location_file_to_line

Location_File_Line_To_Offset = libghdl.files_map__location_file_line_to_offset

Location_File_Line_To_Col = libghdl.files_map__location_file_line_to_col

File_To_Location = libghdl.files_map__file_to_location

File_Pos_To_Location = libghdl.files_map__file_pos_to_location

File_Line_To_Position = libghdl.files_map__file_line_to_position

Get_File_Name = libghdl.files_map__get_file_name

Get_Directory_Name = libghdl.files_map__get_directory_name

Get_File_Buffer = libghdl.files_map__get_file_buffer
Get_File_Buffer.restype = c_void_p

Get_File_Length = libghdl.files_map__get_file_length
Set_File_Length = libghdl.files_map__set_file_length

Read_Source_File = libghdl.files_map__read_source_file

Reserve_Source_File = libghdl.files_map__reserve_source_file

Discard_Source_File = libghdl.files_map__discard_source_file
Free_Source_File = libghdl.files_map__free_source_file

Get_Last_Source_File_Entry = libghdl.files_map__get_last_source_file_entry
