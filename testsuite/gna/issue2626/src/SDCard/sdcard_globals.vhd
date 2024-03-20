library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

package sdcard_globals is

   -- From Part1_Physical_Layer_Simplified_Specification_Ver8.00.pdf,
   -- Section 4.7.4 Detail Command Description, Page 117

   -- Class 0 Basic Commands                              -- Expected response:
   constant C_CMD_GO_IDLE_STATE            : natural                        := 0;   -- none
   constant C_CMD_ALL_SEND_CID             : natural                        := 2;   -- R2
   constant C_CMD_SEND_RELATIVE_ADDR       : natural                        := 3;   -- R6
   constant C_CMD_SET_DSR                  : natural                        := 4;   -- none
   constant C_CMD_SELECT_CARD              : natural                        := 7;   -- R1b
   constant C_CMD_SEND_IF_COND             : natural                        := 8;   -- R7 or none
   constant C_CMD_SEND_CSD                 : natural                        := 9;   -- R2
   constant C_CMD_SEND_CID                 : natural                        := 10;  -- R2
   constant C_CMD_VOLTAGE_SWITCH           : natural                        := 11;  -- R1
   constant C_CMD_STOP_TRANSMISSION        : natural                        := 12;  -- R1b
   constant C_CMD_SEND_STATUS              : natural                        := 13;  -- R1
   constant C_CMD_GO_INACTIVE_STATE        : natural                        := 15;  -- none

   -- Class 2 and 4 Block Commands                        -- Expected response:
   constant C_CMD_SET_BLOCKLEN             : natural                        := 16;  -- R1
   constant C_CMD_READ_SINGLE_BLOCK        : natural                        := 17;  -- R1
   constant C_CMD_READ_MULTIPLE_BLOCK      : natural                        := 18;  -- R1
   constant C_CMD_SET_TUNING_BLOCK         : natural                        := 19;  -- R1
   constant C_CMD_SPEED_CLASS_CONTROL      : natural                        := 20;  -- R1b
   constant C_CMD_ADDRESS_EXTENSION        : natural                        := 22;  -- R1
   constant C_CMD_SET_BLOCK_COUNT          : natural                        := 23;  -- R1
   constant C_CMD_WRITE_BLOCK              : natural                        := 24;  -- R1
   constant C_CMD_WRITE_MULTIPLE_BLOCK     : natural                        := 25;  -- R1
   constant C_CMD_PROGRAM_CSD              : natural                        := 27;  -- R1

   -- Class 6 Write Protection Commands                   -- Expected response:
   constant C_CMD_SET_WRITE_PROT           : natural                        := 28;  -- R1b
   constant C_CMD_CLR_WRITE_PROT           : natural                        := 29;  -- R1b
   constant C_CMD_SEND_WRITE_PROT          : natural                        := 30;  -- R1

   -- Class 5 Erase Commands                              -- Expected response:
   constant C_CMD_ERASE_WR_BLK_START       : natural                        := 32;  -- R1
   constant C_CMD_ERASE_WR_BLK_END         : natural                        := 33;  -- R1
   constant C_CMD_ERASE                    : natural                        := 38;  -- R1b

   -- Class 8 Application Specific Commands               -- Expected response:
   constant C_CMD_APP_CMD                  : natural                        := 55;  -- R1
   constant C_CMD_GEN_CMD                  : natural                        := 56;  -- R1

   -- Class 10 Switch Function Commands                   -- Expected response:
   constant C_CMD_SWITCH_FUNC              : natural                        := 6;   -- R1

   -- Application Specific Commands                       -- Expected response:
   constant C_ACMD_SET_BUS_WIDTH           : natural                        := 6;   -- R1
   constant C_ACMD_SD_STATUS               : natural                        := 13;  -- R1
   constant C_ACMD_SET_NUM_WR_BLOCKS       : natural                        := 22;  -- R1
   constant C_ACMD_SET_WR_BLK_ERASE_COUNT  : natural                        := 23;  -- R1
   constant C_ACMD_SD_SEND_OP_COND         : natural                        := 41;  -- R3
   constant C_ACMD_SET_CLR_CARD_DETECT     : natural                        := 42;  -- R1
   constant C_ACMD_SEND_SCR                : natural                        := 51;  -- R1


   -- Section 4.3.13, Page 105
   -- Response R7
   subtype  r_cmd8_1_2v                 is natural range  13 downto  13;            -- PCIe 1.2V Support
   subtype  r_cmd8_pcie                 is natural range  12 downto  12;            -- PCIe Availability
   subtype  r_cmd8_vhs                  is natural range  11 downto   8;            -- Supply Voltage
   subtype  r_cmd8_check                is natural range   7 downto   0;            -- Check pattern
   constant C_CMD8_VHS_27_36               : std_logic_vector(3 downto 0)   := "0001";
   constant C_CMD8_VHS_LOW                 : std_logic_vector(3 downto 0)   := "0010";

   subtype  r_cmd_rca                   is natural range  31 downto  16;            -- RCA
   constant C_CMD_RCA_DEFAULT              : std_logic_vector(31 downto 16) := (others => '0');

   subtype  r_acmd6_bus_width           is natural range   1 downto   0;
   constant C_ACMD6_BUS_WIDTH_1            : std_logic_vector(1 downto 0)   := "00";
   constant C_ACMD6_BUS_WIDTH_4            : std_logic_vector(1 downto 0)   := "10";



   -- From Part1_Physical_Layer_Simplified_Specification_Ver8.00.pdf,
   -- Section 4.9 Responses, Page 131
   constant C_RESP_R1_LEN                  : natural                        := 48;  -- Normal response
   constant C_RESP_R2_LEN                  : natural                        := 136; -- CID, CSD register
   constant C_RESP_R3_LEN                  : natural                        := 48;  -- OCR register
   constant C_RESP_R6_LEN                  : natural                        := 48;  -- Published RCA response
   constant C_RESP_R7_LEN                  : natural                        := 48;  -- Card interface condition


   -- From Part1_Physical_Layer_Simplified_Specification_Ver8.00.pdf,
   -- Section 4.10.1 Card Status, Page 134
   subtype  r_cmd_index                 is natural range 39 downto 32;
   constant C_CARD_STAT_OUT_OF_RANGE       : natural                        := 31;
   constant C_CARD_STAT_ADDRESS_ERROR      : natural                        := 30;
   constant C_CARD_STAT_BLOCK_LEN_ERROR    : natural                        := 29;
   constant C_CARD_STAT_ERASE_SEQ_ERROR    : natural                        := 28;
   constant C_CARD_STAT_ERASE_PARAM        : natural                        := 27;
   constant C_CARD_STAT_WP_VIOLATION       : natural                        := 26;
   constant C_CARD_STAT_CARD_IS_LOCKED     : natural                        := 25;
   constant C_CARD_STAT_LOCK_UNLOCK_FAILED : natural                        := 24;
   constant C_CARD_STAT_COM_CRC_ERROR      : natural                        := 23;
   constant C_CARD_STAT_ILLEGAL_COMMAND    : natural                        := 22;
   constant C_CARD_STAT_CARD_ECC_FAILED    : natural                        := 21;
   constant C_CARD_STAT_CC_ERROR           : natural                        := 20;
   constant C_CARD_STAT_ERROR              : natural                        := 19;
   constant C_CARD_STAT_CSD_OVERWRITE      : natural                        := 16;
   constant C_CARD_STAT_WP_ERASE_SKIP      : natural                        := 15;
   constant C_CARD_STAT_CARD_ECC_DISABED   : natural                        := 14;
   constant C_CARD_STAT_ERASE_RESET        : natural                        := 13;
   subtype  r_card_stat_current_state     is natural range 12 downto 9;
   constant C_CARD_STAT_READY_FOR_DATA     : natural                        := 8;
   constant C_CARD_STAT_FX_EVENT           : natural                        := 6;
   constant C_CARD_STAT_APP_CMD            : natural                        := 5;
   constant C_CARD_STAT_AKE_SEQ_ERROR      : natural                        := 3;

   constant C_CARD_STATE_IDLE              : std_logic_vector(3 downto 0)   := X"0";
   constant C_CARD_STATE_READY             : std_logic_vector(3 downto 0)   := X"1";
   constant C_CARD_STATE_IDENT             : std_logic_vector(3 downto 0)   := X"2";
   constant C_CARD_STATE_STBY              : std_logic_vector(3 downto 0)   := X"3";
   constant C_CARD_STATE_TRAN              : std_logic_vector(3 downto 0)   := X"4";
   constant C_CARD_STATE_DATA              : std_logic_vector(3 downto 0)   := X"5";
   constant C_CARD_STATE_RCV               : std_logic_vector(3 downto 0)   := X"6";
   constant C_CARD_STATE_PRG               : std_logic_vector(3 downto 0)   := X"7";
   constant C_CARD_STATE_DIS               : std_logic_vector(3 downto 0)   := X"8";


   -- From Part1_Physical_Layer_Simplified_Specification_Ver8.00.pdf,
   -- Section 5 Card Registers, Page 204

   -- Response R2
   subtype  r_r2_cid                    is natural range 127 downto   0;
   subtype  r_cid_mid                   is natural range 127 downto 120;            -- Manufacturer ID
   subtype  r_cid_oid                   is natural range 119 downto 104;            -- OEM/Application ID
   subtype  r_cid_pnm                   is natural range 103 downto  64;            -- Product name
   subtype  r_cid_prv                   is natural range  63 downto  56;            -- Product revision
   subtype  r_cid_psn                   is natural range  55 downto  24;            -- Product serial number
   subtype  r_cid_mdt                   is natural range  19 downto   8;            -- Manufacturing date
   subtype  r_cid_crc                   is natural range   7 downto   1;            -- CRC7 checksum

   -- Response R2
   subtype  r_r2_csd                    is natural range 127 downto   0;
   subtype  r_csd_csd_structure         is natural range 127 downto 126;            -- CSD structure
   subtype  r_csd_taac                  is natural range 119 downto 112;            -- data read access-time-1
   subtype  r_csd_nsac                  is natural range 111 downto 104;            -- data read access-time-2
   subtype  r_csd_tran_speed            is natural range 103 downto  96;            -- max. data transfer rate
   subtype  r_csd_ccc                   is natural range  95 downto  84;            -- card command classes
   subtype  r_csd_read_bl_len           is natural range  83 downto  80;            -- max. read data block length
   subtype  r_csd_read_bl_partial       is natural range  79 downto  79;            -- partial blocks for read allowed
   subtype  r_csd_write_blk_misalign    is natural range  78 downto  78;            -- write block misalignment
   subtype  r_csd_read_blk_misalign     is natural range  77 downto  77;            -- read block misalignment
   subtype  r_csd_dsr_imp               is natural range  76 downto  76;            -- DSR implemented
   subtype  r_csd_c_size                is natural range  73 downto  62;            -- device size
   subtype  r_csd_vdd_r_curr_min        is natural range  61 downto  59;            -- max. read current @ VDD min
   subtype  r_csd_vdd_r_curr_max        is natural range  58 downto  56;            -- max. read current @ VDD max
   subtype  r_csd_vdd_w_curr_min        is natural range  55 downto  53;            -- max. write current @ VDD min
   subtype  r_csd_vdd_w_curr_max        is natural range  52 downto  50;            -- max. write current @ VDD max
   subtype  r_csd_c_size_mult           is natural range  49 downto  47;            -- device size multiplier
   subtype  r_csd_erase_blk_en          is natural range  46 downto  46;            -- erase single block enable
   subtype  r_csd_sector_size           is natural range  45 downto  39;            -- erase sector size
   subtype  r_csd_wp_grp_size           is natural range  38 downto  32;            -- write protect group size
   subtype  r_csd_wp_grp_enable         is natural range  31 downto  31;            -- write protect group enable
   subtype  r_csd_r2w_factor            is natural range  28 downto  26;            -- write speed factor
   subtype  r_csd_write_bl_len          is natural range  25 downto  22;            -- max. write data block length
   subtype  r_csd_write_bl_partial      is natural range  21 downto  21;            -- partial blocks for write allowed
   subtype  r_csd_file_format_grp       is natural range  15 downto  15;            -- File format group
   subtype  r_csd_copy                  is natural range  14 downto  14;            -- copy flag
   subtype  r_csd_perm_write_protect    is natural range  13 downto  13;            -- permanent write protection
   subtype  r_csd_tmp_write_protect     is natural range  12 downto  12;            -- temporary write protection
   subtype  r_csd_file_format           is natural range  11 downto  10;            -- File format
   subtype  r_csd_crc                   is natural range   7 downto   1;            -- CRC

   -- ACMD41
   constant C_ACMD41_OCR_27X               : natural                        := 15;
   constant C_ACMD41_OCR_28X               : natural                        := 16;
   constant C_ACMD41_OCR_29X               : natural                        := 17;
   constant C_ACMD41_OCR_30X               : natural                        := 18;
   constant C_ACMD41_OCR_31X               : natural                        := 19;
   constant C_ACMD41_OCR_32X               : natural                        := 20;
   constant C_ACMD41_OCR_33X               : natural                        := 21;
   constant C_ACMD41_OCR_34X               : natural                        := 22;
   constant C_ACMD41_OCR_35X               : natural                        := 23;
   constant C_ACMD41_S18R                  : natural                        := 24;  -- Switching to 1.8V Request
   constant C_ACMD41_XPC                   : natural                        := 28;  -- SDXC Power Control
   constant C_ACMD41_HCS                   : natural                        := 30;  -- Host Capacity Support

   -- Response R3
   constant C_R3_OCR_27X                   : natural                        := 15;
   constant C_R3_OCR_28X                   : natural                        := 16;
   constant C_R3_OCR_29X                   : natural                        := 17;
   constant C_R3_OCR_30X                   : natural                        := 18;
   constant C_R3_OCR_31X                   : natural                        := 19;
   constant C_R3_OCR_32X                   : natural                        := 20;
   constant C_R3_OCR_33X                   : natural                        := 21;
   constant C_R3_OCR_34X                   : natural                        := 22;
   constant C_R3_OCR_35X                   : natural                        := 23;
   constant C_R3_S18A                      : natural                        := 24;  -- Switching to 1.8V Accepted
   constant C_R3_UHSII                     : natural                        := 29;  -- UHS-II Card Status
   constant C_R3_CCS                       : natural                        := 30;  -- Card Capacity Status
   constant C_R3_BUSY                      : natural                        := 31;  -- Card power up status bit

   -- Response R6
   subtype  r_r6_rca                    is natural range  31 downto  16;            -- RCA
   constant C_R6_STAT_COM_CRC_ERROR        : natural                        := 15;
   constant C_R6_STAT_ILLEGAL_COMMAND      : natural                        := 14;
   constant C_R6_STAT_ERROR                : natural                        := 13;
   subtype  r_r6_stat_current_state     is natural range 12 downto 9;
   constant C_R6_STAT_READY_FOR_DATA       : natural                        := 8;
   constant C_R6_STAT_FX_EVENT             : natural                        := 6;
   constant C_R6_STAT_APP_CMD              : natural                        := 5;
   constant C_R6_STAT_AKE_SEQ_ERROR        : natural                        := 3;

   subtype  r_scr_scr_structure         is natural range  63 downto  60;            -- SCR Structure
   subtype  r_scr_sd_spec               is natural range  59 downto  56;            -- SD Memory Card - Spec. Version
   subtype  r_scr_data_stat_after_erase is natural range  55 downto  55;            -- data_status_after erases
   subtype  r_scr_sd_security           is natural range  54 downto  52;            -- CPRM Security Support
   subtype  r_scr_sd_bus_widths         is natural range  51 downto  48;            -- DAT Bus widths supported
   subtype  r_scr_sd_spec3              is natural range  47 downto  47;            -- Spec. Version 3.00 or higher
   subtype  r_scr_ex_security           is natural range  46 downto  43;            -- Extended Security Support
   subtype  r_scr_sd_spec4              is natural range  42 downto  42;            -- Spec. Version 4.00 or higher
   subtype  r_scr_sd_specx              is natural range  41 downto  38;            -- Spec. Version 5.00 or higher
   subtype  r_scr_cmd_support           is natural range  35 downto  32;            -- Command Support bits

end package sdcard_globals;

