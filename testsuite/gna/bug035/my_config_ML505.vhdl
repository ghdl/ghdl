-- EMACS settings: -*-  tab-width: 2; indent-tabs-mode: t -*-
-- vim: tabstop=2:shiftwidth=2:noexpandtab
-- kate: tab-width 2; replace-tabs off; indent-width 2;
-- 
-- =============================================================================
-- Authors:         Thomas B. Preusser
--                  Martin Zabel
--                  Patrick Lehmann
--
-- Package:					Project specific configuration.
-- 
-- Description:
-- ------------------------------------
--		This file was created from template <PoCRoot>/src/common/my_config.template.vhdl.
--		
--
-- License:
-- =============================================================================
-- Copyright 2007-2015 Technische Universitaet Dresden - Germany,
--										 Chair for VLSI-Design, Diagnostics and Architecture
-- 
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--    http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- =============================================================================

library PoC;


package my_config is
  -- Change these lines to setup configuration.
  constant MY_BOARD		: string	:= "ML505";			-- ML505 - Xilinx Virtex 5 reference design board: XC5VLX50T
  constant MY_DEVICE	: string	:= "None";			-- infer from MY_BOARD
		
	-- For internal use only
	constant MY_VERBOSE	: boolean	:= FALSE;
end package;
