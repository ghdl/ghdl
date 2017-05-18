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
--		This is a template file. 
--		
--		The global packages common/config and common/board evaluate the settings
--		declared in this file.
--
--		USAGE:
--			1) Copy this file into your project's source directory and rename it to
--				 "my_config.vhdl".
--			2) Add file to library "PoC" in your synthesis tool.
--			3) Change setup appropriately.
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
	constant MY_BOARD		: string	:= "ML505"; -- e.g. Custom, ML505, KC705, Atlys
	constant MY_DEVICE	: string	:= "XC5VLX50T-1FF1136"; -- e.g. None, XC5VLX50T-1FF1136, EP2SGX90FF1508C3
	
	-- For internal use only
	constant MY_VERBOSE	: boolean	:= FALSE;					-- activate detailed report statements in functions and procedures
end package;
