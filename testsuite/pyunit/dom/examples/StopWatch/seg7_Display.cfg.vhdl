-- Author:  Patrick Lehmann
-- License: MIT
--
-- A generic counter module used in the StopWatch example.
--
context work.StopWatch_ctx;


-- Encoder that translates from 4-bit binary (BCD) to 7-segment code.
configuration seg7_Display_cfg of seg7_Display is
	for rtl
		for enc : seg7_Encoder
			use entity work.seg7_Encoder(rtl);
		end for;
	end for;
end configuration;
