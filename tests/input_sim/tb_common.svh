`ifndef TB_COMMON_SVH
`define TB_COMMON_SVH

`define ASSERT(cond) assert(cond) else $fatal(1, "Assertion failed: %s:%0d: %s", `__FILE__, `__LINE__, `"cond`")

`define START_DUMP(tb_name) \
	if ('1) begin \
		string vcd_path; \
		if (!$value$plusargs("DUMP_PATH=%s", vcd_path)) \
			$fatal(1, "DUMP_PATH not specified"); \
		$display("DUMP_PATH=%s", vcd_path); \
		$dumpfile(vcd_path); \
		$dumpvars(0, tb_name); \
	end

`endif
