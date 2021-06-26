#include <vpi_user.h>
#include <inttypes.h>
#include <stdio.h>

#define STOP_ITERATION 5

static uint32_t iteration = 0;

static const char* const input_values[5] = {"000",
					    "001",
					    "010",
					    "011",
					    "100"};

static vpiHandle enum_in_handle;
static vpiHandle enum_out_handle;
static vpiHandle enum_decoded_handle;

static PLI_INT32 start_cb(p_cb_data);
static PLI_INT32 end_cb(p_cb_data);
static PLI_INT32 rw_cb(p_cb_data);
static PLI_INT32 ro_cb(p_cb_data);
static PLI_INT32 delay_rw_cb(p_cb_data);
static PLI_INT32 delay_ro_cb(p_cb_data);

static void register_cb(PLI_INT32(*f)(p_cb_data),
			PLI_INT32 reason,
			int64_t cycles)
{

    s_cb_data cbData;
    s_vpi_time simuTime;
    if (cycles < 0){
        cbData.time = NULL;
    } else {
        cbData.time = &simuTime;
        simuTime.type = vpiSimTime;
        simuTime.high = (PLI_INT32) (cycles >> 32);
        simuTime.low = (PLI_INT32) (cycles & 0xFFFFFFFF);
    }

    cbData.reason = reason;
    cbData.cb_rtn = f;
    cbData.user_data = 0;
    cbData.value = 0;

    vpi_register_cb(&cbData);
}

static void entry_point_cb() {
    register_cb(start_cb, cbStartOfSimulation, -1);
    register_cb(end_cb, cbEndOfSimulation, -1);
    register_cb(delay_ro_cb, cbAfterDelay, 0);
}

static PLI_INT32 start_cb(p_cb_data data){
    (void) data;
    printf("Start of simulation \n");

    enum_in_handle = vpi_handle_by_name("enum_test.enum_in", NULL);
    if(!enum_in_handle) printf("enum in not found\n");
    enum_out_handle = vpi_handle_by_name("enum_test.enum_out", NULL);
    if(!enum_out_handle) printf("enum out not found\n");
    enum_decoded_handle = vpi_handle_by_name("enum_test.enum_decoded", NULL);
    if(!enum_decoded_handle) printf("enum decoded not found\n");

    return 0;
}

static PLI_INT32 end_cb(p_cb_data data){
    (void) data;
    printf("End of simulation %u \n", iteration);
    return 0;
}


static PLI_INT32 rw_cb(p_cb_data data){
    (void) data;
    s_vpi_value val;
    val.format = vpiBinStrVal;

    if(iteration > 0) {
        vpi_get_value(enum_decoded_handle, &val);
        val.format = vpiBinStrVal;
        printf("enum decoded = %s iteration %u \n", val.value.str, iteration);
        vpi_get_value(enum_out_handle, &val);
        printf("enum out = %s iteration %u \n", val.value.str, iteration);

        val.format = vpiBinStrVal;
    }

    if(iteration < STOP_ITERATION) {

        val.value.str = (char *) input_values[iteration];
        printf("enum in <= %s iteration %u \n", val.value.str, iteration);
        vpi_put_value(enum_in_handle, &val, NULL, vpiNoDelay);
        register_cb(delay_ro_cb, cbAfterDelay, 1);
    } else {
        vpi_control(vpiFinish, 0);
    }

    iteration++;
    return 0;
}

static PLI_INT32 ro_cb(p_cb_data data){
    (void) data;
    register_cb(delay_rw_cb, cbAfterDelay, 0);
    return 0;
}

static PLI_INT32 delay_rw_cb(p_cb_data data){
    (void) data;
    register_cb(rw_cb, cbReadWriteSynch, 0);
    return 0;
}

static PLI_INT32 delay_ro_cb(p_cb_data data){
    (void) data;
    register_cb(ro_cb, cbReadOnlySynch, 0);
    return 0;
}

void (*vlog_startup_routines[]) () = {
    entry_point_cb,
    0
};
