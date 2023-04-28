#include "wrapper.h"

void wdf_driver_config_init(
    PWDF_DRIVER_CONFIG Config,
    PFN_WDF_DRIVER_DEVICE_ADD EvtDriverDeviceAdd)
{
  WDF_DRIVER_CONFIG_INIT(Config, EvtDriverDeviceAdd);
}
