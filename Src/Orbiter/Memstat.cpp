// Copyright (c) Martin Schweiger
// Licensed under the MIT License

#include "Memstat.h"
#include <mach/mach.h>

MemStat::MemStat ()
{
}

MemStat::~MemStat ()
{
}

long MemStat::HeapUsage ()
{
	task_vm_info_data_t info;
	mach_msg_type_number_t count = TASK_VM_INFO_COUNT;
	if (task_info (mach_task_self (), TASK_VM_INFO, (task_info_t)&info, &count) != KERN_SUCCESS)
		return 0;
	return (long)info.phys_footprint;
}
