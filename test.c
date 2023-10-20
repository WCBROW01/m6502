#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>
#include <stdatomic.h>

#include "cpu.h"
#include "instructions.h"

static int ram_write_status = 0;

uint8_t ram_read(void *mem, uint16_t addr) {
	return ((uint8_t *)mem)[addr];
}

void ram_write(void *mem, uint16_t addr, uint8_t data) {
	ram_write_status = 1;
	((uint8_t *)mem)[addr] = data;
}

int main(void) {
	const uint8_t code[] = {
		0xA9, 0x21, 0x85, 0x7F, 0xA9, 0x7E, 0xC5, 0x7F,
		0xF0, 0x05, 0xE6, 0x7F, 0x4C, 0x04, 0x02, 0xA9,
		0x21, 0x85, 0x7F, 0x4C, 0x04, 0x02
	};

	uint8_t ram[65536];
	memcpy(ram + 0x200, code, sizeof(code));
	ram[0xfffc] = 0x00;
	ram[0xfffd] = 0x02;
	CPU cpu = {
		.context = ram,
		.read = &ram_read,
		.write = &ram_write
	};
	CPU_power(&cpu);
	CPU_reset(&cpu);

	while (1) {
		CPU_run(&cpu, 1);
		if (ram_write_status) {
			printf("%c", ram[0x7f]);
			ram_write_status = 0;
		}
		fflush(stdout);
	}

	return 0;
}
