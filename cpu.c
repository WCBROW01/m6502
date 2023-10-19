#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#include "cpu.h"
#include "instructions.h"

uint16_t addr_acc(CPU *cpu) {
	return 0;
}

// used for implicit addressing to do nothing
uint16_t addr_imp(CPU *cpu) {
	return 0;
}

uint16_t addr_imm(CPU *cpu) {
	cpu->addr = cpu->pc++;
	return cpu->addr;
}

uint16_t addr_zpg(CPU *cpu) {
	cpu->addr = load(cpu, addr_imm);
	return cpu->addr;
}

uint16_t addr_zpx(CPU *cpu) {
    cpu->addr = load(cpu, addr_imm) + cpu->x;
	++cpu->cycles;
	return cpu->addr;
}

uint16_t addr_zpy(CPU *cpu) {
    cpu->addr = load(cpu, addr_imm) + cpu->y;
	++cpu->cycles;
	return cpu->addr;
}

uint16_t addr_rel(CPU *cpu) {
	uint8_t offset = load(cpu, addr_imm);
	if ((cpu->pc & 0xFF) + offset > 0xFF) ++cpu->cycles;
	cpu->addr = cpu->pc + offset;
	return cpu->addr;
}

uint16_t addr_abs(CPU *cpu) {
    cpu->addr = load(cpu, addr_imm) | load(cpu, addr_imm) << 8;
	return cpu->addr;
}

uint16_t addr_abx(CPU *cpu) {
	uint8_t addr_tmp[2] = {load(cpu, addr_imm), load(cpu, addr_imm)};
	uint16_t addr = addr_tmp[0] | (addr_tmp[1] << 8);

	if (((addr + cpu->x) & (1 << 8)) < (addr & (1 << 8))) ++cpu->cycles;
	cpu->addr = addr + cpu->x;
	return cpu->addr;
}

uint16_t addr_aby(CPU *cpu) {
	uint8_t addr_tmp[2] = {load(cpu, addr_imm), load(cpu, addr_imm)};
	uint16_t addr = addr_tmp[0] | (addr_tmp[1] << 8);

	if (((addr + cpu->y) & (1 << 8)) < (addr & (1 << 8))) ++cpu->cycles;
	cpu->addr = addr + cpu->y;
	return cpu->addr;
}

uint16_t addr_abi(CPU *cpu) {
	uint16_t ind_addr = addr_abs(cpu);
	cpu->addr = cpu->read(cpu->context, ind_addr) | (cpu->read(cpu->context, ind_addr + 1) << 8);
	cpu->cycles += 2;
	return cpu->addr;
}

uint16_t addr_inx(CPU *cpu) {
	uint16_t ind_addr = addr_zpx(cpu);
	cpu->addr = cpu->read(cpu->context, ind_addr & 0xFF);
	cpu->cycles += 2;
	return cpu->addr;
}

uint16_t addr_iny(CPU *cpu) {
	uint16_t ind_addr = addr_zpg(cpu);
	uint16_t addr = cpu->read(cpu->context, ind_addr);
	++cpu->cycles;

	if (((addr + cpu->y) & (1 << 8)) < (addr & (1 << 8))) ++cpu->cycles;
	cpu->addr = addr + cpu->y;
	return cpu->addr;
}

uint8_t load(CPU *cpu, addr_mode mode) {
	++cpu->cycles;
	return cpu->read(cpu->context, mode(cpu));
}

void store(CPU *cpu, uint16_t addr, uint8_t data) {
	++cpu->cycles;
	cpu->write(cpu->context, addr, data);
}

void stack_push(CPU *cpu, uint8_t data) {
	cpu->addr = 0x100 + cpu->s--;
	++cpu->cycles;
	cpu->write(cpu->context, cpu->addr, data);
}

uint8_t stack_pull(CPU *cpu) {
	cpu->addr = 0x100 + ++cpu->s;
	cpu->cycles += 2;
	return cpu->read(cpu->context, cpu->addr);
}

void interrupt(CPU *cpu) {
	stack_push(cpu, cpu->pc >> 8);
	stack_push(cpu, cpu->pc);
	stack_push(cpu, cpu->p);
}

void CPU_reset(CPU *cpu) {
	cpu->p |= I;
	cpu->pc = 0x00FF;
	cpu->s = 0x00;
	interrupt(cpu);
	cpu->addr = 0xFFFC;
	cpu->pc = cpu->read(cpu->context, cpu->addr);
	cpu->pc |= cpu->read(cpu->context, ++cpu->addr) << 8;
}

size_t CPU_run(CPU *cpu, size_t cycles) {
	cpu->cycles = 0;
	while (cpu->cycles < cycles) {
		uint8_t opcode = load(cpu, addr_imm);
		opcode_t inst = OPCODE_TABLE[opcode];
		inst.func(cpu, inst.mode);
	}

	return cpu->cycles;
}
