#ifndef M6502_CPU
#define M6502_CPU

#include <stdint.h>

enum status_flags {
	C = 1 << 0,
	Z = 1 << 1,
	I = 1 << 2,
	D = 1 << 3,
	B = 1 << 4,
	V = 1 << 6,
	N = 1 << 7
};

// read and write must be initialized before reseting the CPU.
typedef struct CPU {
	size_t cycles; // number of cycles executed
	void *context; // context for memory address bus
	uint8_t (*read)(void *, uint16_t); // read from memory
	void (*write)(void *, uint16_t, uint8_t); // write to memory
	uint16_t pc; // program counter
	uint16_t addr; // address bus
	uint8_t s; // stack pointer
	int8_t a, x, y; // registers
	uint8_t p; // status flag
	uint8_t irq; // interrupt line
	uint8_t nmi; // nmi line
} CPU;

void CPU_power(CPU *cpu);
void CPU_reset(CPU *cpu);
size_t CPU_run(CPU *cpu, size_t cycles);

void stack_push(CPU *cpu, uint8_t data);
uint8_t stack_pull(CPU *cpu);
void interrupt(CPU *cpu);


// Function pointer type for address mode functions
typedef uint16_t (*addr_mode)(CPU *);

uint8_t load(CPU *cpu, addr_mode mode);
void store(CPU *cpu, uint16_t addr, uint8_t data);

uint16_t addr_acc(CPU *cpu);
uint16_t addr_imp(CPU *cpu);
uint16_t addr_imm(CPU *cpu);
uint16_t addr_zpg(CPU *cpu);
uint16_t addr_zpx(CPU *cpu);
uint16_t addr_zpy(CPU *cpu);
uint16_t addr_rel(CPU *cpu);
uint16_t addr_abs(CPU *cpu);
uint16_t addr_abx(CPU *cpu);
uint16_t addr_aby(CPU *cpu);
uint16_t addr_abi(CPU *cpu);
uint16_t addr_inx(CPU *cpu);
uint16_t addr_iny(CPU *cpu);

#endif
