// Differential trace driver: runs a ROM in SameBoy with the execution callback
// set to a per-instruction state dumper, which emitts one line per instruction
// in the same format as Ocelot's tools/ocelot-trace tool. Pipe both into diff
// to find the first divergence.
//
// Output line format (whitespace-separated, fixed-width fields):
//   pc=XXXX af=XXXX bc=XXXX de=XXXX hl=XXXX sp=XXXX if=XX ie=XX ly=XXX lcdc=XX
//
// Usage:
//   sameboy-trace <rom> <instruction-count>
//
// Build via tools/Makefile (requires SameBoy core objects to be built;
// run `make -C external/SameBoy tester` first to populate
// `external/SameBoy/build/obj/Core/*.o`).

#define GB_INTERNAL
#include <Core/gb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static GB_gameboy_t gb;
static uint64_t instructions_remaining;
static FILE *trace_out;
static bool reached_cart;

// Minimal CGB boot stub. We write the I/O register values that the real
// CGB boot ROM leaves behind (LCDC=0x91, BGP=0xFC, OBP0/1=0xFF, NR50/51/52)
// then load the post-boot CPU register pair values, then unmap. The
// final byte sequence at 0xFE-0xFF is `E0 50` (LDH (FF50), A) so PC
// falls through to the cart's entry at 0x100. State matches Ocelot's
// 'cgbPostBoot' so any divergence in the trace is real emulator drift.
//
// This is hand-assembled. Bytes are located at the END of the 0x100
// region; the slack at the start is NOPs.
static const unsigned char boot_stub[0x100] = {
    // XOR A sets F=0x80 (Z=1, N=0, H=0, C=0) which is the Pan Docs CGB
    // post-boot F value; without this
    // the F register starts at 0x00 and any AF-comparing diff against Ocelot
    // drifts on instruction 1.
    [0x00] = 0xAF,          // XOR A
    [0x01 ... 0xD8] = 0x00, // NOP padding

    // LCDC = 0x91 (LCD on, BG on, BG tile data 0x8000, BG tilemap 0x9800)
    [0xD9] = 0x3E,
    [0xDA] = 0x91, // LD A, 0x91
    [0xDB] = 0xE0,
    [0xDC] = 0x40, // LDH (FF40), A
    // BGP = 0xFC
    [0xDD] = 0x3E,
    [0xDE] = 0xFC, // LD A, 0xFC
    [0xDF] = 0xE0,
    [0xE0] = 0x47, // LDH (FF47), A
    // OBP0 = 0xFF, OBP1 = 0xFF (A reused)
    [0xE1] = 0x3E,
    [0xE2] = 0xFF, // LD A, 0xFF
    [0xE3] = 0xE0,
    [0xE4] = 0x48, // LDH (FF48), A
    [0xE5] = 0xE0,
    [0xE6] = 0x49, // LDH (FF49), A
    // NR50 = 0x77
    [0xE7] = 0x3E,
    [0xE8] = 0x77, // LD A, 0x77
    [0xE9] = 0xE0,
    [0xEA] = 0x24, // LDH (FF24), A
    // NR51 = 0xF3
    [0xEB] = 0x3E,
    [0xEC] = 0xF3, // LD A, 0xF3
    [0xED] = 0xE0,
    [0xEE] = 0x25, // LDH (FF25), A
    // NR52 = 0x80 (APU on, no channels enabled)
    [0xEF] = 0x3E,
    [0xF0] = 0x80, // LD A, 0x80
    [0xF1] = 0xE0,
    [0xF2] = 0x26, // LDH (FF26), A
    // CPU register pairs (BC defaults to 0, H to 0).
    [0xF3] = 0x16,
    [0xF4] = 0xFF, // LD D, 0xFF
    [0xF5] = 0x1E,
    [0xF6] = 0x56, // LD E, 0x56
    [0xF7] = 0x2E,
    [0xF8] = 0x0D, // LD L, 0x0D
    [0xF9] = 0x31,
    [0xFA] = 0xFE,
    [0xFB] = 0xFF, // LD SP, 0xFFFE
    [0xFC] = 0x3E,
    [0xFD] = 0x11, // LD A, 0x11
    [0xFE] = 0xE0,
    [0xFF] = 0x50, // LDH (FF50), A   ; unmap, hand off
};

// Required no-op callbacks. SameBoy crashes mid-frame if some of these are NULL
// (specifically rgb_encode and vblank are exercised by the PPU once it leaves
// the LCD-off state).
static char *async_input_callback(GB_gameboy_t *unused) {
  (void)unused;
  return NULL;
}
static uint32_t rgb_encode(GB_gameboy_t *unused, uint8_t r, uint8_t g,
                           uint8_t b) {
  (void)unused;
  return ((uint32_t)r << 16) | ((uint32_t)g << 8) | b;
}
static void on_vblank(GB_gameboy_t *unused, GB_vblank_type_t type) {
  (void)unused;
  (void)type;
}
static void on_log(GB_gameboy_t *unused, const char *msg,
                   GB_log_attributes_t attrs) {
  (void)unused;
  (void)msg;
  (void)attrs;
}

static void on_instruction(GB_gameboy_t *gb, uint16_t pc, uint8_t opcode) {
  (void)opcode;
  if (instructions_remaining == 0)
    return;
  // Skip the boot-stub instructions; only emit trace lines once we reach the
  // cart's entry point at 0x100, so the trace lines up 1:1 with Ocelot's
  // starting from cgbPostBoot.
  if (!reached_cart) {
    if (pc != 0x0100)
      return;
    reached_cart = true;
  }
  instructions_remaining--;
  GB_registers_t *r = GB_get_registers(gb);
  uint8_t iflag = GB_read_memory(gb, 0xFF0F);
  uint8_t ie = GB_read_memory(gb, 0xFFFF);
  uint8_t ly = GB_read_memory(gb, 0xFF44);
  uint8_t lcdc = GB_read_memory(gb, 0xFF40);
  fprintf(trace_out,
          "pc=%04X af=%04X bc=%04X de=%04X hl=%04X sp=%04X if=%02X ie=%02X "
          "ly=%03d lcdc=%02X\n",
          pc, r->af, r->bc, r->de, r->hl, r->sp, iflag, ie, ly, lcdc);
}

int main(int argc, char **argv) {
  if (argc != 3) {
    fprintf(stderr, "usage: sameboy-trace <rom> <instruction-count>\n");
    return 2;
  }
  const char *rom_path = argv[1];
  uint64_t target = strtoull(argv[2], NULL, 10);
  if (target == 0) {
    fprintf(stderr, "instruction-count must be > 0\n");
    return 2;
  }
  instructions_remaining = target;
  trace_out = stdout;

  // CGB model. Real games we care about are CGB-only; running them in
  // DMG mode would diverge from Ocelot trivially.
  GB_init(&gb, GB_MODEL_CGB_E);
  GB_set_async_input_callback(&gb, async_input_callback);
  GB_set_rgb_encode_callback(&gb, rgb_encode);
  GB_set_vblank_callback(&gb, on_vblank);
  GB_set_log_callback(&gb, on_log);
  static uint32_t pixels[256 * 224];
  GB_set_pixels_output(&gb, pixels);
  GB_set_sample_rate(&gb, 0);
  setbuf(stdout, NULL);

  if (GB_load_rom(&gb, rom_path) != 0) {
    fprintf(stderr, "could not load ROM: %s\n", rom_path);
    return 1;
  }

  GB_load_boot_rom_from_buffer(&gb, boot_stub, sizeof(boot_stub));
  GB_set_execution_callback(&gb, on_instruction);

  while (instructions_remaining > 0) {
    GB_run(&gb);
  }

  fflush(trace_out);
  return 0;
}
