"use strict";

const ROM_EXTS = [".gb", ".gbc", ".sgb"];

function u16(bytes, offset) {
    return bytes[offset] | (bytes[offset + 1] << 8);
}

function u32(bytes, offset) {
    return (bytes[offset] | (bytes[offset + 1] << 8) | (bytes[offset + 2] << 16) | (bytes[offset + 3] << 24)) >>> 0;
}

function decodeName(bytes) {
    return new TextDecoder().decode(bytes);
}

function findEndOfCentralDirectory(bytes) {
    const min = Math.max(0, bytes.length - 0x10000 - 22);
    for (let i = bytes.length - 22; i >= min; i--) {
        if (u32(bytes, i) === 0x06054b50) return i;
    }
    throw new Error("ZIP extraction failed: end of central directory not found");
}

function findRomEntry(bytes) {
    const eocd = findEndOfCentralDirectory(bytes);
    const entryCount = u16(bytes, eocd + 10);
    let offset = u32(bytes, eocd + 16);

    for (let i = 0; i < entryCount; i++) {
        if (u32(bytes, offset) !== 0x02014b50) {
            throw new Error("ZIP extraction failed: invalid central directory");
        }
        const method = u16(bytes, offset + 10);
        const compressedSize = u32(bytes, offset + 20);
        const uncompressedSize = u32(bytes, offset + 24);
        const nameLen = u16(bytes, offset + 28);
        const extraLen = u16(bytes, offset + 30);
        const commentLen = u16(bytes, offset + 32);
        const localOffset = u32(bytes, offset + 42);
        const name = decodeName(bytes.subarray(offset + 46, offset + 46 + nameLen));

        if (ROM_EXTS.some(ext => name.toLowerCase().endsWith(ext))) {
            return {name, method, compressedSize, uncompressedSize, localOffset};
        }

        offset += 46 + nameLen + extraLen + commentLen;
    }

    throw new Error("No .gb, .gbc, or .sgb ROM found inside ZIP");
}

async function inflateRaw(bytes) {
    if (typeof DecompressionStream !== "function") {
        throw new Error("ZIP extraction failed: this browser cannot decompress deflated ZIP entries");
    }
    const stream = new Blob([bytes]).stream().pipeThrough(new DecompressionStream("deflate-raw"));
    return new Uint8Array(await new Response(stream).arrayBuffer());
}

async function readEntry(bytes, entry) {
    const local = entry.localOffset;
    if (u32(bytes, local) !== 0x04034b50) {
        throw new Error("ZIP extraction failed: invalid local file header");
    }
    const nameLen = u16(bytes, local + 26);
    const extraLen = u16(bytes, local + 28);
    const dataStart = local + 30 + nameLen + extraLen;
    const compressed = bytes.subarray(dataStart, dataStart + entry.compressedSize);

    if (entry.method === 0) return compressed.slice();
    if (entry.method === 8) return inflateRaw(compressed);
    throw new Error(`ZIP extraction failed: unsupported compression method ${entry.method}`);
}

export async function unzipFirstRom(file) {
    const bytes = new Uint8Array(await file.arrayBuffer());
    const entry = findRomEntry(bytes);
    const romBytes = await readEntry(bytes, entry);
    if (romBytes.length !== entry.uncompressedSize) {
        throw new Error("ZIP extraction failed: decompressed size mismatch");
    }
    return new File([romBytes], entry.name, {type: "application/octet-stream"});
}
