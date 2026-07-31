/* Target backlog, in samples (L and R counted separately).

The producer delivers one emulated frame of audio at a time, roughly every
16.7 ms, while this node drains smoothly in 128-frame quanta. The backlog has
to absorb that burstiness, so aim for a few producer periods: ~2048 stereo
frames is about 43 ms. */
const TARGET_FILL_SAMPLES = 6144;

// Fraction of the target the backlog may wander before the resampler reacts.
// The producer arrives in ~1600-sample bursts, so a little slack here stops the
// ratio from being modulated by ordinary burstiness.
const FILL_DEADBAND = 0.1;

/* Hard cap on the resampling nudge, reached when the backlog is empty or at
twice target.

This has to exceed the worst clock mismatch it must absorb, or the controller
saturates and the backlog runs away regardless: at ±0.5% a 1%-fast producer
grew the backlog to 0.37 s, and a 1%-slow one underran a thousand times a
minute. The correction is proportional, so a realistic sub-1% mismatch settles
at a fraction of this. */
const MAX_RATE_TRIM = 0.02;

class OcelotAudioProcessor extends AudioWorkletProcessor {
    constructor(options) {
        super();
        this.bufferSize = 32768 * 2;
        this.buffer = new Float32Array(this.bufferSize);
        this.readPos = 0;
        this.writePos = 0;
        this.count = 0;
        this.fadeGain = 1.0;

        const opts = (options && options.processorOptions) || {};
        const srcRate = opts.srcRate && opts.srcRate > 0 ? opts.srcRate : sampleRate;
        // The nominal ratio; 'step' is trimmed around it to track the backlog.
        this.baseStep = srcRate / sampleRate;
        this.step = this.baseStep;

        this.port.onmessage = (event) => {
            if (event.data === "query-level") {
                this.port.postMessage({type: "level", count: this.count, capacity: this.bufferSize});
                return;
            }
            if (event.data === "clear") {
                // Drop the backlog (e.g. after an unmute) and fade back in so the
                // jump in the sample stream does not click.
                this.readPos = 0;
                this.writePos = 0;
                this.count = 0;
                this.fadeGain = 0.0;
                return;
            }
            const samples = event.data;
            for (let i = 0; i < samples.length; i++) {
                if (this.count >= this.bufferSize) break;
                this.buffer[this.writePos] = samples[i] / 32768.0;
                this.writePos = (this.writePos + 1) % this.bufferSize;
                this.count++;
            }
            this.port.postMessage({type: "return-buffer", buffer: samples.buffer}, [samples.buffer]);
        };
    }

    /* Trim the resampling ratio so the backlog drifts back toward target.

    The emulator's frame pacing and the AudioContext run off independent
    clocks, so even a tiny rate mismatch eventually empties the buffer
    (underrun clicks) or pins it at capacity (dropped samples, growing
    latency). Nudging the resampler by a fraction of a percent absorbs that
    without an audible pitch change. */
    updateRateTrim() {
        const error = (this.count - TARGET_FILL_SAMPLES) / TARGET_FILL_SAMPLES;
        if (Math.abs(error) <= FILL_DEADBAND) {
            this.step = this.baseStep;
            return;
        }
        const excess = error > 0 ? error - FILL_DEADBAND : error + FILL_DEADBAND;
        // Normalise so the trim saturates at an empty or double-target backlog.
        const normalized = Math.max(-1, Math.min(1, excess / (1 - FILL_DEADBAND)));
        // Backlog above target -> consume slightly faster, and vice versa.
        this.step = this.baseStep * (1 + normalized * MAX_RATE_TRIM);
    }

    process(_inputs, outputs) {
        const output = outputs[0];
        const outL = output[0];
        const outR = output.length > 1 ? output[1] : null;
        if (!outL) return true;

        this.updateRateTrim();

        const frames = outL.length;
        const bufFrames = this.bufferSize / 2;

        // A single output frame consumes ceil(step) input frames at worst, and
        // interpolation peeks one frame past that. Requiring the backlog to
        // cover both keeps `count` from being clamped at zero while `readPos`
        // has already moved on, which would desync the ring buffer.
        const minCount = (Math.ceil(this.step) + 1) * 2;

        for (let i = 0; i < frames; i++) {
            if (this.count >= minCount) {
                const intFrame = Math.floor(this.readPos);
                const frac = this.readPos - intFrame;
                const idx0 = (intFrame * 2) % this.bufferSize;
                const idx1 = (((intFrame + 1) % bufFrames) * 2);
                const l = this.buffer[idx0] * (1 - frac) + this.buffer[idx1] * frac;
                const r = this.buffer[idx0 + 1] * (1 - frac) + this.buffer[idx1 + 1] * frac;

                if (this.fadeGain < 1.0) {
                    this.fadeGain = Math.min(1.0, this.fadeGain + 1.0 / 64.0);
                }

                outL[i] = l * this.fadeGain;
                if (outR) outR[i] = r * this.fadeGain;

                this.readPos += this.step;
                const newInt = Math.floor(this.readPos);
                const consumed = (newInt - intFrame) * 2;
                if (consumed > 0) this.count = Math.max(0, this.count - consumed);
                if (this.readPos >= bufFrames) this.readPos -= bufFrames;
            } else {
                if (this.fadeGain > 0.0) {
                    this.fadeGain = Math.max(0.0, this.fadeGain - 1.0 / 32.0);
                }
                outL[i] = 0;
                if (outR) outR[i] = 0;
            }
        }
        return true;
    }
}

registerProcessor("ocelot-audio", OcelotAudioProcessor);
