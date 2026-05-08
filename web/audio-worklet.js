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
        this.step = srcRate / sampleRate;

        this.port.onmessage = (event) => {
            if (event.data === "query-level") {
                this.port.postMessage({type: "level", count: this.count, capacity: this.bufferSize});
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

    process(_inputs, outputs) {
        const output = outputs[0];
        const outL = output[0];
        const outR = output.length > 1 ? output[1] : null;
        if (!outL) return true;

        const frames = outL.length;
        const bufFrames = this.bufferSize / 2;

        for (let i = 0; i < frames; i++) {
            if (this.count >= 4) {
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
