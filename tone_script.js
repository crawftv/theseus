//StartAudioContext(Tone.context)
function x(notes = [{ time: "0:0", length:"2n", note: "G4", velocity: 0.9 },
        { time: "0:2",length:"4n", note: "C4", velocity: 0.9 },
        { time: "0:3",length:"4n", note: "C4", velocity: 0.9 },

        { time: "1:0", length:"2n", note: "G4", velocity: 0.9 },
        { time: "1:2",length:"4n", note: "C4", velocity: 0.9 },
        { time: "1:3",length:"4n", note: "C4", velocity: 0.9 },

        { time: "2:0", length:"8n", note: "E4", velocity: 0.9},
        { time: "2:0.5", length:"4n", note: "G4", velocity: 0.9},
        { time: "2:1.5", length:"8n", note: "E4", velocity: 0.9},
        { time: "2:2",length:"4n", note: "C4", velocity: 0.9 },
        { time: "2:3",length:"4n", note: "C4", velocity: 0.9 },

        { time: "3:0", length:"2n", note: "G4", velocity: 0.9 },
        { time: "3:2",length:"4n", note: "C4", velocity: 0.9 },
        { time: "3:3",length:"4n", note: "C4", velocity: 0.9 },

        { time: "4:0", length:"2n", note: "E4", velocity: 0.9 },
        { time: "4:2",length:"4n", note: "G4", velocity: 0.9 },
        { time: "4:3",length:"4n", note: "C4", velocity: 0.9 },

        { time: "5:0",length:"2n", note: "C4", velocity: 0.9 },
        { time: "5:2",length:"4n", note: "G4", velocity: 0.9 },
    ])  {
    console.log("started playing");
    const synth = new Tone.Synth().toDestination();
    // use an array of objects as long as the object has a "time" attribute

    const part = new Tone.Part(((time, value) => {
        // the value is an object which contains both the note and the velocity
        synth.triggerAttackRelease(value.note, value.length, time, value.velocity);
    }), notes
    ).start(0);
    Tone.Transport.start();
}
