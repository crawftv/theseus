//function audio() (
//        notes = [{ time: "0:0", length:"2n", note: "G4", velocity: 0.9 },
//        { time: "0:2",length:"4n", note: "C4", velocity: 0.9 },
//        { time: "0:3",length:"4n", note: "C4", velocity: 0.9 },
//
//        { time: "1:0", length:"2n", note: "G4", velocity: 0.9 },
//        { time: "1:2",length:"4n", note: "C4", velocity: 0.9 },
//        { time: "1:3",length:"4n", note: "C4", velocity: 0.9 },
//
//        { time: "2:0", length:"8n", note: "E4", velocity: 0.9},
//        { time: "2:0.5", length:"4n", note: "G4", velocity: 0.9},
//        { time: "2:1.5", length:"8n", note: "E4", velocity: 0.9},
//        { time: "2:2",length:"4n", note: "C4", velocity: 0.9 },
//        { time: "2:3",length:"4n", note: "C4", velocity: 0.9 },
//
//        { time: "3:0", length:"2n", note: "G4", velocity: 0.9 },
//        { time: "3:2",length:"4n", note: "C4", velocity: 0.9 },
//        { time: "3:3",length:"4n", note: "C4", velocity: 0.9 },
//
//        { time: "4:0", length:"2n", note: "E4", velocity: 0.9 },
//        { time: "4:2",length:"4n", note: "G4", velocity: 0.9 },
//        { time: "4:3",length:"4n", note: "C4", velocity: 0.9 },
//
//        { time: "5:0",length:"2n", note: "C4", velocity: 0.9 },
//        { time: "5:2",length:"4n", note: "G4", velocity: 0.9 },
//    ])
//  {
//    const synth = new Tone.Synth().toDestination();
//    // use an array of objects as long as the object has a "time" attribute
//
//    const part = new Tone.Part(((time, value) => {
//        // the value is an object which contains both the note and the velocity
//        synth.triggerAttackRelease(value.note, value.length, time, value.velocity);
//    }), notes
//    ).start(0);
//    Tone.Transport.start();
//});
//document.getElementById("play-button").addEventListener("click", function() {
//  if (Tone.Transport.state !== 'started') {
//    x();
//  } else {
//    Tone.Transport.stop();
//  }
//  });

function notes(){
  // This approach to importing classes works in CJS contexts (i.e., a regular <script src="..."> tag).
    const { Stave, StaveNote, Beam, Formatter, Renderer, StaveTie, Annotation,Font, } = Vex;
    const annotation = (text) =>
        new Annotation(text).setFont(Font.SERIF, 12).setVerticalJustification(Annotation.VerticalJustify.BOTTOM);

    // In an ESM context (or when using TypeScript), you will need to use the "import" keyword.
    // import { Stave, StaveNote, Beam, Formatter, Renderer } from 'vexflow';

    // Create an SVG renderer and attach it to the DIV element with id="output".
    const div = document.getElementById("vexflow-output");
    const renderer = new Renderer(div, Renderer.Backends.SVG);
    console.log("draw")

    // Configure the rendering context.
    renderer.resize(1600, 130);
    const context = renderer.getContext();
    const notesMeasure1 = [
      new StaveNote({ keys: ['g/4'], duration: 'h' }).addModifier(annotation('ἄν'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('δρα'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('μοι'), 0),
    ];
    ties1 = []
    const notesMeasure2 = [
      new StaveNote({ keys: ['g/4'], duration: 'h' }).addModifier(annotation('ἔν'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('νε'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('πε'), 0),
    ];
    const ties2 = []
    const notesMeasure3 = [
      new StaveNote({ keys: ['e/4'], duration: '8' }).addModifier(annotation('Μοῦ'), 0),
      new StaveNote({ keys: ['g/4'], duration: 'q' }),
      new StaveNote({ keys: ['e/4'], duration: '8' }),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('σα'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('πο'), 0),
    ];
    const ties3 = [
        new StaveTie({first_note: notesMeasure3[0],last_note: notesMeasure3[2],first_indices: [0],last_indices: [0],}),
    ];
    const notesMeasure4 = [
      new StaveNote({ keys: ['g/4'], duration: 'h' }).addModifier(annotation('λύ'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('τρο'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('πον'), 0),
    ];
    const ties4 =[]
    const notesMeasure5 = [
      new StaveNote({ keys: ['e/4'], duration: 'h' }).addModifier(annotation('ὃς'), 0),
      new StaveNote({ keys: ['g/4'], duration: 'q' }).addModifier(annotation('μά'), 0),
      new StaveNote({ keys: ['c/4'], duration: 'q' }).addModifier(annotation('λα'), 0),
    ];
    const ties5 =[]
    const notesMeasure6 = [
      new StaveNote({ keys: ['c/4'], duration: 'h' }).addModifier(annotation('πολ'), 0),
      new StaveNote({ keys: ['g/4'], duration: 'q' }).addModifier(annotation('λὰ'), 0),
    ];
    const ties6 =[]
    // Measure 1
    const staveMeasure1 = new Stave(10, 0, 200);
    staveMeasure1.addClef('treble').addTimeSignature("4/4").setContext(context).draw();
    // Helper function to justify and draw a 4/4 voice
    Formatter.FormatAndDraw(context, staveMeasure1, notesMeasure1);
    ties1.forEach((t)=> {t.setContext(context).draw();});
    // measure 2
    const staveMeasure2 = new Stave(staveMeasure1.width + staveMeasure1.x, 0, 200);
    staveMeasure2.setContext(context).draw();
    Formatter.FormatAndDraw(context, staveMeasure2, notesMeasure2);
    ties2.forEach((t) => {t.setContext(context).draw();});
    // measure 3
    const staveMeasure3 = new Stave(staveMeasure2.width + staveMeasure2.x, 0, 200);
    staveMeasure3.setContext(context).draw();
    Formatter.FormatAndDraw(context, staveMeasure3, notesMeasure3);
    ties3.forEach((t) => {    t.setContext(context).draw();});
    // measure 4
    const staveMeasure4 = new Stave(staveMeasure3.width + staveMeasure3.x, 0, 200);
    staveMeasure4.setContext(context).draw();
    Formatter.FormatAndDraw(context, staveMeasure4, notesMeasure4);
    ties4.forEach((t) => {    t.setContext(context).draw();});
    // measure 5
    const staveMeasure5 = new Stave(staveMeasure4.width + staveMeasure4.x, 0, 200);
    staveMeasure5.setContext(context).draw();
    Formatter.FormatAndDraw(context, staveMeasure5, notesMeasure5);
    ties5.forEach((t) => {    t.setContext(context).draw();});
    // measure 6
    const staveMeasure6 = new Stave(staveMeasure5.width + staveMeasure5.x, 0, 200);
    staveMeasure6.setContext(context).draw();
    Formatter.FormatAndDraw(context, staveMeasure6, notesMeasure6);
    }

customElements.define('vexflow-output',
    class extends HTMLElement {
    constructor() { super(); }
    connectedCallback() { this.setTextContent(); }
    attributeChangedCallback() { this.setTextContent(); }
    static get observedAttributes() { return []; }

        // Our function to set the textContent based on attributes.
        setTextContent()
        {

            this.textContent = notes();
        }
    }
);
