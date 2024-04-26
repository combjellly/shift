let selectedOutput = null; // Variable to store the selected MIDI output

  // Function to populate the MIDI output dropdown menu
  function populateMidiOutputDropdown() {
    const dropdown = document.getElementById("midiOutputDropdown");

    navigator.requestMIDIAccess()
      .then(function (midiAccess) {
        midiAccess.outputs.forEach(function (output) {
          const option = document.createElement("option");
          option.text = output.name;
          option.value = output.id;
          dropdown.add(option);
        });

        if (dropdown.options.length > 0) {
          // Select the first available MIDI output by default
          selectedOutput = midiAccess.outputs.values().next().value;
        }

        dropdown.addEventListener("change", function () {
          const selectedOption = dropdown.options[dropdown.selectedIndex];
          selectedOutput = midiAccess.outputs.get(selectedOption.value);
        });
      })
      .catch(function (error) {
        console.error("Error accessing MIDI devices: " + error);
      });
  }

  // Function to play MIDI events from the provided message format
  function playMidiEvents(EngineRecord) {
    if (!selectedOutput) {
      console.error("No MIDI output selected. Call populateMidiOutputDropdown() first.");
      return;
    }

    // Example MIDI message
    const midiEventsString = String(EngineRecord);

    // Remove surrounding parentheses and split into individual messages
    const messages = midiEventsString.slice(1, -1).split(' : ');
    console.log
    // Play MIDI events for each parsed message
    messages.forEach((message) => {
      if (message !== 'Nil' && message !== 'sleepy time' && message !== 'i') { // where is the i coming from?
        playMidiEvent(message);
        console.log(message)
      }
    });
  }

  // Function to play a single MIDI event based on the provided message
  function playMidiEvent(message) {
    // Ensure that 'Nil' is replaced with null
    const fixedMessage = message.replace(/Nil/g, 'null').replace(/[() ]/g, '');
    // Parse the string into a JavaScript object

    let parsedMessage;
    try {
      // Convert unquoted keys and values to a valid JSON format
      parsedMessage = JSON.parse(fixedMessage.replace(/([\w-]+):/g, '"$1":'));

    } catch (error) {
      console.error("Error parsing MIDI message:", error);
      return;
    }

    //console.log(parsedMessage);

    const { channel, duration, note, velocity, whenPosix, s } = parsedMessage;


    // Calculate the delay in milliseconds until the specified POSIX time
    const delay = (whenPosix * 1000 - Date.now())+375 ;

    // Schedule the MIDI note on event after the specified delay
    setTimeout(() => {
      // Send the MIDI note-on message
      selectedOutput.send([0x90 + channel, note, velocity]);

      // Schedule the MIDI note-off event after the specified duration
      setTimeout(() => {
        selectedOutput.send([0x80 + channel, note, 0x00]); // MIDI note-off message
      }, duration);
    }, delay);
    
  }


  // Populate the MIDI output dropdown menu on page load
