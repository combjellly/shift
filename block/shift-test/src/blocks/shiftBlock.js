import * as Blockly from 'blockly';

export const blocks = Blockly.common.createBlockDefinitionsFromJsonArray(
[{
  "type": "every",
  "message0": "every %1 %2",
  "args0": [
    {
      "type": "input_value",
      "name": "loopNumber"
    },
    {
      "type": "input_statement",
      "name": "listOfActions"
    }
  ],
  "inputsInline": false,
  "colour": 230,
  "tooltip": "",
  "helpUrl": ""
},
{
  "type": "play",
  "message0": "play %1 %2",
  "args0": [
    {
      "type": "input_value",
      "name": "PLAY"
    },
    {
      "type": "input_statement",
      "name": "playAttributes"
    }
  ],
  "previousStatement": null,
  "nextStatement": null,
  "colour": 230,
  "tooltip": "",
  "helpUrl": ""
},
{
  "type": "note",
  "message0": "note %1",
  "args0": [
    {
      "type": "input_value",
      "name": "NAME"
    }
  ],
  "previousStatement": null,
  "nextStatement": null,
  "colour": 230,
  "tooltip": "",
  "helpUrl": ""
},
{
  "type": "volume",
  "message0": "volume %1",
  "args0": [
    {
      "type": "input_value",
      "name": "NAME"
    }
  ],
  "previousStatement": null,
  "nextStatement": null,
  "colour": 230,
  "tooltip": "",
  "helpUrl": ""
},
{
  "type": "pan",
  "message0": "pan %1",
  "args0": [
    {
      "type": "input_value",
      "name": "NAME"
    }
  ],
  "previousStatement": null,
  "nextStatement": null,
  "colour": 230,
  "tooltip": "",
  "helpUrl": ""
},
{
  "type": "cut",
  "message0": "cut %1",
  "args0": [
    {
      "type": "input_value",
      "name": "NAME"
    }
  ],
  "previousStatement": null,
  "nextStatement": null,
  "colour": 230,
  "tooltip": "",
  "helpUrl": ""
},
{
  "type": "num",
  "message0": "%1",
  "args0": [
    {
      "type": "field_input",
      "name": "NUM",
      "text": "default"
    }
  ],
  "inputsInline": false,
  "output": null,
  "colour": 70,
  "tooltip": "",
  "helpUrl": ""
}]

);